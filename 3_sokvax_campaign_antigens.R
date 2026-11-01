#==============================================================================

# Vaccination Campaign Round 2 Data - DATA ANALYSIS
# Sokvax Project, Nigeria Mission, MSF

#==============================================================================

# This code is to analyze all the vaccines provided during the MSF vaccination campaign by age group and dose.

#==============================================================================
# Antigens and age groups
#==============================================================================


# Create table with summary of antigens and age group
tab_antigen_edit <- data_edit %>%
  select(
    "penta1_0_11m", "penta1_12_23m", "penta1_24_59m", "penta2_0_11m", "penta2_12_23m", "penta2_24_59m", "penta3_0_11m", "penta3_12_23m",  "penta3_24_59m", "measles1_0_11m", "measles1_12_23m", "measles1_24_59m",  "measles2_12_23m", "measles2_24_59m", "opv0_0_22wks", "opv1_0_11m",  "opv1_12_23m", "opv1_24_59m", "opv2_0_11m", "opv2_12_23m", "opv2_24_59m",  "opv3_0_11m", "opv3_12_23m", "opv3_24_59m", "ipv1_0_11m", "ipv1_12_23m",  "ipv1_24_59m", "ipv2_0_11m", "ipv2_12_23m", "ipv2_24_59m", "pcv1_0_11m", "pcv1_12_23m", "pcv1_24_59m", "pcv2_0_11m", "pcv2_12_23m", "pcv2_24_59m", "pcv3_0_11m", "pcv3_12_23m", "pcv3_24_59m", "rota1_0_11m", "rota1_12_23m", "rota1_24_59m", "rota2_0_11m", "rota2_12_23m", "rota2_24_59m", "rota3_0_11m", "rota3_12_23m", "rota3_24_59m", "yellowfever_0_11m", "yellowfever_12_23m", "yellowfever_24_59m", "mena_0_11m", "mena_12_23m", "mena_24_59m", "bcg_0_11m", "bcg_12_23m", "hepb0_0_24hrs", "hepb0_24hrs_2wks", "vita_0_11m", "vita_12_23m"
  ) %>%
  pivot_longer(cols = everything(), 
               names_to = c("antigen", "age_group"),
               names_pattern = "(\\w+)_(\\d+_\\d+\\w*)", 
               values_to = "count") %>%
  mutate(age_group = recode(age_group, 
                            "0_24hrs" = "0_11m", 
                            "24hrs_2wks" = "0_11m", 
                            "0_22wks" = "0_11m")) %>%
  group_by(antigen, age_group) %>%
  summarize(total_count = sum(count, na.rm = TRUE), .groups = "drop") %>%
  mutate(antigen = factor(antigen, levels = c(
    "penta1", "penta2", "penta3", "measles1", "measles2", "opv0", "opv1", "opv2", "opv3", "ipv1", "ipv2", "pcv1", "pcv2", "pcv3", "rota1", "rota2", "rota3", "yellowfever", "mena", "bcg", "hepb0", "vita"))) %>%
  mutate(age_group = factor(age_group, levels = c("0_11m", "12_23m", "24_59m")))


# Pivot wider with age groups as columns
tab_antigen_edit <- tab_antigen_edit %>%
  pivot_wider(names_from = age_group, values_from = total_count, values_fill = list(total_count = 0)) %>%
  arrange(antigen) %>%
  select(-"NA")


# Create overall column
tab_antigen_edit <- tab_antigen_edit %>%
  mutate(total = rowSums(select(., starts_with("0_11m"), starts_with("12_23m"), starts_with("24_59m")), na.rm = TRUE))


# Create overall row
tab_antigen_total <- tab_antigen_edit %>%
  bind_rows(
    tab_antigen_edit %>%
      summarise(
        antigen = "total",
        total = sum(total, na.rm = TRUE),
        across(starts_with("0_11m"):starts_with("24_59m"), sum, na.rm = TRUE)) %>%
      mutate(across(starts_with("0_11m"):starts_with("24_59m"), ~ ifelse(is.na(.), 0, .))))


# Calculate sum of same antigens
tab_antigen_total <- tab_antigen_total %>%
  mutate(
    total_sum = case_when(
      antigen %in% c("penta1", "penta2", "penta3") ~ sum(total[antigen %in% c("penta1", "penta2", "penta3")], na.rm = TRUE),
      antigen %in% c("measles1", "measles2") ~ sum(total[antigen %in% c("measles1", "measles2")], na.rm = TRUE),
      antigen %in% c("opv0", "opv1", "opv2", "opv3") ~ sum(total[antigen %in% c("opv0", "opv1", "opv2", "opv3")], na.rm = TRUE),
      antigen %in% c("ipv1", "ipv2") ~ sum(total[antigen %in% c("ipv1", "ipv2")], na.rm = TRUE),
      antigen %in% c("pcv1", "pcv2", "pcv3") ~ sum(total[antigen %in% c("pcv1", "pcv2", "pcv3")], na.rm = TRUE),
      antigen %in% c("rota1", "rota2", "rota3") ~ sum(total[antigen %in% c("rota1", "rota2", "rota3")], na.rm = TRUE),
      TRUE ~ NA_real_))


# Convert to flextable
tab_antigen <- flextable(tab_antigen_total)


# Merge cells for antigen totals
tab_antigen <- tab_antigen %>%
  merge_at(i = 1:3, j = 6) %>%
  merge_at(i = 4:5, j = 6) %>%
  merge_at(i = 6:9, j = 6) %>%
  merge_at(i = 10:11, j = 6) %>%
  merge_at(i = 12:14, j = 6) %>%
  merge_at(i= 15:17, j = 6)


# Print the table
tab_antigen


#==============================================================================
# End of code
#==============================================================================

