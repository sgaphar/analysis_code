#==============================================================================

# Vaccination Campaign Round 2 Data
# Sokvax Project, Nigeria Mission, MSF

#==============================================================================

# This script is to identify the number of children who attended a previous round of the vaccination campaign compared to the number of children who are new to the vaccination campaign and have not previously been vaccinated by MSF

#==============================================================================
# New versus returning children
#==============================================================================

# Create count of new vs returning patients
tab_sex_edit <- data_edit %>%
  select(all_of(col_sum)) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("round", "sex", "agegroup"),
    names_pattern = "(r[1-3])_(.)_(\\d+_\\d+m)", 
    values_to = "count")

#==============================================================================
# Create new data frames for each round that the child attended
#==============================================================================

# Children attending R2 that also attended R1
tab_sex_edit_r1 <- tab_sex_edit %>% 
  filter(round == "r1")

# Children attending R2 that did not attend R1
tab_sex_edit_r2 <- tab_sex_edit %>%
  filter(round == "r2")

# This will be used for children attended R3 that did not attend R1 or R2
tab_sex_edit_r3 <- tab_sex_edit %>%
 filter(round == "r3")

#==============================================================================
# Returning from Round 1
#==============================================================================

# Pivot wider for age group and sex
tab_sex_pivot_r1 <- tab_sex_edit_r1 %>%
  group_by(sex, agegroup) %>%
  summarise(count = sum(count, na.rm = TRUE), .groups = "drop") %>% 
  pivot_wider(names_from = agegroup, values_from = count, values_fill = list(count = 0))


# Add overall column
tab_sex_totcol_r1 <- tab_sex_pivot_r1 %>%
  mutate(total = rowSums(select(., starts_with("0_11m"), starts_with("12_23m"), starts_with("24_59m")), na.rm = TRUE))


# Add overall rows
tab_sex_totrow_r1 <- tab_sex_totcol_r1 %>%
  bind_rows(
    tab_sex_totcol_r1 %>%
      summarise(
        sex = "total",
        total = sum(total, na.rm = TRUE),
        across(starts_with("0_11m"):starts_with("24_59m"), sum, na.rm = TRUE)) %>%
      mutate(across(starts_with("0_11m"):starts_with("24_59m"), ~ ifelse(is.na(.), 0, .))))
  

# Convert to flextable
tab_agesex_r1 <- flextable(tab_sex_totrow_r1) %>%
  add_header_lines(values = "Returning from Round 1")


# Print table
tab_agesex_r1

#==============================================================================
# New to Round 2
#==============================================================================

# Pivot wider for age group and sex
tab_sex_pivot_r2 <- tab_sex_edit_r2 %>%
  group_by(sex, agegroup) %>%
  summarise(count = sum(count, na.rm = TRUE), .groups = "drop") %>% 
  pivot_wider(names_from = agegroup, values_from = count, values_fill = list(count = 0))


# Add overall column
tab_sex_totcol_r2 <- tab_sex_pivot_r2 %>%
  mutate(total = rowSums(select(., starts_with("0_11m"), starts_with("12_23m"), starts_with("24_59m")), na.rm = TRUE))


# Add overall rows
tab_sex_totrow_r2 <- tab_sex_totcol_r2 %>%
  bind_rows(
    tab_sex_totcol_r2 %>%
      summarise(
        sex = "total",
        total = sum(total, na.rm = TRUE),
        across(starts_with("0_11m"):starts_with("24_59m"), sum, na.rm = TRUE)) %>%
      mutate(across(starts_with("0_11m"):starts_with("24_59m"), ~ ifelse(is.na(.), 0, .))))


# Convert to flextable
tab_agesex_r2 <- flextable(tab_sex_totrow_r2) %>%
  add_header_lines(values = "New to Round 2")


# Print table
tab_agesex_r2

#==============================================================================

# Clean environment
rm(tab_sex_edit_r1,
   tab_sex_edit_r2,
   tab_sex_edit_r3,
   tab_sex_pivot_r1,
   tab_sex_pivot_r2,
   tab_sex_totcol_r1,
   tab_sex_totcol_r2,
   tab_sex_totrow_r1,
   tab_sex_totrow_r2)

#==============================================================================
# End of code
#==============================================================================


