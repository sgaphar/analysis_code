#==============================================================================

# Vaccination Campaign Round 2 Data
# Sokvax Project, Nigeria Mission, MSF

#==============================================================================

# This code calculcate the number of vials used based on reports by the NTS. The code is currently incomplete and inaccurate. There is limited opportunities to ensure the vial counts are correct. 

#==============================================================================
# Consumption Monitoring
#==============================================================================

tab_vials_edit <- data_edit %>%
  select(
# Pentavalent vials  
"penta_sod_open", "penta_sod_closed", "penta_eod_empty", "penta_eod_open", "penta_eod_closed",
# Measles vials         
"measles_sod_open", "measles_sod_closed", "measles_eod_empty", "measles_eod_open", "measles_eod_closed", 
# Measles diluent vials
"measles_diluent_sod_open", "measles_diluent_sod_closed", "measles_diluent_eod_empty", "measles_diluent_eod_open", "measles_diluent_eod_closed", 
# OPV vials
"opv_sod_open", "opv_sod_closed", "opv_eod_empty", "opv_eod_open", "opv_eod_closed", 
# IPV vials
"ipv_sod_open", "ipv_sod_closed", "ipv_eod_empty", "ipv_eod_open", "ipv_eod_closed", 
# PCV vials
"pcv_sod_open", "pcv_sod_closed", "pcv_eod_empty", "pcv_eod_open", "pcv_eod_closed", 
# Rota vials
"rota_sod_open", "rota_sod_closed", "rota_eod_empty", "rota_eod_open", "rota_eod_closed", 
# Yellow fever vials
"yf_sod_open", "yf_sod_closed", "yf_eod_empty", "yf_eod_open", "yf_eod_closed", 
# Yellow fever diluent vials
"yf_diluent_sod_open", "yf_diluent_sod_closed", "yf_diluent_eod_empty", "yf_diluent_eod_open", "yf_diluent_eod_closed", 
# Meningitis A vials
"mena_sod_open", "mena_sod_closed", "mena_eod_empty", "mena_eod_open", "mena_eod_closed", 
# Meningitis A diluent vials
"mena_diluent_sod_open", "mena_diluent_sod_closed", "mena_diluent_eod_empty", "mena_diluent_eod_open", "mena_diluent_eod_closed", 
# BCG vials
"bcg_sod_open", "bcg_sod_closed", "bcg_eod_empty", "bcg_eod_open", "bcg_eod_closed", 
# BCG diluent vials
"bcg_diluent_sod_open", "bcg_diluent_sod_closed", "bcg_diluent_eod_empty", "bcg_diluent_eod_open", "bcg_diluent_eod_closed", 
# Hep B vials
"hepb_sod_open", "hepb_sod_closed", "hepb_eod_empty", "hepb_eod_open", "hepb_eod_closed", 
# Vitamin A vials
"vita_sod_open", "vita_sod_closed", "vita_eod_empty", "vita_eod_open", "vita_eod_closed")

  
# Pivot longer by time and vial characteristic
tab_vials_edit_long <- tab_vials_edit %>%
  pivot_longer(cols = everything(), 
               names_to = "Vaccine_Status", 
               values_to = "Count") %>%
  mutate(vaccine = sub("_(sod|eod|open|closed|empty).*", "", Vaccine_Status),
time = sub(".*_(sod|eod).*", "\\1", Vaccine_Status),  
vial_char = sub(".*_(open|closed|empty)", "\\1", Vaccine_Status), status  = paste(time, vial_char, sep = "_")) %>%
  select(vaccine, status, Count)


# Put the status in order
status_order <- c("sod_open", "sod_closed", "eod_empty", "eod_open", "eod_closed")


# Put the vaccine names in order
vaccine_order <- c("penta", "measles", "measles_diluent", "opv", "ipv", "pcv", "rota", "yf", "yf_diluent", "mena", "mena_diluent", "bcg", "bcg_diluent", "hepb", "vita")


# Pivot wider by antigen
tab_vials_edit_wide <- tab_vials_edit_long %>%
  group_by(status, vaccine) %>%
  summarise(Count = sum(Count, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = vaccine, values_from = Count, values_fill = list(Count = 0)) %>%
  mutate(status = factor(status, levels = status_order)) %>%
  arrange(status)


# Apply factors to vaccine orders to be consistent with summary sheet
tab_vials_edit_wide <- tab_vials_edit_wide %>%
  select(c("status", vaccine_order)) %>%
  rename(
    Penta = penta, 
    Measles = measles,
    `Measles (D)` = measles_diluent,
    OPV = opv,
    IPV = ipv,
    PCV = pcv, 
    ROTA = rota,
    YF = yf,
    `YF (D)` = yf_diluent, 
    `Men A` = mena,
    `Men A (D)` = mena_diluent,
    BCG = bcg,
    `BCG (D)` = bcg_diluent,
    `Hep B` = hepb,
    `Vit A` = vita)


# Split the status column into two
tab_vials_edit_split <- tab_vials_edit_wide %>%
  separate(status, into = c("Time", "Status"), sep = "_") %>%
  mutate(
    Time = recode(Time, 
                  sod = "Start of day", 
                  eod = "End of day")) %>%
  mutate(
    Status = recode(Status,
                  empty = "Empty",
                  open = "Open",
                  closed = "Closed")) 
  
  
# Convert to flextable
tab_vials <- flextable(tab_vials_edit_split)


# Merge cells for SOD
tab_vials <- tab_vials %>%
  merge_at(i = 1:2, j = 1)


# Merge cells for EOD
tab_vials <- tab_vials %>%
  merge_at(i = 3:5, j = 1)


# Adjust table format
tab_vials <- tab_vials %>%
  fontsize(i = 1, size = 9, part = "header") %>%
  fontsize(size = 9, part = "body") %>%
  bold(i = 1, bold = TRUE, part = "header") %>%
  theme_vanilla() %>%
  align(align = "center", part = "body") %>%
  align(align = "center", part = "header") %>%
  align(j = 1, align = "left", part = "body") %>%
  add_footer_lines("(D) = Diluent") %>%
  fontsize(size = 9, part = "footer")


# Print the table
tab_vials

#==============================================================================
# End of code
#==============================================================================

