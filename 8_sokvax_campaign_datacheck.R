
#==============================================================================

# Vaccination Campaign Round 2 Data
# Data Quality Check
# Sokvax Project, Nigeria Mission, MSF

#==============================================================================

# This script is used to identify any discrepancies or errors in the data. New data quality checks can be entered here.

#==============================================================================
# Review number of sites per day
#==============================================================================

# Define file path for expected number of sites per day excel file
filepath_datacheck_sites <- here::here("04 Analysis", "01 Data Checks", "datacheck_sites_per_day.xlsx")


# Import datacheck spreadsheet
datacheck_sites <- read_excel(filepath_datacheck_sites)

#==============================================================================

# Define actual number of sites per day
numsite_date <- data_edit %>%
  group_by(date) %>%
  summarise(actual = n())


# Left join the actual number of sites with the expected number of sites
datacheck_sites <- datacheck_sites %>%
  left_join(numsite_date, by = "date")


# Flag any discrepant values
datacheck_sites <- datacheck_sites %>%
  mutate(discrepancy = if_else(actual != expected, "DISCREPANCY", "N"))

  
print(datacheck_sites, n = 1000)

#==============================================================================

# Find duplicates in site_code column
duplicates <- tab_vax_site %>%
  filter(duplicated(site_code) | duplicated(site_code, fromLast = TRUE))

# View the duplicates
duplicates

#==============================================================================
# End of code
#==============================================================================
