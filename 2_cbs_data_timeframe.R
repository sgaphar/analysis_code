#==============================================================================
# 
# 2. Identify timeframes for CBS
#
#==============================================================================

# Install and load required packages
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")}
library(pacman)


# Install and load required packages to run script
pacman::p_load(dplyr,             # Manipulate data
               lubridate          # Work with dates

  )

#==============================================================================

# Create a new column for date
data_edit <- data_edit %>%
  mutate(
    interview_info_doi = ymd(interview_info_doi),
    yearmonth = format(interview_info_doi, "%Y-%m"))

#==============================================================================

# NEED TO ADD CODE TO FILTER THE DATA FOR THE CORRECT MONTH IN FUTURE

#==============================================================================
# End of code
#==============================================================================
