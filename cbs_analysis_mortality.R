#==============================================================================
# 
# 4. Data analysis - Mortality
#
#==============================================================================

# Install and load required packages
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")}
library(pacman)


# Install and load required packages to run script
pacman::p_load(dplyr             # Manipulate data
  )

#==============================================================================

# Create data frame for observations where mortality was identified
data_mort <- data_edit %>%
  filter(mortality_mortality_hh == "y")

#==============================================================================
#
# End of code
#
#==============================================================================
