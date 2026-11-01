#==============================================================================
# 
# Data Analysis - Demographics
#
#==============================================================================

# Install and load required packages
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")}
library(pacman)


# Install and load required packages to run script
pacman::p_load(dplyr,             # Manipulate data
               lubridate,         # Work with dates
               ggplot2            # Create plots

  )

#==============================================================================

# Convert numbers to numeric values 
data_edit <- data_edit %>%
  mutate(across(c(hh_info_hh_size_male, 
                  hh_info_hh_size_female, 
                  hh_info_hh_size_child_male,
                  hh_info_hh_size_child_female,
                  hh_info_hh_size_infant), 
    as.numeric))


# Calculate household total column
data_edit <- data_edit %>%
  rowwise() %>%
  mutate(hh_total = sum(c_across(c(hh_info_hh_size_male,
                                   hh_info_hh_size_female,
                                   hh_info_hh_size_child_male,
                                   hh_info_hh_size_child_female,
                                   hh_info_hh_size_infant)),
    na.rm = TRUE)) %>%
  ungroup()


# Create a table summarizing population of each community
pop_table <- data_edit %>%
  group_by(interview_info_community) %>%
  summarise(
    total_population = sum(hh_total, na.rm = TRUE),
    household_count = n(),
    .groups = 'drop')


# Calculate age group table
age_table <- data_edit %>%
  summarize(
    total_adult_m = sum(hh_info_hh_size_male, na.rm = TRUE),
    total_adult_f = sum(hh_info_hh_size_female, na.rm = TRUE),
    total_adult = total_adult_m + total_adult_f,
    total_child_m = sum(hh_info_hh_size_child_male, na.rm = TRUE),
    total_child_f = sum(hh_info_hh_size_child_female, na.rm = TRUE),
    total_child = total_child_m + total_child_f,
    total_infant = sum(hh_info_hh_size_infant, na.rm = TRUE),
    total_u5 = total_child_m + total_child_f + total_infant
  )


# Summary statistics for age and sex demographics
hh_avg <- mean(data_edit$hh_total[data_edit$hh_total != 0], na.rm = TRUE)
hh_min <- min(data_edit$hh_total[data_edit$hh_total != 0], na.rm = TRUE)
hh_max <- max(data_edit$hh_total[data_edit$hh_total != 0], na.rm = TRUE)


#==============================================================================
# End of code
#==============================================================================
