#==============================================================================
# 
# Data Analysis - Pregnancy
#
#==============================================================================

# Install and load required packages
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")}
library(pacman)


# Install and load required packages to run script
pacman::p_load(dplyr          # Manipulate data
  )

#==============================================================================

# Clean and reformat data frame for analysis
data_preg <- data_preg %>%
  rename_with(~ gsub("/", "_", .)) %>%
  mutate(pregnancy_info_preg_anc_num = as.numeric(pregnancy_info_preg_anc_num))


# Create count of pregnancies by community
preg_count <- data_preg %>%
  group_by(interview_info_community) %>%
  summarise(completed_pregnancies = n()) %>%
  ungroup()


# Number of patients who attended ANC care
anc_count <- table(data_preg$pregnancy_info_preg_anc)
print(anc_count)


# Outcome of the pregnancy
preg_outcome <- table(data_preg$pregnancy_info_preg_outcome)
print(preg_outcome)


# Average number of ANC visits
mean_anc <- mean(data_preg$pregnancy_info_preg_anc_num, na.rm = TRUE)
print(mean_anc)


# Minimum number of ANC visits per pregnancy
min_anc <- min(data_preg$pregnancy_info_preg_anc_num, na.rm = TRUE)
print(min_anc)


# Number of patients who will attend PNC care
pnc_count <- table(data_preg$pregnancy_info_preg_pnc)
print(pnc_count)


# Maximum number of ANC visits per pregnancy
max_anc <- max(data_preg$pregnancy_info_preg_anc_num, na.rm = TRUE)
print(max_anc)

#==============================================================================
# End of Code
#==============================================================================