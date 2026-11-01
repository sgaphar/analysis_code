#==============================================================================
# 
# Data Analysis - Malnutrition
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

# Rename for reporting
data_muac <- data_muac %>%
  mutate(
    muac_score = recode(muac_score, 
                        "green" = "Green", 
                        "yellow" = "Yellow", 
                        "red" = "Red",
                        .default = NA_character_),
    sex = recode(sex, 
                 "male" = "Male", 
                 "female" = "Female"))


# Create summary of MUAC data 
muac_summary <- data_muac %>%
  group_by(sex, muac_score) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(sex) %>%
  mutate(percentage = (count / sum(count)) * 100)


# Create plot 
ggplot(muac_summary, aes(x = sex, y = percentage, fill = muac_score)) +
  geom_bar(stat = "identity") +
  labs(x = "Sex",
       y = "Proportion (%)",
       fill = "MUAC Score") +
  scale_fill_manual(values = c("Green" = "#26DA4D", 
                                "Yellow" = "yellow", 
                                "Red" = "red", 
                                "NA" = "gray")) +  # Specify colors, keep "NA" as a string
  theme_minimal()



#==============================================================================
# End of code
#==============================================================================
