#==============================================================================
# 
# Data Analysis - Health Seeking Behaviours
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

# Create new data frame for households that reported illness or injury requiring medical care
data_health <- data_edit %>%
  filter(health_behaviours_illness_yn == 'y') %>%
  mutate(
    health_behaviours_illness_under5 = as.numeric(health_behaviours_illness_under5),
    health_behaviours_illness_over5 = as.numeric(health_behaviours_illness_over5))


# Numer of people who experienced illness compared by community
illness_table <- data_health %>%
  group_by(interview_info_community) %>%
  summarize(
    sum_health_behaviours_under5 = sum(health_behaviours_illness_under5, na.rm = TRUE),
    sum_health_behaviours_over5 = sum(health_behaviours_illness_over5, na.rm = TRUE),
    .groups = 'drop')

print(illness_table)

#==============================================================================

# Number of ill/injured people that sought medical care grouped by community
medcare_y <- sum(data_health$health_behaviours_med_care == "y", na.rm = TRUE)
print(medcare_y)

# Number of ill/injured people that sought medical care grouped by community
medcare_table <- data_health %>%
  group_by(interview_info_community) %>%
  summarize(count_y = sum(health_behaviours_med_care == "y", na.rm = TRUE), .groups = 'drop')
print(medcare_table)


# Create new data frame of ill/injured people that did not receive medical care 
data_no_med <- data_health %>%
  filter(health_behaviours_med_care == 'n')


# Create table of reasons contributing to ill/injured people not receiving medical care (weighted)
no_med_table <- data_no_med %>%
  group_by(health_behaviours_med_care_no) %>%
  summarize(count = n(), .groups = 'drop') 



# Create a plot to show the reason for not receiving medical care *MANUALLY DONE - NOT AUTOMATED YET*
data_no_med_plot <- data.frame(
  category = c("Financial", "Checkpoints", "Distance", "Access", "Fear of violence", "Time"),
  percentage = c(36, 11, 15, 22, 11, 5)
)

ggplot(data_no_med_plot, aes(x = reorder(category, -percentage), y = percentage)) +
  geom_bar(stat = "identity", fill = "darkred") +
  labs(x = "Barrier to healthcare access", y = "Percentage (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# 
anova_barriers <- aov(health_behaviours_, data = data_health)




# Create a contingency table
contingency_table <- table(data_health$interview_info_community, data_health$health_behaviours_med_care)

# Print the contingency table
print(contingency_table)

# Perform the Chi-squared test
chi_squared_result <- chisq.test(contingency_table)

# Print the result
print(chi_squared_result)


data_no_med <- data_no_med %>%
  mutate(total_health_behaviours = health_behaviours_illness_under5 + health_behaviours_illness_over5)



#==============================================================================
# End of code
#==============================================================================