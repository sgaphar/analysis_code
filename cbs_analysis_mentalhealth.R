#==============================================================================
# 
# Data Analysis - Mental Health
#
#==============================================================================

# Install and load required packages
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")}
library(pacman)


# Install and load required packages to run script
pacman::p_load(dplyr,             # Manipulate data
               ggplot2
  )

#==============================================================================

# Rename headers for easier analysis
data_mh <- data_mh %>%
  rename_with(~ gsub("/", "_", .))


# Create score for PHQ4 rating
data_mh <- data_mh %>%
  mutate(
    nervous_score = case_when(
      mental_health_mh_nervous == "none" ~ 0,
      mental_health_mh_nervous == "several_days" ~ 1,
      mental_health_mh_nervous == "half_days" ~ 2,
      mental_health_mh_nervous == "all_days" ~ 3,
      mental_health_mh_nervous == "decline" ~ NA_real_,
      TRUE ~ NA_real_  # For any other case
    ),
    worry_score = case_when(
      mental_health_mh_worry == "none" ~ 0,
      mental_health_mh_worry == "several_days" ~ 1,
      mental_health_mh_worry == "half_days" ~ 2,
      mental_health_mh_worry == "all_days" ~ 3,
      mental_health_mh_worry == "decline" ~ NA_real_,
      TRUE ~ NA_real_
    ),
    depressed_score = case_when(
      mental_health_mh_depressed == "none" ~ 0,
      mental_health_mh_depressed == "several_days" ~ 1,
      mental_health_mh_depressed == "half_days" ~ 2,
      mental_health_mh_depressed == "all_days" ~ 3,
      mental_health_mh_depressed == "decline" ~ NA_real_,
      TRUE ~ NA_real_
    ),
    interest_score = case_when(
      mental_health_mh_interest == "none" ~ 0,
      mental_health_mh_interest == "several_days" ~ 1,
      mental_health_mh_interest == "half_days" ~ 2,
      mental_health_mh_interest == "all_days" ~ 3,
      mental_health_mh_interest == "decline" ~ NA_real_,
      TRUE ~ NA_real_
    )
  )


# Create total PHQ4 score ratings
data_mh <- data_mh %>%
  mutate(
    phq4_score = rowSums(select(., nervous_score, worry_score, depressed_score, interest_score), na.rm = TRUE),
    sx_dep_score = rowSums(select(.,depressed_score, interest_score), na.rm = TRUE),
    sx_anx_score = rowSums(select(.,nervous_score, worry_score), na.rm = TRUE))


# Idenify rate based on PHQ4 score
data_mh <- data_mh %>%
  mutate(
    phq4_rate = case_when(
      phq4_score >= 0 & phq4_score <= 2 ~ "Normal",
      phq4_score >= 3 & phq4_score <= 5 ~ "Mild",
      phq4_score >= 6 & phq4_score <= 8 ~ "Moderate",
      phq4_score >= 9 & phq4_score <= 12 ~ "Severe",
      TRUE ~ NA_character_),
    phq4_rate = factor(phq4_rate, levels = c("Normal", "Mild", "Moderate", "Severe")) # Set order here
  )

# Remove NA values
data_mh_edit <- data_mh %>%
  filter(!is.na(phq4_rate))

# Plot PHQ4 rate
ggplot(data_mh_edit, aes(x = phq4_rate, fill = mental_health_mh_sex)) +
  geom_bar(position = "stack") +
  labs(x = "PHQ-4 Score",
       y = "Count (n)") +
  scale_fill_manual(values = c("#ED1C24", "darkred"), 
                    name = "Sex") +
  theme_minimal()


# Create contingency table for chi-square test
mh_m1 <- sum(data_mh$mental_health_mh_sex == "male" & data_mh$phq4_binary == "1")
mh_m0 <- sum(data_mh$mental_health_mh_sex == "male" & data_mh$phq4_binary == "0")
mh_f1 <- sum(data_mh$mental_health_mh_sex == "female" & data_mh$phq4_binary == "1")
mh_f0 <- sum(data_mh$mental_health_mh_sex == "female" & data_mh$phq4_binary == "0")


contingency_table <- matrix(c(mh_m1, mh_m0, mh_f1, mh_f0), 
                            nrow = 2, 
                            dimnames = list(c("Male", "Female"), c("Normal", "Not Normal")))

chi_square_result <- chisq.test(contingency_table)

# Print the result
print(chi_square_result)


# Calculate proportion with anxiety symptoms score equal or greater than 3
count_anxiety <- data_mh %>%
  filter(sx_anx_score >= 3)


# Calculate proprtion with depression symptoms equal or greater than 3
count_depression <- data_mh %>%
  filter(sx_dep_score >= 3)


# Convert numeric values to correct class
data_mh <- data_mh %>%
  mutate(mental_health_mh_age = as.numeric(mental_health_mh_age))


# Create bins for age groups
data_mh <- data_mh %>%
  mutate(age_group = cut(mental_health_mh_age, 
                         breaks = c(-Inf, 25, 34, 44, 54, 64, 74, Inf), 
                         labels = c("<25", "25-34", "35-44", "45-54", "55-64", "65-74", ">74"),
                         right = FALSE))


# Summarize anxiety and depression counts
count_data <- data_mh %>%
  group_by(age_group) %>%
  summarise(
    Anxiety = sum(sx_anx_score >= 3, na.rm = TRUE),  # Count anxiety scores >= 3
    Depression = sum(sx_dep_score >= 3, na.rm = TRUE)  # Count depression scores >= 3
  )


# Reshape the data for plotting
count_data_long <- count_data %>%
  pivot_longer(cols = c(Anxiety, Depression), 
               names_to = "condition", 
               values_to = "count")


# Plot the double bar graph
ggplot(count_data_long, aes(x = age_group, y = count, fill = condition)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Age group (years)", 
       y = "Count (n)", 
       fill = "Symptom correlation") +
  scale_fill_manual(values = c("Anxiety" = "#ED1C24", "Depression" = "darkred")) +
  theme_minimal()


# Plot line graph
ggplot(count_data_long, aes(x = age_group, y = count, color = condition, group = condition)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  labs(x = "Age Group (years)", 
       y = "Count (n)", 
       color = "Symptom correlation") +
  scale_color_manual(values = c("Anxiety" = "#ED1C24", "Depression" = "darkred")) +
  theme_minimal()

#==============================================================================
# End of code
#==============================================================================
