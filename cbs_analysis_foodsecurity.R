#==============================================================================
# 
# Data Analysis - Food Security
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

# Create new dataframe to analyse food insecurity data
data_food <- data_edit %>%
  filter(food_security_food_enough == 'n')

#==============================================================================

# Summary for food limit
food_limit <- data_food %>%
  group_by(food_security_food_limit) %>%
  summarise(count = n(), .groups = 'drop') %>%
  filter(count > 0)

# Remove NA row
food_limit <- food_limit %>% filter(!is.na(food_security_food_limit))

# Create reverse cumulative count
food_limit <- food_limit%>%
  mutate(cumulative_count = rev(cumsum(rev(count))))

#==============================================================================

# Summary for food_security_food_restrict with reverse cumulative count
food_restrict <- data_food %>%
  group_by(food_security_food_restrict) %>%
  summarise(count = n(), .groups = 'drop') %>%
  filter(count > 0)

# Remove NA row
food_restrict <- food_restrict %>% filter(!is.na(food_security_food_restrict))

# Create reverse cumulative count
food_restrict <- food_restrict %>%
    mutate(cumulative_count = rev(cumsum(rev(count)))) 

#==============================================================================

# Summary for food_security_food_reduce with reverse cumulative count
food_reduce <- data_food %>%
  group_by(food_security_food_reduce) %>%
  summarise(count = n(), .groups = 'drop') %>%
  filter(count > 0)

# Remove rows with NA
food_reduce <- food_reduce %>% filter(!is.na(food_security_food_reduce))

# Create reverse cumulative count
food_reduce <- food_reduce %>%
  mutate(cumulative_count = (rev(cumsum(count))))

#==============================================================================

# Plot for limiting portion size of meals
ggplot(food_limit, aes(x = food_security_food_limit, y = cumulative_count)) +
  geom_bar(stat = "identity", fill = "darkred") +
  labs(title = "Limiting portion sizes",
       x = "Number of days",
       y = "Cumulative count (n)") +
  theme_minimal()


# Plot for restrict consumption by adults to allow for children to eat
ggplot(food_restrict, aes(x = food_security_food_restrict, y = cumulative_count)) +
  geom_bar(stat = "identity", fill = "darkred") +
  labs(title = "Restriction of consumption by adults to allow for children to eat",
       x = "Number of days",
       y = "Cumulative count (n)") +
  theme_minimal()


# Plot for reducing overall number of meals
ggplot(food_reduce, aes(x = food_security_food_reduce, y = cumulative_count)) +
  geom_bar(stat = "identity", fill = "darkred") +
  labs(title = "Reducing the overall number of meals",
       x = "Number of days",
       y = "Cumulative count (n)") +
  theme_minimal()

#==============================================================================

# Food insecurity by community

food_location <- data_food %>%
  group_by(interview_info_community) %>%
  summarise(count = n(), .groups = 'drop') 


ggplot(food_location, aes(x = interview_info_community, y = count)) +
  geom_bar(stat = "identity", fill = "darkred") +
  labs(x = "Location",
       y = "Households (n)") +
  theme_minimal()



#==============================================================================
# End of code
#==============================================================================

