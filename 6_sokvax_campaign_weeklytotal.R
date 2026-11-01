#==============================================================================

# Vaccination Campaign Round 2 Data
# Sokvax Project, Nigeria Mission, MSF

#==============================================================================

# This code creates a daily and cumulative total of all cases. It also creates a weekly summary of cases. 

#==============================================================================
# Daily totals
#==============================================================================

# Add total children vaccinated column
daily_total <- data_edit %>%
  mutate(sum_children = rowSums(select(., all_of(col_sum)), na.rm = TRUE))

#==============================================================================

# Create a table for daily totals
tab_daily_total <- daily_total %>%
  group_by(date) %>%
  summarise(
    sum_children_total = sum(sum_children, na.rm = TRUE)) %>%
  ungroup()


# Create a cumulative summary for each day
tab_daily_total <- tab_daily_total %>%
  mutate(
    date = as.Date(date),                       
    week_number = isoweek(date),                 
    cumulative_total = cumsum(sum_children_total))


# Convert table to flextable
tab_daily_total_ft <- flextable(tab_daily_total)


# Print flextable
tab_daily_total_ft

#==============================================================================

# Create a table for weekly totals
tab_week_total <- tab_daily_total %>%
  group_by(week_number) %>%
  summarize(week_total = sum(sum_children_total, na.rm = TRUE))


# Convert the table to a flextable
tab_week_total_ft <- flextable(tab_week_total)


# Print flextable
print(tab_week_total_ft)


#==============================================================================
# End of code
#==============================================================================

