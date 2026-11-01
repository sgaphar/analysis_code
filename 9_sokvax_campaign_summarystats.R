#==============================================================================

# Vaccination Campaign Round 2 Data
# Sokvax Project, Nigeria Mission, MSF

#==============================================================================

# This code create simple statistical calculations that are beneficial for report writing. 

#==============================================================================

# Count total number of males versus females
summary_sex <- tab_sex_edit %>%
  group_by(sex) %>%
  summarise(total_count = sum(count, na.rm = TRUE)) %>%
  mutate(percentage = total_count / sum(total_count) * 100)


# Create a total row
total_row_sex <- summarise(summary_sex, 
                       sex = "total", 
                       total_count = sum(total_count), 
                       percentage = 100)


# Bind total row to the summary table
summary_sex <- bind_rows(summary_sex, total_row_sex)


# View the result
print(summary_sex)

#==============================================================================

# Calculate proportion of vaccines that are penta


# Calculate proportion of vaccines that are measles


# Calculate proportion of vaccines that are not penta or measles




#==============================================================================
# End of code
#==============================================================================

