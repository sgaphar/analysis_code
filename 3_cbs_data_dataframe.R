#==============================================================================
# 
# 3. Create dataframes and backup data
#
#==============================================================================

# Install and load required packages
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")}
library(pacman)


# Install and load required packages to run script
pacman::p_load(dplyr,             # Manipulate data
               lubridate,         # Work with dates
               tidyr,             # Clean data
               writexl
  )

#==============================================================================

# Create new dataframe for mental health module
data_mh <- data_edit %>% 
  select(id, interview_info_community, yearmonth, mental_health) %>%
  unnest(mental_health)

#==============================================================================

# Create new dataframe for pregnancy module
data_preg <- data_edit %>%
  select(id, interview_info_community, yearmonth, pregnancy_info) %>%
  unnest(pregnancy_info)
  
#==============================================================================

# Create new dataframe for MUAC module
data_muac_m <- data_edit %>%
  select(id, interview_info_community, yearmonth, nutrition_screen_male) %>%
  unnest(nutrition_screen_male)


data_muac_m <- data_muac_m %>%
  rename(muac_score = `nutrition_screen_male/muac_score`) %>%
  mutate(sex = "male")
  

data_muac_f <- data_edit %>%
  select(id, interview_info_community, yearmonth, nutrition_screen_female) %>%
  unnest(nutrition_screen_female)


data_muac_f <- data_muac_f %>%
  rename(muac_score = `nutrition_screen_female/muac_score_001`) %>%
  mutate(sex = "female")


# Combine MUAC dataframes into a singular dataframe
data_muac <- bind_rows(data_muac_m, data_muac_f)

#==============================================================================

# Identify date based on system settings
today <- format(Sys.Date(), "%Y_%m_%d")


# Construct file names
file_name_data <- paste0("cbs_heb_data_", today, ".xlsx")

file_name_mh <- paste0("cbs_heb_mh_", today, ".xlsx")

file_name_preg <- paste0("cbs_heb_preg_", today, ".xlsx")

file_name_muac <- paste0("cbs_heb_muac_", today, ".xlsx")
  
  
# Specify the save location
save_location <- "C:/Users/msfe-jerusalem-epi/OneDrive - MSF/1_Epidemiology/3_Projects/2_DMC_2024/2_CBS/1_Data"

#==============================================================================

# Save the dataframes 

tryCatch({
    write_xlsx(data_edit, path = file.path(save_location, file_name_data))
    print(paste("Successfully saved:", file_name_data))
}, error = function(e) {
    print(paste("Error saving", file_name_data, ":", e))
})

# Save data_mh
tryCatch({
    write_xlsx(data_mh, path = file.path(save_location, file_name_mh))
    print(paste("Successfully saved:", file_name_mh))
}, error = function(e) {
    print(paste("Error saving", file_name_mh, ":", e))
})

# Save data_preg
tryCatch({
    write_xlsx(data_preg, path = file.path(save_location, file_name_preg))
    print(paste("Successfully saved:", file_name_preg))
}, error = function(e) {
    print(paste("Error saving", file_name_preg, ":", e))
})

# Save data_muac
tryCatch({
    write_xlsx(data_muac, path = file.path(save_location, file_name_muac))
    print(paste("Successfully saved:", file_name_muac))
}, error = function(e) {
    print(paste("Error saving", file_name_muac, ":", e))
})

#==============================================================================
# End of code
#==============================================================================