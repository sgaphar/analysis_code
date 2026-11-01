#==============================================================================
# 
# Retrieve Data from Kobo for CBS
#
#==============================================================================

# Install and load required packages
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")}
library(pacman)


# Install and load required packages to run script
pacman::p_load(httr,         # Manipulate data
               jsonlite,
               dplyr,
               tidyr,
               stringr)

#==============================================================================

# Specify the Kobo URL and credentials
kobo_url <- "https://kobo.msf.org"
username <- "msfe_jerusalem_epi"
password <- "45Pancakes##kobo"
form_id <- "aEheU9inSLN7XqYmWVZE8V"
api <- "e485119d17c7a2def540665a44be18e7ca3f9704"


# Define the data URL
data_url <- paste0(kobo_url, "/api/v2/assets/", form_id, "/data/")


# Authenticate and extract the data
response <- GET(
  data_url,
  authenticate(username, password),
  accept_json())


# Check for successful response (200 is a successful HTML request)
if (response$status_code == 200)
  # Parse the JSON content
  data <- content(response, "text", encoding = "UTF-8")
  data_json <- fromJSON(data, flatten = TRUE)
  
#==============================================================================
  
# Convert to a data frame
data_raw <- bind_rows(data_json$results, .id = "id")
  

# Create new edit data frame
data_edit <- data_raw
  
  
# Clean header names  
names(data_edit) <- str_replace_all(names(data_raw), "/", "_")


# Identify columns to be removed
columns_to_remove <- c("id", "formhub_uuid", "start", "end", "deviceid",
                        "__version__", "meta_instanceID", "_xform_id_string",
                        "_uuid", "_attachments", "_status", "_geolocation",
                        "_submission_time", "_tags", "_notes")


# Remove columns not required
data_edit <- data_edit %>%
  select(-all_of(columns_to_remove)) %>%
  rename(id = `_id`)

#==============================================================================
# End of code
#==============================================================================

