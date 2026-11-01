#==============================================================================

# Vaccination Campaign Round 2 Data
# Sokvax Project, Nigeria Mission, MSF

#==============================================================================

# This script is to ensure all required packages are loaded, the raw data is imported, the reporting dates are correct, and the data frame is ready for analysis.

#==============================================================================
# Setup 
#==============================================================================

# Manually modify reporting timeframe
startdate <- "2025-01-20"
enddate <- "2025-02-27"

#------------------------------------------------------------------------------

# Install required packages
if (!require("pacman")) {install.packages("pacman")}
pacman::p_load(
  tidyverse,
  janitor,
  here,
  scales,
  labelled,
  matchmaker,
  readxl,
  openxlsx,
  apyramid,
  sitrep,
  survey,
  srvyr,
  gt,
  gtsummary,
  flextable,
  ggh4x,
  patchwork,
  rio,
  sf,
  ggspatial,
  ggrepel,
  lubridate,
  ggplot2,
  stringr)


#==============================================================================
# Import data using the latest data extract
#==============================================================================

# NOTE: You will need to manually extract the data from Kobo and save it in 03 Raw Data


# Identify save location of raw data
data_path <- here::here("03 Raw Data")


# Generate a list of all excel files 
xls_files <- list.files(data_path, pattern = "\\.xlsx$", full.names = TRUE)


# Get file info to check modification time
file_info <- file.info(xls_files)


# Sort files by the modification time, most recent first
latest_file <- xls_files[which.max(file_info$mtime)]


# Read the latest Excel file
data_raw <- read_excel(latest_file)

#==============================================================================
# Clean data
#==============================================================================

# Clean column names
data_edit <- data_raw %>%
  clean_names()


# Import list of cleaned site names
clean_site_names <- import(here::here("04 Analysis", "01 Data Checks", "clean_site_names.xlsx")) # Update this file with new sites if applicable


# Perform a left join to add site_code from clean_sitename to data_edit
data_edit <- data_edit %>%
  left_join(clean_site_names, by = c("site_name" = "kobo name"))


# Ensure there are no missing sites by creating new df to identify missing code numbers
missing_sites <- data_edit %>%
  filter(is.na(site_code))

missing_sites

# Remove "ward.y" and "site" columns from data_edit that were created
data_edit <- data_edit %>%
  select(-ward.y, -site) %>%
  rename(ward = ward.x)

#------------------------------------------------------------------------------

# Clean the round names
data_edit <- data_edit %>%
  mutate(round = str_to_lower(round), 
         round = str_replace_all(round, 
                                 c(" " = "", "_" = "")))

#------------------------------------------------------------------------------

# Format dates
data_edit <- data_edit %>%
  mutate(date = lubridate::ymd(date)) %>%
  filter(date >= startdate & date <= enddate)

#------------------------------------------------------------------------------

# Identify date of latest entry in Kobo
date_latest <- max(data_edit$date, na.rm = TRUE)


# Identify data of earliest entry in Kobo
date_earliest <- min(data_edit$date, na.rm = TRUE)


#==============================================================================
# Create new column for sum of children
#==============================================================================

# Identify columns to sum for total children vaccinated per site
col_sum <- c("r1_m_0_11m", "r1_f_0_11m", "r1_m_12_23m", "r1_f_12_23m",
             "r1_m_24_59m", "r1_f_24_59m", "r2_m_0_11m", "r2_f_0_11m",
             "r2_m_12_23m", "r2_f_12_23m", "r2_m_24_59m", "r2_f_24_59m",
             "r3_m_0_11m", "r3_f_0_11m", "r3_m_12_23m", "r3_f_12_23m",
             "r3_m_24_59m", "r3_f_24_59m")


# Identify columns for Round 1 only 
col_sum_r1 <- c("r1_m_0_11m", "r1_f_0_11m", "r1_m_12_23m", "r1_f_12_23m",
                "r1_m_24_59m", "r1_f_24_59m")


# Identify columns for Round 2 only
col_sum_r2 <- c("r2_m_0_11m", "r2_f_0_11m",
                "r2_m_12_23m", "r2_f_12_23m", "r2_m_24_59m", "r2_f_24_59m")


# Identify colums for Round 3 only
col_sum_r3 <- c("r3_m_0_11m", "r3_f_0_11m", "r3_m_12_23m", "r3_f_12_23m",
                "r3_m_24_59m", "r3_f_24_59m")

#------------------------------------------------------------------------------

# Add total children vaccinated column for all rounds
data_edit <- data_edit %>%
  mutate(sum_children = rowSums(select(., all_of(col_sum)), na.rm = TRUE))


# Create total children vaccinated column for Round 1
data_edit <- data_edit %>%
  mutate(sum_children_r1 = rowSums(select(., all_of(col_sum_r1)), na.rm = TRUE))


# Create total children vaccinated column for Round 2
data_edit <- data_edit %>%
  mutate(sum_children_r2 = rowSums(select(., all_of(col_sum_r2)), na.rm = TRUE))


# Create total children vaccinated column for Round 3
data_edit <- data_edit %>%
  mutate(sum_children_r3 = rowSums(select(., all_of(col_sum_r3)), na.rm = TRUE))

#==============================================================================

# Rename wards to match shapefiles
data_edit <- data_edit %>%
  mutate(ward = recode(ward,
                       "Tudun wada A" = "Tudun Wada A",
                       "Tudun wada B" = "Tudun Wada B"))

#==============================================================================

# Create table of total number of children vaccinated by site
tab_vax_site <- data_edit %>%
  group_by(round, lga, ward, site_name, site_code) %>%
  summarize(total_sum_children = sum(sum_children, na.rm = TRUE), .groups = "drop")


# Create a count of occurrences of each site_code
site_code_count <- data_edit %>%
  group_by(site_code) %>%
  summarize(opdays = n(), .groups = "drop")


# Join the count back to tab_vax_site
tab_vax_site <- tab_vax_site %>%
  left_join(site_code_count, by = "site_code")

#------------------------------------------------------------------------------

# Create table of total number of children vaccinated by ward
tab_vax_ward <- tab_vax_site %>%
  group_by(round, lga, ward) %>%
  summarize(sitedays = sum(opdays, na.rm = TRUE), .groups = "drop")


# Create summaries per ward
tab_vax_ward <- tab_vax_site %>%
  group_by(round, lga, ward) %>%
  summarize(
    total_sum_children = sum(total_sum_children, na.rm = TRUE),
    sitedays = sum(opdays, na.rm = TRUE),
    .groups = "drop")


#==============================================================================
# Import Shapefiles for map creation
#==============================================================================

# Import shapefiles for LGA and ward boundaries
shp_adm1 <- import(here::here("03 Raw Data", "data-geo", "ADM1_sokoto.rds")) 
shp_adm1 <- st_as_sf(shp_adm1)

shp_adm2 <- import(here::here("03 Raw Data", "data-geo", "ADM2_sokoto.rds")) 
shp_adm2 <- st_as_sf(shp_adm2)

shp_adm3 <- import(here::here("03 Raw Data", "data-geo", "ADM3_sokoto.rds")) 
shp_adm3 <- st_as_sf(shp_adm3)


# Remove NA values in longitude and latitude columns
shp_sites <- clean_site_names %>%
  filter(!is.na(lon) & !is.na(lat))


# Convert to a shapefile and specify the lon and lat columns
shp_sites <- st_as_sf(shp_sites, coords = c("lon", "lat"), crs = 4326)


# Perform a left join to pull total_sum_children from tab_vax_site into shp_sites
shp_sites <- shp_sites %>%
  left_join(tab_vax_site %>%
              select(site_code, total_sum_children), 
            by = "site_code")

#------------------------------------------------------------------------------

# Restrict to Sokoto North, Sokoto South, Dange Shuni and Wamakko
shp_adm2_sokvax <- shp_adm2 %>%
  filter(adm2_name %in% c("Sokoto North", "Sokoto South", "Dange Shuni", "Wamakko"))


# Restrict to wards included in the campaign
shp_adm3_sokvax <- shp_adm3 %>%
  filter(adm2_name %in% c("Sokoto North", "Sokoto South", "Dange Shuni", "Wamakko"))


# Filter out non-urban wards in Dange Shuni and Wamakko
shp_adm3_sokvax <- shp_adm3_sokvax %>%
  filter(
    adm2_name %in% c("Sokoto North", "Sokoto South") |
      (adm2_name == "Dange Shuni" & str_detect(adm3_name, "Tuntube")) | # For some reason it was not pulling Tuntube-Tsefe so I did it like this instead
      (adm2_name == "Dange Shuni" & adm3_name == "Rikina") |
      (adm2_name == "Wamakko" & adm3_name %in% c("Arkilla", "Bado", "Kalambaina")))

#------------------------------------------------------------------------------

# Rename wards to match Kobo spelling
shp_adm3_sokvax <- shp_adm3_sokvax %>%
  mutate(adm3_name = recode(adm3_name,
                            "Rijia A" = "Rijiya A",
                            "Rijia B" = "Rijiya B",
                            "Sarkin Addar A" = "Sarkin Adar A",
                            "Sarkin Addar B" = "Sarkin Adar B",
                            "Tuntube-Tsefe" = "Tuntube Tsefe"))

#------------------------------------------------------------------------------

# Add scale
add_sca_br <- annotation_scale(location = "br", width_hint = 0.2)
add_sca_bl <- annotation_scale(location = "bl", width_hint = 0.2)


# Add north arrow
add_arr_br <- annotation_north_arrow(
  location = "br", which_north = "true",
  pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
  style = north_arrow_fancy_orienteering) 

add_arr_bl <- annotation_north_arrow(
  location = "bl", which_north = "true",
  pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
  style = north_arrow_fancy_orienteering)

#==============================================================================

# Clean environment
rm(file_info,
   shp_adm1,
   shp_adm2,
   shp_adm3)

#==============================================================================
# End of code
#==============================================================================
