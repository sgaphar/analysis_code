#==============================================================================

# Vaccination Campaign Round 2 Data
# Sokvax Project, Nigeria Mission, MSF

#==============================================================================

# This script creates maps based on number of children vaccinated per ward. This includes the entire Sokoto Urban Area. For LGA or other specific geographical distributions, see other folders in 04 Analysis. 

#==============================================================================
# Create values for maps
#==============================================================================

# Put data into table
tab_vax_ward <- data_edit %>%
  group_by(ward) %>%
  summarize(sum_children_ward = sum(sum_children, na.rm = TRUE))

#==============================================================================

# Add the values to the shapefile
shp_adm3_sokvax <- shp_adm3_sokvax %>%
  left_join(tab_vax_ward, by = c("adm3_name" = "ward"))


# Give each ward a number to be a label on the map
shp_adm3_sokvax <- shp_adm3_sokvax %>%
  mutate(label_num = case_when(
    adm3_name == "Magajin Gari A" ~ 1,
    adm3_name == "Magajin Gari B" ~ 2,
    adm3_name == "Magajin Rafi A" ~ 3,
    adm3_name == "Magajin Rafi B" ~ 4,
    adm3_name == "Sarkin Adar Gandu" ~ 5,
    adm3_name == "Sarkin Adar Gidan Igwai" ~ 6,
    adm3_name == "Sarkin Musulmi A" ~ 7,
    adm3_name == "Sarkin Musulmi B" ~ 8,
    adm3_name == "Waziri A" ~ 9,
    adm3_name == "Waziri B" ~ 10,
    adm3_name == "Waziri C" ~ 11,
    adm3_name == "Gagi A" ~ 12,
    adm3_name == "Gagi B" ~ 13,
    adm3_name == "Gagi C" ~ 14,
    adm3_name == "Rijiya A" ~ 15,
    adm3_name == "Rijiya B" ~ 16,
    adm3_name == "Sarkin Adar A" ~ 17,
    adm3_name == "Sarkin Adar B" ~ 18,
    adm3_name == "Sarkin Zamfara A" ~ 19,
    adm3_name == "Sarkin Zamfara B" ~ 20,
    adm3_name == "Tudun Wada A" ~ 21,
    adm3_name == "Tudun Wada B" ~ 22,
    adm3_name == "Arkilla" ~ 23,
    adm3_name == "Bado" ~ 24,
    adm3_name == "Kalambaina" ~ 25,
    adm3_name == "Rikina" ~ 26,
    adm3_name == "Tuntube Tsefe" ~ 27,
    TRUE ~ NA_real_ 
  ))

#==============================================================================
# Create map for urban Sokoto
#==============================================================================

# Create a box for the urban areas only
bbox_adm3 <- st_bbox(shp_adm3_sokvax)


# Generate a map of the data
map_vax_r2 <- ggplot(data = shp_adm3_sokvax) +
  geom_sf(aes(fill = sum_children_ward)) +
  geom_sf(data = shp_adm3_sokvax,
          colour = "black",
          linewidth = 0.5,
          fill = NA) +
  geom_sf(data = shp_adm2_sokvax,
          color = "black",
          linewidth = 1.1,
          fill = NA) + 
  geom_sf_text(aes(label = label_num),
               color = "black",
               size = 1.4) +
  scale_fill_gradient(low = "lightgreen",
                      high = "darkgreen",
                      name = "Vaccinated (n)",
                      na.value = "lightgrey") +
  theme(
    legend.position = "right",   
    axis.text = element_blank(), 
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 8),
  ) +
  coord_sf(xlim = c(bbox_adm3["xmin"], bbox_adm3["xmax"]), 
          ylim = c(bbox_adm3["ymin"], bbox_adm3["ymax"]))


# Print map
map_vax_r2

#==============================================================================
# End of code
#==============================================================================
