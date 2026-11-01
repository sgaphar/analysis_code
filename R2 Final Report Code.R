#==============================================================================

# Vaccination Campaign - ALL ROUNDS
# Sokvax Project, Nigeria Mission, MSF

#==============================================================================

# Import R1 data
r1data <- import(here::here("..", "01 Information", "sites_master.xlsx")) 

#==============================================================================


#==============================================================================
# FIGURE 4 - SCATTERPLOT SITEDAYS AND VACCINATED
#==============================================================================

# Create table of total number of children vaccinated by ward
tab_vax_ward_r1 <- r1data %>%
  group_by(lga, ward) %>%
  summarize(
    total_sum_children = sum(r1_vaccinated, na.rm = TRUE),
    sitedays = sum(r1_opdays, na.rm = TRUE),
    .groups = "drop")


# Combine data for R1 and R2 and put in single plot
tab_vax_ward_allrounds <- tab_vax_ward_r1 %>%
  left_join(tab_vax_ward, by = c("lga", "ward")) %>%
  mutate(
    total_sum_children = total_sum_children.x + coalesce(total_sum_children.y, 0),
    sitedays = sitedays.x + coalesce(sitedays.y, 0)
  ) %>%
  select(-total_sum_children.x, -total_sum_children.y, -sitedays.x, -sitedays.y , -round)


#------------------------------------------------------------------------------

# Create scatterplot for relationship between sitedays and vaccinated children
plot_vax_sitedays_allrounds <- ggplot(tab_vax_ward_allrounds, aes(x = sitedays, y = total_sum_children)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "firebrick2") +
  labs(
    x = "Site-days",
    y = "Vaccinated (n)") +
  scale_x_continuous(breaks = seq(0, 100, by = 25)) +
  theme_minimal()

plot_vax_sitedays_allrounds

#------------------------------------------------------------------------------

# Conduct linear regression to see the relationship between site-days and children vaccinated
lm_sitedays_vax_allrounds <- lm(total_sum_children ~ sitedays, data = tab_vax_ward)


# Summary of the linear regression
summary(lm_sitedays_vax_allrounds)

#==============================================================================
# FIGURE - CLUSTERED BAR CHART ALL ROUNDS BY ANTIGEN
#==============================================================================

# Manually create dataframe to include data from Round 1
antigens_allrounds <- data.frame(
  vaccine = c("Penta", "Measles", "OPV", "IPV", "PCV", "Rota", "YF", "MenA", "BCG", "HepB", "VitA"),
  round_1 = c(51915, 1370, 5027, 2307, 4961, 2794, 412, 380, 489, 134, 87),
  round_2 = c(40805, 16267, 1365, 666, 1757, 191, 403, 341, 101, 489, 23)
) # ADD NUMBERS FROM ROUND 3


# Reshape the data for ggplot
antigens_long <- reshape(antigens_allrounds, 
                         varying = c("round_1", "round_2"), 
                         v.names = "num", 
                         timevar = "round", 
                         times = c("Round 1", "Round 2"), 
                         direction = "long")


# Create the clustered bar chart
ggplot(antigens_long, aes(x = vaccine, y = num, fill = round, group = round)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +  # Add black outline to bars
  labs(x = "Antigen", y = "Vaccines administered (n)") +
  scale_fill_manual(values = c("Round 1" = "indianred1", "Round 2" = "darkseagreen")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#==============================================================================


# Assuming your data is in a data frame called data_edit
penta1_r2_ward<- data_edit %>%
  group_by(ward) %>%
  summarise(
    round_2 = sum(penta1_0_11m, penta1_12_23m, penta1_24_59m, na.rm = TRUE)
  )




# Manually create a dataframe including all rounds for PENTA 1 only
penta1_allrounds_ward <- data.frame(
  ward = c("Rikina", "Tuntube Tsefe", "Magajin Gari A", "Magajin Gari B", "Magajin Rafi A", "Magajin Rafi B", "Sarkin Adar Gandu", "Sarkin Adar Gidan Igwai", "Sarkin Musulmi A", "Sarkin Musulmi B", "Waziri A", "Waziri B", "Waziri C", "Gagi A", "Gagi B", "Gagi C", "Rijiya A", "Rijiya B", "Sarkin Adar A", "Sarkin Adar B", "Sarkin Zamfara A", "Sarkin Zamfara B", "Tudun Wada A", "Tudun Wada B", "Arkilla", "Bado", "Kalambaina"),
  round_1 = c(2272, 1984, 572, 757, 543, 514, 432, 1878, 296, 573, 397, 446, 719, 1462, 956, 633, 
              475, 757, 465, 861, 655, 624, 520, 1425, 5329, 2655, 5364))

# Left join the data from R1 to a new dataframe for all rounds
penta1_allrounds_ward <- left_join(penta1_allrounds_ward, penta1_r2_ward, by = "ward")


# Create a new column which is the sum of R1 and R2 Penta 1
penta1_allrounds_ward <- penta1_allrounds_ward %>%
  mutate(penta1_total = coalesce(round_1, 0) + coalesce(round_2, 0))


# Left join to the shapefile
shp_adm3_sokvax <- left_join(shp_adm3_sokvax, penta1_allrounds_ward, by = c("adm3_name" = "ward"))


# sum all penta 1 antigens in R1
sum_penta1_r1 <- sum(penta1_allrounds_ward$round_1, na.rm = TRUE)
sum_penta1_r2 <- sum(penta1_allrounds_ward$round_2, na.rm = TRUE)



#==============================================================================

# Create a box for the urban areas only
bbox_adm3 <- st_bbox(shp_adm3_sokvax)


# Generate a map of the data
map_penta1 <- ggplot(data = shp_adm3_sokvax) +
  geom_sf(aes(fill = round_1)) +
  geom_sf(data = shp_adm3_sokvax,
          colour = "black",
          linewidth = 0.5,
          fill = NA) +
  geom_sf(data = shp_adm2_sokvax,
          color = "black",
          linewidth = 1.1,
          fill = NA) + 
  scale_fill_gradient(low = "ivory",
                      high = "firebrick2",
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
map_penta1

#==============================================================================
map_sites <- ggplot(data = shp_sites) +
  geom_sf(data = shp_adm3_sokvax,
          colour = "black",
          linewidth = 0.5,
          fill = "gray") +
  geom_sf(data = shp_adm2_sokvax,
          color = "black",
          linewidth = 1.1,
          fill = NA) +
  geom_sf(aes(geometry = geometry), color = "black", size = 1) +
  theme(
    legend.position = "right",   
    axis.text = element_blank(), 
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 8)
  ) +
  coord_sf(xlim = c(bbox_adm3["xmin"], bbox_adm3["xmax"]), 
           ylim = c(bbox_adm3["ymin"], bbox_adm3["ymax"])) 

# Print map
map_sites


# Generate the map with each adm2 region having a different color
map_sites <- ggplot(data = shp_sites) +
  geom_sf(data = shp_adm3_sokvax,
          colour = "black",
          linewidth = 0.5) +
  geom_sf(data = shp_adm3_sokvax,
          aes(fill = factor(adm2_name)),
          color = "black",
          linewidth = 0.5) +
  geom_sf(aes(geometry = geometry), color = "black", size = 1) +
  scale_fill_manual(values = c(
    "Wamakko" = "indianred2",   
    "Dange Shuni" = "lightgoldenrod1",   
    "Sokoto North" = "darkseagreen2",  
    "Sokoto South" = "darkslategray3"),
    name = "LGA") + 
  theme(
    legend.position = "right",   
    axis.text = element_blank(), 
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 8)
  ) +
  coord_sf(xlim = c(bbox_adm3["xmin"], bbox_adm3["xmax"]), 
           ylim = c(bbox_adm3["ymin"], bbox_adm3["ymax"])) 

# Print map
map_sites




map_sites


#==============================================================================

# Create a box for the urban areas only
bbox_adm3 <- st_bbox(shp_adm3_sokvax)


# Generate a map of the data
map_vax_all <- ggplot(data = shp_adm3_sokvax) +
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

# Create horizontal bar chart with total vaccinated per site

library(dplyr)

# Filter out rows where vaccine_name1 is not 0 or NA, and select the corresponding columns
unusablevials_data <- data_edit %>%
  filter(!is.na(vaccine_name1) & vaccine_name1 != 0) %>%
  select(date, vaccine_name1, site_name, ward)

# View the filtered data
head(unusablevials_data)





library(dplyr)
library(tidyr)

# Create the table with the columns: received, remaining, difference
library(tidyr)
library(dplyr)

# Create the supply_data data frame
supply_data <- data.frame(
  item = c("Syringe 0.5mL", "Syringe 5mL", "19g Needle", "Safety Box", "Gloves", "Cotton"),
  received = c(data_edit$syringe_05ml_received,
               data_edit$syringe_5ml_recieved,
               data_edit$x19g_needle_recieved,
               data_edit$safety_box_recieved, 
               data_edit$gloves_recieved, 
               data_edit$cotton_recieved),
  remaining = c(data_edit$syringe_05ml_remaining, 
                data_edit$syringe_5ml_remaining, 
                data_edit$x19g_needle_remaining,
                data_edit$safety_box_remaining, 
                data_edit$gloves_remaining, 
                data_edit$cotton_remaining)
)

# Pivot the data wider, spreading the received and remaining columns into separate columns
# Load dplyr package
library(dplyr)

# Summarize the data
supply_data_wide <- supply_data %>%
  group_by(item) %>%
  summarize(
    total_remaining = sum(remaining, na.rm = TRUE),  # Sum of remaining
    total_received = sum(received, na.rm = TRUE)     # Sum of received
  )


# Calculate the difference (remaining - received) for each item
supply_data_wide$used <- supply_data_wide$total_received - supply_data_wide$total_remaining

# Factor the 'item' column with the specified levels
supply_data_wide$item <- factor(supply_data_wide$item, levels = c("Syringe 0.5mL", "Syringe 5mL", "19g Needle", "Safety Box", "Gloves", "Cotton"))





#==============================================================================
# Create a horizontal bar chart by ward
#==============================================================================

# Rename columns for R1
tab_vax_ward_r1 <- tab_vax_ward_r1 %>%
  rename(children_r1 = total_sum_children,
         sitedays_r1 = sitedays)

# Rename columns for R2
tab_vax_ward_r2 <- tab_vax_ward %>%
  rename(children_r2 = total_sum_children,
         sitedays_r2 = sitedays)


# Left join to pull R1 and R2 data into single df
tab_vax_ward_allrounds <- tab_vax_ward_allrounds %>%
  left_join(tab_vax_ward_r1, by = c("lga", "ward")) %>%
  left_join(tab_vax_ward_r2, by = c("lga", "ward"))


# Remove columns not required
tab_vax_ward_allrounds <- tab_vax_ward_allrounds %>%
  select(-round)


# Reshape the data long
long_vax_ward_allr <- gather(tab_vax_ward_allrounds, key = "variable", value = "value", children_r1, children_r2)


# Rename for reporting
long_vax_ward_allr <- long_vax_ward_allr %>%
  mutate(variable = recode(variable, 
                           "children_r1" = "Round 1", 
                           "children_r2" = "Round 2"))

# Create plot
library(ggplot2)
library(dplyr)

# Create a new column for the x-axis group labels (e.g., 'lga')
long_vax_ward_allr <- long_vax_ward_allr %>%
  mutate(lga_label = as.factor(lga))

ggplot(long_vax_ward_allr, aes(x = value, y = ward, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge", color = "black", size = 0.5) +  # Add black border around bars
  labs(x = "Vaccinated (n)", y = "Ward", fill = "Round") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_fill_manual(values = c("Round 1" = "indianred1", "Round 2" = "darkseagreen")) +  # Change colors for 'Round 1' and 'Round 2'
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10),
        strip.text = element_text(size = 12))
#==============================================================================

# Create line chart with cumulative number of children vaccinated per day in Round 2

# Load necessary library
library(ggplot2)

# Assuming tab_daily_total is your dataframe
# Load necessary library
library(ggplot2)

# Create the plot
ggplot(tab_daily_total, aes(x = date, y = cumulative_total)) +
  geom_line() +
  geom_point() +
  labs(x = "Date of campaign",
       y = "Cumulative vaccinated (n)") +
  theme_minimal() +
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 day") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#==============================================================================

# Create table with screened not vaccinated R2

# Assuming you have a dataframe called data_edit

# Summing the values from the specified columns
screened <- colSums(data_edit[, c("vacc_0_11", "vacc_12_23", "vacc_24_59")], na.rm = TRUE)

# Creating a data frame for a more readable table
screened_tab <- data.frame(
  Category = names(screened),
  Sum = screened
)

# Display the table
print(screened_tab)
#==============================================================================
# Create data for R3 planning
#==============================================================================

tab_vax_site <- tab_vax_site %>%
  rename(total_r2 = total_sum_children) %>%
  rename(opdays_r2 = opdays) %>%
  select(-round) %>%
  mutate(vaxperday = total_r2 / opdays_r2)


tab_vax_site_wam <- tab_vax_site %>%
  filter(lga == "Wamakko")
  
tab_vax_site_ds <- tab_vax_site %>%
  filter(lga == "Dange Shuni")
  
tab_vax_site_ss <- tab_vax_site %>%
  filter(lga == "Sokoto South")

tab_vax_site_sn <- tab_vax_site %>%
  filter(lga == "Sokoto North")


write.csv(tab_vax_site, file = "tab_vax_site_r2.csv")

#==============================================================================
# End of code
#==============================================================================
