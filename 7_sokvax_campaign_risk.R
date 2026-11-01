
#==============================================================================

# Vaccination Campaign Round 2 Data
# Sokvax Project, Nigeria Mission, MSF

#==============================================================================


# This code is for all risk management monitoring including AEFIs, AEB, and passive communicable disease surveillance. 


#==============================================================================
# Adverse events following immunization
#==============================================================================

# Sum all reports of AEFI
aefi_num <- sum(data_edit$aefi_number, na.rm = TRUE)

# Print number of AEFI incidents
aefi_num


#==============================================================================
# Accidental exposure to blood
#==============================================================================

# Sum all reports of AEB
aeb_num <- sum(data_edit$aeb_number, na.rm = TRUE)

# Print number of AEB incidents
aeb_num


#==============================================================================
# Disease surveillance
#==============================================================================

# Convert the variables to numeric
data_edit$measles_cases <- as.numeric(data_edit$measles_cases)
data_edit$polio_cases <- as.numeric(data_edit$polio_cases)
data_edit$yellow_fever_cases <- as.numeric(data_edit$yellow_fever_cases)
data_edit$neonatal_tetanus_cases <- as.numeric(data_edit$neonatal_tetanus_cases)
data_edit$fever_coughing_cases <- as.numeric(data_edit$fever_coughing_cases)
data_edit$diarrhea_cases <- as.numeric(data_edit$diarrhea_cases)



# Count the number of cases where the value is 1 for each disease
measles_count <- sum(data_edit$measles_cases == 1, na.rm= TRUE)
polio_count <- sum(data_edit$polio_cases == 1, na.rm= TRUE)
yf_count <- sum(data_edit$yellow_fever_cases == 1, na.rm= TRUE)
tetanus_count <- sum(data_edit$neonatal_tetanus_cases == 1, na.rm= TRUE)
fever_count <- sum(data_edit$fever_coughing_cases == 1, na.rm= TRUE)
diarrhea_count <- sum(data_edit$diarrhea_cases == 1, na.rm= TRUE)


# Create a data frame with disease names and their respective counts
disease_counts <- data.frame(
  Disease = c("Measles", "Polio", "Yellow Fever", "Neonatal Tetanus", "Fever Coughing", "Diarrhea"),
  Count = c(measles_count, polio_count, yf_count, tetanus_count, fever_count, diarrhea_count)
)

# Print the table
print(disease_counts)


#==============================================================================
# End of code
#==============================================================================


