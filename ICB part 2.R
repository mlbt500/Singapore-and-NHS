# Step 1: Filter practices with over 5000 patients per GP
high_patient_load_practices <- merged_data[merged_data$Patients_Per_GP > 5000, ]

# Step 2: Aggregate the total number of patients for these practices within each ICB
total_patients_high_load_per_icb <- aggregate(TOTAL_PATIENTS ~ ICB_NAME, data = high_patient_load_practices, sum)

# Prepare the new column for the main dataframe
# Initialize with zeros or NA (if you prefer to signify no data for ICBs without any such practices)
dataframe$"Patients_in_High_Load_Practices" <- NA
names(total_patients_high_load_per_icb)[names(total_patients_high_load_per_icb) == "TOTAL_PATIENTS"] <- "Patients_in_High_Load_Practices"

# Step 3: Merge this info into your summary dataframe
for (i in 2:nrow(dataframe)) { # Start from 2 to skip "Grand Total"
  icb_name <- dataframe$ICB[i]
  # Find the total from the aggregated data
  total_for_icb <- total_patients_high_load_per_icb$Patients_in_High_Load_Practices[total_patients_high_load_per_icb$ICB_NAME == icb_name]
  
  if(length(total_for_icb) > 0) {
    dataframe$"Patients_in_High_Load_Practices"[i] <- total_for_icb
  } else {
    # If there are no practices with >5000 patients/GP in an ICB, you might want to set this to 0 or NA
    dataframe$"Patients_in_High_Load_Practices"[i] <- 0 # or NA, depending on how you wish to represent it
  }
}

# Note: For the "Grand Total" row, if you want to sum all high load patients across all ICBs, you can do it like this:
dataframe$"Patients_in_High_Load_Practices"[1] <- sum(total_patients_high_load_per_icb$Patients_in_High_Load_Practices)

# Step 1: Filter practices with over 5000 patients per GP
high_patient_load_practices <- merged_data[merged_data$Patients_Per_GP > 5000, ]

# Filter for practices with over 5000 Carr-Hill adjusted patients per GP
high_carr_hill_patient_load_practices <- merged_data[merged_data$Carr_Hill_Patients_Per_GP > 5000, ]

# Step 2: Aggregate the total number of patients for these practices within each ICB
total_patients_high_load_per_icb <- aggregate(TOTAL_PATIENTS ~ ICB_NAME, data = high_patient_load_practices, sum)
total_patients_high_carr_hill_load_per_icb <- aggregate(TOTAL_PATIENTS ~ ICB_NAME, data = high_carr_hill_patient_load_practices, sum)

# Prepare the new columns for the main dataframe
# Initialize with zeros or NA (if you prefer to signify no data for ICBs without any such practices)
dataframe$"Patients_in_High_Load_Practices" <- NA
dataframe$"Patients_in_High_Carr_Hill_Load_Practices" <- NA

# Rename for clarity in merging
names(total_patients_high_load_per_icb)[names(total_patients_high_load_per_icb) == "TOTAL_PATIENTS"] <- "Patients_in_High_Load_Practices"
names(total_patients_high_carr_hill_load_per_icb)[names(total_patients_high_carr_hill_load_per_icb) == "TOTAL_PATIENTS"] <- "Patients_in_High_Carr_Hill_Load_Practices"

# Step 3: Merge this info into your summary dataframe for both metrics
for (i in 2:nrow(dataframe)) { # Start from 2 to skip "Grand Total"
  icb_name <- dataframe$ICB[i]
  
  # Total for normal patient load
  total_for_icb <- total_patients_high_load_per_icb$Patients_in_High_Load_Practices[total_patients_high_load_per_icb$ICB_NAME == icb_name]
  if(length(total_for_icb) > 0) {
    dataframe$"Patients_in_High_Load_Practices"[i] <- total_for_icb
  } else {
    dataframe$"Patients_in_High_Load_Practices"[i] <- 0 # or NA
  }
  
  # Total for Carr-Hill adjusted patient load
  total_carr_hill_for_icb <- total_patients_high_carr_hill_load_per_icb$Patients_in_High_Carr_Hill_Load_Practices[total_patients_high_carr_hill_load_per_icb$ICB_NAME == icb_name]
  if(length(total_carr_hill_for_icb) > 0) {
    dataframe$"Patients_in_High_Carr_Hill_Load_Practices"[i] <- total_carr_hill_for_icb
  } else {
    dataframe$"Patients_in_High_Carr_Hill_Load_Practices"[i] <- 0 # or NA
  }
}

# For the "Grand Total" row, sum all high load patients across all ICBs for both metrics
dataframe$"Patients_in_High_Load_Practices"[1] <- sum(total_patients_high_load_per_icb$Patients_in_High_Load_Practices)
dataframe$"Patients_in_High_Carr_Hill_Load_Practices"[1] <- sum(total_patients_high_carr_hill_load_per_icb$Patients_in_High_Carr_Hill_Load_Practices)

# Print the updated dataframe to check
print(dataframe)

