library(dplyr)
library(htmlTable)

#load csvs

data1 <- read.csv("no_patients_by_practice.csv" ) # dec 2023 https://digital.nhs.uk/data-and-information/publications/statistical/general-and-personal-medical-services
data2 <- read.csv("nhspaymentsgp-21-22-prac-csv-v2.csv") # Nov 2022 https://digital.nhs.uk/data-and-information/publications/statistical/nhs-payments-to-general-practice/england-2021-22/results
data3 <- read.csv("GPPS_2023.csv")


#filter

selected_cols <- c("PRAC_CODE", "ICB_NAME", "TOTAL_PATIENTS", "TOTAL_GP_FTE")
data1_filtered <- data1[,selected_cols]

selected_cols <- c("Practice.Name", "Practice.Code", "Practice.Postcode", "Number.of.Weighted.Patients..Last.Known.Figure.") 
data2_filtered <- data2[,selected_cols]

#combine
names(data2_filtered)[names(data2_filtered) == "Practice.Code"] <- "PRAC_CODE"
merged_data <- merge(data1_filtered, data2_filtered, by = "PRAC_CODE")

# Calculate patients per GP and Carr-Hill weighted patients per GP
merged_data$Patients_Per_GP <- merged_data$TOTAL_PATIENTS / merged_data$TOTAL_GP_FTE
merged_data$Carr_Hill_Patients_Per_GP <- merged_data$Number.of.Weighted.Patients..Last.Known.Figure. / merged_data$TOTAL_GP_FTE

# Step 1: Extract unique ICB names
unique_icb_names <- unique(merged_data$ICB_NAME)

# Step 2: Add "Grand Total" at the beginning of the vector
icb_column <- c("Grand Total", unique_icb_names)

# Initialise the "Total number of patients" column with zeros
dataframe$"Total number of patients" <- numeric(length = length(icb_column))

# Calculate the grand total of patients
grand_total <- sum(merged_data$TOTAL_PATIENTS)

# Assign the grand total to the first row
dataframe$"Total number of patients"[1] <- grand_total

# Loop through the rest of the ICBs to calculate and assign the total number of patients
for(i in 2:length(icb_column)) {
  # Calculate the total number of patients for each ICB
  total <- sum(merged_data$TOTAL_PATIENTS[merged_data$ICB_NAME == icb_column[i]])
  # Assign the total to the corresponding row in the dataframe
  dataframe$"Total number of patients"[i] <- total
}

# Initialise the "Total number of patients" column with zeros
dataframe$"Total number of patients CR" <- numeric(length = length(icb_column))

# Calculate the grand total of patients
grand_total <- sum(merged_data$"Number.of.Weighted.Patients..Last.Known.Figure.")

# Assign the grand total to the first row
dataframe$"Total number of patients CR"[1] <- grand_total

# Loop through the rest of the ICBs to calculate and assign the total number of patients
for(i in 2:length(icb_column)) {
  # Calculate the total number of patients for each ICB
  total <- sum(merged_data$"Number.of.Weighted.Patients..Last.Known.Figure."[merged_data$ICB_NAME == icb_column[i]])
  # Assign the total to the corresponding row in the dataframe
  dataframe$"Total number of patients CR"[i] <- total
}

dataframe

colnames(merged_data)
