library(sf)

# Correcting the column names based on your dataframe structure
dataframe$Percentage_of_Patients_in_High_Load_Practices <- dataframe$Patients_in_High_Load_Practices / dataframe$"Total number of patients"
dataframe$Percentage_of_Patients_in_High_Carr_Hill_Load_Practices <- dataframe$Patients_in_High_Carr_Hill_Load_Practices / dataframe$"Total number of patients CR"

# Ensure we're correctly referencing the column names with spaces by using backticks or quotes as nee
icb_shape <- st_read("ICB_APR_2023_EN_BGC.shp")

# Replace "ICB" with "Integrated Care Board" in the dataframe's ICB column
dataframe$ICB <- gsub("ICB$", "Integrated Care Board", dataframe$ICB)

# Merge the datasets using the updated ICB column in dataframe
merged_data <- merge(icb_shape, dataframe, by.x = "ICB23NM", by.y = "ICB", all.x = TRUE)
