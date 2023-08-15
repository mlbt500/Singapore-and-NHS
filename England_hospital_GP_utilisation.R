library(httr)
library(readxl)
library("openxlsx")

#NHS England hospital admissions

# URL of the file
url <- "https://files.digital.nhs.uk/05/A594E3/hosp-epis-stat-admi-rep-tabs-2018-19-tab.xlsx"
output_file <- "hosp-epis-stat-admi-rep-tabs-2018-19-tab.xlsx"

# Download the file
download_status <- GET(url, write_disk(output_file, overwrite = TRUE))

if (http_status(download_status)$category == "Success") {
  cat("File downloaded successfully!\n")
} else {
  cat("Failed to download the file.\n")
}

# Read the third sheet into a data frame
hospital_admissions <- read_xlsx(output_file, sheet = 3)

# NHS England GP appointment estimates

# URL of the GP appointments file
url <- "https://files.digital.nhs.uk/E4/B08CB9/GP_APPT_Publication_December%20_2019.xlsx"
output_file_gp <- "GP_APPT_Publication_December_2019.xlsx"

# Download the file
download_status <- GET(url, write_disk(output_file_gp, overwrite = TRUE))

if (http_status(download_status)$category == "Success") {
  cat("File downloaded successfully!\n")
} else {
  cat("Failed to download the file.\n")
}

# Read the fourth sheet into the GP_appointments variable
gp_appointments <- read_xlsx(output_file_gp, sheet = 4)
gp_appointments <- gp_appointments[-c(1:4),]
gp_appointments
warnings()

#clean hospital admissions

hospital_admissions_cleaned <- hospital_admissions[-c(1:2),]
names <- hospital_admissions_cleaned[1,]
hospital_admissions_cleaned <- hospital_admissions_cleaned[-1,]
colnames(hospital_admissions_cleaned) <- names
hospital_admissions_cleaned <- as.data.frame(hospital_admissions_cleaned)
hospital_admissions_cleaned[,2] <- as.numeric(hospital_admissions_cleaned[, 2])
hospital_admissions_cleaned[,2] <- as.numeric(hospital_admissions_cleaned[, 3])
hospital_admissions_cleaned <- hospital_admissions_cleaned[-c(22:31),]
no.hospital_admissions

#clean GP appointments
GP_appointments
GP_appointments_cleaned <- GP_appointments[GP_appointments[,1] == "Estimated England total count of appointments",2]
no.GP_appoinments <- GP_appointments_cleaned[7,] # it outputs 7 rows for some reason but 24160000 is the correct figure
