library(httr)
library(readxl)
library("openxlsx")
library(WDI)
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
gp_appointments <- read_xlsx(output_file_gp, sheet = 4, col_names = FALSE, col_types = "text")
gp_appointments <- gp_appointments[-c(1:3),]
gp_appointments[1,2] <- NA
unique(gp_appointments[1,])
date_values <- as.Date(as.numeric(gp_appointments[1,]), origin = "1899-12-30")
names(gp_appointments) <- date_values
gp <- appointments <- gp_appointments[-1,]

#hospital admissions

hospital_admissions_cleaned <- hospital_admissions[-c(1:2),]
names <- hospital_admissions_cleaned[1,]
hospital_admissions_cleaned <- hospital_admissions_cleaned[-1,]
colnames(hospital_admissions_cleaned) <- names
hospital_admissions_cleaned <- as.data.frame(hospital_admissions_cleaned)
hospital_admissions_cleaned[,2] <- as.numeric(hospital_admissions_cleaned[, 2])
hospital_admissions_cleaned[,2] <- as.numeric(hospital_admissions_cleaned[, 3])
hospital_admissions_cleaned <- hospital_admissions_cleaned[-c(22:31),]
hospital_admissions_cleaned[, 2:3] <- lapply(hospital_admissions_cleaned[, 2:3], function(col) as.numeric(as.character(col)))
adm_2018.2019 <- hospital_admissions_cleaned[21,3]

#clean GP appointments
gp_appointments <- read_xlsx(output_file_gp, sheet = 4, col_names = FALSE, col_types = "text")
gp_appointments <- gp_appointments[-c(1:3),]
gp_appointments[1,2] <- NA
date_values <- as.Date(as.numeric(gp_appointments[1,]), origin = "1899-12-30")
names(gp_appointments) <- date_values
gp_appointments <- gp_appointments[-c(1:2),]
gp_appointments[, 3:20] <- lapply(gp_appointments[, 3:20], function(col) as.numeric(as.character(col)))
gp_appointments
total_no_gp_app_2019 <- sum(gp_appointments[11,3:14])

#population of England 2019
England_population_2019 <- 56.3 * 10^6 # source ONS:https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/articles/overviewoftheukpopulation/january2021
England_GP_app_per_capita_2019 <- total_no_gp_app_2019/England_population_2019
England_hos_visits_per_capita <- adm_2018.2019/England_population_2019
England_hos_visits_per_capita
