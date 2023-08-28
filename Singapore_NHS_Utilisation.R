library(dplyr)
library(stringr)
library(tidyr)
library(WDI)
library(readxl)
library(rvest)
library(httr)

url <- "https://www.moh.gov.sg/resources-statistics/singapore-health-facts/admissions-and-outpatient-attendances"
SG <- read_html(url)
table <- SG %>%
  html_table(fill = TRUE) %>%
  .[[1]]

#name columns
names(table) <- c("Values", "2018", "2019", "2020")
table1 <- table[-1,]

#filter out unnecessary row
table1 <- table1[c(2:3, 50:51, 98:99, 147:148, 151:154, 157:158, 161, 166:169),]

#clean values column
table1 <- table1 %>%
  mutate(Values = str_replace_all(Values, "\t", "")) %>%
  mutate(Values = str_replace_all(Values, "\n[0-9]+", "")) %>%
  mutate(Values = str_replace_all(Values, "►", "")) %>%
  mutate(Values = str_trim(Values))

#some variables were over two rows -- I simplified them
table1[2,1] <- table1[1,1]
table1[4,1] <- table1[3,1]
table1[6,1] <- table1[5,1]
table1 <- table1[-c(1,3,5),]
table1[6,1]
#make columns 2 to 4 numeric
table1 <- table1 %>%
  mutate_at(vars(`2018`, `2019`, `2020`), ~str_replace_all(., ",", ""))
table1 <- table1 %>%
  mutate(`2018` = as.numeric(`2018`),
         `2019` = as.numeric(`2019`),
         `2020` = as.numeric(`2020`))

#mutiple certain rows by 1000 so they are all the same units
table2 <- table1
table2[13:16, 2:4] <- table2[13:16, 2:4] * 1000
table2 <- table2 %>%
  mutate(Values = str_extract(Values, "^[^\\(]+"))

#final estimates
GP_estimate <- data.frame(table2[15,])
total_gp_estimate_2018 <- as.numeric(sum(GP_estimate[, 2] * 5))
GP_estimate[, 2:4] <- GP_estimate[, 2:4] * 4
GP_estimate[1, 1] <- "Private GP estimate"
colnames(GP_estimate) <- names(table2)
Singapore_health_utilisation <- rbind(table2, GP_estimate)
hospital_admissions_SG <- as.numeric(sum(Singapore_health_utilisation[1:3,2]))
A_and_E_visits_SG <- as.numeric(table2[13,2])

#population and per capita
Population <- WDI(indicator = "SP.POP.TOTL", country = c("AU", "SG"), start = 2018, end = 2018)
Population_SG_WB <- as.numeric(Population[2,5])
Population_SG_OECD <- 3.994283*10^6 # https://data.oecd.org/pop/population.htm

GP_visits_per_capita_WB <- total_gp_estimate_2018 /Population_SG_WB
hospital_admissions_per_capita_WB <- hospital_admissions_SG/Population_SG_WB
A_and_E_visits_per_capita_WB <- A_and_E_visits_SG/Population_SG_WB

GP_visits_per_capita_OECD <- total_gp_estimate_2018 /Population_SG_OECD
hospital_admissions_per_capita_OECD <- hospital_admissions_SG/Population_SG_OECD
A_and_E_visits_per_capita_OECD <- A_and_E_visits_SG/Population_SG_OECD

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
hospital_admissions1 <- read_xlsx(output_file, sheet = 10)
hospital_admissions1 <- hospital_admissions1[-c(1:3, 16:25),-c(4:5)]
names <- hospital_admissions1[1,]
colnames(hospital_admissions1) <- names
hospital_admissions1 <- hospital_admissions1[-1,]
FCE_IP_2018_9 <- as.numeric(hospital_admissions1[11,2])

hospital_admissions2 <- read_xlsx(output_file, sheet = 3)
hospital_admissions2 <- hospital_admissions2[-c(1:2, 25:34),]
names <- hospital_admissions2[1,] 
colnames(hospital_admissions2) <- names
hospital_admissions2 <- hospital_admissions2[-1,]
hospital_admissions2$FCEs <- as.numeric(hospital_admissions2$FCEs)
hospital_admissions2$FAEs <- as.numeric(hospital_admissions2$FAEs)
hospital_admissions2_18_9 <- hospital_admissions2[hospital_admissions2$Year == "2018-19",]
difference_FCE_FAE <- as.numeric(hospital_admissions2_18_9[,2]- hospital_admissions2_18_9[,3])
adm_2018.2019  <- FCE_IP_2018_9 - difference_FCE_FAE

# Check whether this assumption is correct
hospital_admissions1 <- read_excel("hosp-epis-stat-admi-rep-tabs-2020-21-tab-v2.xlsx", sheet = 3)
FCE19_20 <- as.numeric(hospital_admissions1[25,2])
FAE19_20 <- as.numeric(hospital_admissions1[25,3])
difference <- FCE19_20 - FAE19_20
hospital_admissions2 <- read_excel("hosp-epis-stat-admi-rep-tabs-2020-21-tab-v2.xlsx", sheet = 10)
hospital_admissions2 <- hospital_admissions2[-c(1:3), -c(4:5)]
FCE_IP <- as.numeric(hospital_admissions2[11,2])
FAE_IP_E <- FCE_IP - difference
hospital_admissions3 <- read_excel("hosp-epis-stat-admi-rep-tabs-2020-21-tab-v2.xlsx", sheet = 19)
hospital_admissions3 <- hospital_admissions3[-c(1:2),]
names <- hospital_admissions3[1,]
colnames(hospital_admissions3) <- names
hospital_admissions3 <- hospital_admissions3[-1,]
admissions <- as.numeric(hospital_admissions3$"Ordinary")
FAE_IP <- sum(admissions[1:53])
(FAE_IP_E- FAE_IP)/FAE_IP # FAE_IP_E and FAE are roughly equivalent

admissions <- hospital_admissions3$"Ordinary"
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


#clean GP appointments
gp_appointments <- read_xlsx(output_file_gp, sheet = 4, col_names = FALSE, col_types = "text")
gp_appointments <- gp_appointments[-c(1:3),]
gp_appointments[1,2] <- NA
date_values <- as.Date(as.numeric(gp_appointments[1,]), origin = "1899-12-30")
names(gp_appointments) <- date_values
gp_appointments <- gp_appointments[-c(1:2),]
gp_appointments[, 3:20] <- lapply(gp_appointments[, 3:20], function(col) as.numeric(as.character(col)))
gp_appointments
total_no_gp_app <- sum(gp_appointments[11,3:14])

#NHS England A&E
# Define the URL of the file you want to download
url <- "https://files.digital.nhs.uk/DB/1CED9F/AE1819_Summary_Report_Tables.xlsx"

# Download the file to a temporary location
tmp_file <- tempfile(fileext = ".xlsx")
download.file(url, tmp_file, mode = "wb")

# Load the desired sheet from the Excel file into R
A_and_E <- read_xlsx(tmp_file, sheet = 3)

# Clean
A_and_E_cleaned <- as.data.frame(A_and_E[-c(1:3,6, 6:23),])
A_and_E_cleaned <- A_and_E_cleaned[,-1]
NAMES <- A_and_E_cleaned[1,]
colnames(A_and_E_cleaned) <- NAMES
A_and_E_cleaned <- A_and_E_cleaned[-1,]
rownames(A_and_E_cleaned) <- "MSitAE"
A_and_E_2018.19 <- as.numeric(A_and_E_cleaned[,10])

#population of England 2019
England_population_2018 <- 55977000 # https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationprojections/bulletins/subnationalpopulationprojectionsforengland/2018based#:~:text=The%20population%20of%20England%20is,2018%2Dbased%20national%20population%20projections.
England_hos_visits_per_capita <- adm_2018.2019/England_population_2018
England_A_and_E_per_capita <- A_and_E_2018.19/England_population_2018
England_GP_visits_per_capita <- total_no_gp_app/England_population_2018

#Australia -- utilisation figures https://www.aihw.gov.au/getmedia/c14c8e7f-70a3-4b00-918c-1f56d0bd9414/aihw-hse-247.pdf.aspx?inline=true
Australia_population_2019 <- as.numeric(Population[1,5])
Hospitalisations <- 11.5 *10^6



#organise data frame

Singapore_WB <- data.frame("Source" = "Singapore MoH, World Bank Population", 
                        "Hospital visits per capita" = hospital_admissions_per_capita_WB, 
                        "GP visits per capita" = GP_visits_per_capita_WB,
                        "A&E visits per capita" = A_and_E_visits_per_capita_WB) 

Singapore_OECD <- data.frame("Source" = "Singapore MoH, OECD", 
                             "Hospital visits per capita" = hospital_admissions_per_capita_OECD, 
                             "GP visits per capita" = GP_visits_per_capita_OECD,
                             "A&E visits per capita" = A_and_E_visits_per_capita_OECD) 

England <- data.frame("Source" = "NHS England", 
                      "Hospital visits per capita" = England_hos_visits_per_capita, 
                      "GP visits per capita" = England_GP_visits_per_capita,
                      "A&E visits per capita" = England_A_and_E_per_capita)

Australia <- data.frame("Source" = "Australian Institute of Health and Welfare")

summary_table <- rbind(Singapore_WB, Singapore_OECD, England)

comparison_WB <- data.frame("Source" = "England/Singapore, World Bank Population", 
                      "Hospital visits per capita" = England_hos_visits_per_capita/hospital_admissions_per_capita_WB, 
                      "GP visits per capita" = England_GP_visits_per_capita/GP_visits_per_capita_WB,
                      "A&E visits per capita" = England_A_and_E_per_capita/A_and_E_visits_per_capita_WB)

comparison_OECD <- data.frame("Source" = "England/Singapore, OECD Population", 
                         "Hospital visits per capita" = England_hos_visits_per_capita/hospital_admissions_per_capita_OECD, 
                         "GP visits per capita" = England_GP_visits_per_capita/GP_visits_per_capita_OECD,
                         "A&E visits per capita" = England_A_and_E_per_capita/A_and_E_visits_per_capita_OECD)
summary_table <- rbind(summary_table, comparison_WB, comparison_OECD)
summary_table