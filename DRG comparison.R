# Load necessary libraries
library(readxl)
library(httr)
library(tidyverse)
library(knitr)
library(kableExtra)

# values
# the data for both Australia and Singapore is 1 July to 30 June
# For July 2018 to 2019 I used

# Australia
# Define the URL
url <- "https://www.rba.gov.au/statistics/tables/xls-hist/2018-2022.xls"

# Create a temporary file
temp_file <- tempfile(fileext = ".xls")

# Download the file into the temporary file
download.file(url, temp_file, mode = "wb")

# Load the data into R from the temporary file
# Note: This will load the first sheet by default. You can specify other sheets using the 'sheet' argument.
data <- read_excel(temp_file)
data <- data[-c(1:9),1:2]
names <- data[1,]
colnames(data) <- names
data <- data[-1,]
data$`Series ID` <- as.numeric(data$`Series ID`)
data$FXRUSD <- as.numeric(data$FXRUSD)
data$Converted_Date <- as.Date(as.numeric(data$`Series ID`), origin = "1899-12-30")
filtered_data <- data %>%
  dplyr::filter(Converted_Date >= as.Date("2018-07-01") & Converted_Date <= as.Date("2019-06-30"))
AUD_to_USD <- mean(filtered_data$FXRUSD)

#Singapore exchange rate
SGER <- read_excel("Exchange Rates.xlsx") # https://eservices.mas.gov.sg/Statistics/msb/ExchangeRates.aspx
SGER <- SGER[-c(1:5), -c(1:3),]
names <- SGER[1,]
SGER <- SGER[-1,]
colnames(SGER) <- names
SGER$`S$ Per Unit of US Dollar` <- as.numeric(SGER$`S$ Per Unit of US Dollar`)
SGD_to_USD <-1/mean(SGER$`S$ Per Unit of US Dollar`, na.rm = TRUE)

# Specify the URL for the Excel file -- MoH
url <- "https://www.moh.gov.sg/docs/librariesprovider5/default-document-library/fee-publication-data-july18---june19-(for-download)_25-feb-2020.xlsx" 

# Create a temporary file to store the downloaded content
download_file <- tempfile(fileext = ".xlsx")

# Download the file from the specified URL
GET(url, write_disk(download_file))

# Get the names of all the sheets in the Excel file
sheets <- excel_sheets(download_file)

# Read the five sheets into separate data frames
S_data_sheet <- read_excel(download_file, sheet = sheets[4])
names <- S_data_sheet[3,]
S_data_sheet[3,]
S_data_sheet <- S_data_sheet[-c(1:3),]
colnames(S_data_sheet) <- names
unique(S_data_sheet$`Ward Type`)
Ward_A_SG <- S_data_sheet[S_data_sheet$`Ward Type`== "Ward A",]
Ward_A_SG <- Ward_A_S[,c("DRG", "DRG Description", "P50 Bill")]
colnames(Ward_A_SG)
Ward_A_SG$"P50 Bill, USD 2019, July 2018-19" <- as.numeric(Ward_A_SG$"P50 Bill")*SGD_to_USD_2019

# Optionally, you can remove the temporary file
unlink(download_file)

# I had to download the AUS data manually.
A_data_sheet <-  read_excel("Round 23 NHCDC Report_2018.19.xlsx", sheet = 4) # https://www.ihacpa.gov.au/health-care/costing/national-hospital-cost-data-collection/national-hospital-cost-data-collection-public-sector
names <- A_data_sheet[6,]
A_data_sheet <- A_data_sheet[-c(1:6),]
colnames(A_data_sheet) <- names
AUS <- A_data_sheet[,c("product", "Cw", "Seps")]
AUS$"Mean Unit Cost (AUD) 2018-19 FY" <- as.numeric(AUS$Cw)*NEP_AUD_2018.19
AUS$"Mean Unit Cost (USD 2018, 18-19 FY)" <- AUS$"Mean Unit Cost (AUD) 2018-19 FY" * AUD_to_USD_2018

#Compare Singapore and AUS

# Extracting and transforming the relevant columns from AUS dataset
AUS_selected <- AUS %>%
  select(DRG = product,
         `Unit Cost for Australia in USD` = `Mean Unit Cost (USD 2018, 18-19 FY)`,
         `Seps AUS` = Seps)

# Extracting and transforming the relevant columns from Ward_A_SG dataset
SG_selected <- Ward_A_SG %>%
  select(DRG, 
         `DRG Description`,
         `Unit Cost for Singapore in USD` = `P50 Bill, USD 2019, July 2018-19`)

# Merging the two datasets on DRG ensuring only common DRGs are included
final_table <- AUS_selected %>%
  inner_join(SG_selected, by = "DRG") %>%
  select(DRG, `DRG Description`, `Unit Cost for Australia in USD`, `Unit Cost for Singapore in USD`, `Seps AUS`)

# Seps column coverted to numeric
final_table$`Seps AUS` <- as.numeric(final_table$`Seps AUS`)

# Adjust for Singaporean GST

final_table$`Unit Cost for Singapore in USD` <- final_table$`Unit Cost for Singapore in USD`*1/1.07

# Calculate the product of Unit Cost and Seps for Australia
final_table$AUS_total_cost <- final_table$`Unit Cost for Australia in USD` * final_table$`Seps AUS`

# Calculate the product of Unit Cost and Seps for Singapore
final_table$SG_total_cost <- final_table$`Unit Cost for Singapore in USD` * final_table$`Seps AUS`

# Calculate the weighted average unit cost for Australia
AUS_weighted_avg_cost <- sum(final_table$AUS_total_cost) / sum(final_table$`Seps AUS`)

# Calculate the weighted average unit cost for Singapore
SG_weighted_avg_cost <- sum(final_table$SG_total_cost) / sum(final_table$`Seps AUS`)

# Divide the weighted average unit cost of Singapore by that of Australia
ratio <- SG_weighted_avg_cost / AUS_weighted_avg_cost

ratio

#output data
# Convert final_table to an HTML table with styling
final_table_html <- final_table %>%
  kable(format = "html", table.attr = "class='table table-striped'") %>%
  kable_styling()

# Save the HTML table to a file
writeLines(final_table_html, "final_table.html")

# Convert final_data to an HTML table with styling (assuming final_data is already defined in your script)
final_data_html <- final_data %>%
  kable(format = "html", table.attr = "class='table table-striped'") %>%
  kable_styling()

# Save the HTML table to a file
writeLines(final_data_html, "final_data.html")


