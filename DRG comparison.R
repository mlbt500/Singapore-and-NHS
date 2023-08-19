# Load necessary libraries
library(readxl)
library(httr)
library(tidyverse)
library(knitr)
library(kableExtra)

# values

NEP_AUD_2018.19 <- 5012

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
Ward_A_S <- S_data_sheet[S_data_sheet$`Ward Type`== "Ward A",]
Ward_A_S <- Ward_A_S[,c("DRG", "DRG Description", "P50 Bill")]
colnames(Ward_A_S)

# Optionally, you can remove the temporary file
unlink(download_file)

# I had to download the AUS data manually.
A_data_sheet <-  read_excel("Round 23 NHCDC Report_2018.19.xlsx", sheet = 4) # https://www.ihacpa.gov.au/health-care/costing/national-hospital-cost-data-collection/national-hospital-cost-data-collection-public-sector
names <- A_data_sheet[6,]
A_data_sheet <- A_data_sheet[-c(1:6),]
colnames(A_data_sheet) <- names
AUS <- A_data_sheet[,c("product", "Cw", "Seps")]


