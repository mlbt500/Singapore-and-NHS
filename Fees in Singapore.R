# Load necessary libraries
library(readxl)
library(httr)

# Specify the URL for the Excel file
url <- "https://www.moh.gov.sg/docs/librariesprovider5/excel-uploads/fee-publication-data-jan21---dec21-(for-download)(1).xlsx?sfvrsn=e5d6a7db_2" 

# Create a temporary file to store the downloaded content
download_file <- tempfile(fileext = ".xlsx")

# Download the file from the specified URL
GET(url, write_disk(download_file))

# Get the names of all the sheets in the Excel file
sheets <- excel_sheets(download_file)

# Read the five sheets into separate data frames
data_sheet1 <- read_excel(download_file, sheet = sheets[1])
data_sheet2 <- read_excel(download_file, sheet = sheets[2])
data_sheet3 <- read_excel(download_file, sheet = sheets[3])
data_sheet4 <- read_excel(download_file, sheet = sheets[4])
data_sheet5 <- read_excel(download_file, sheet = sheets[5])

# Optionally, you can remove the temporary file
unlink(download_file)