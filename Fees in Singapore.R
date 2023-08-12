# Load necessary libraries
library(readxl)
library(httr)
library(dplyr)
library(tidyverse)
library(knitr)
library(kableExtra)

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

#data_sheet2
total_bills <- data_sheet2[-c(1:2),]
names <- total_bills[1,]
colnames(total_bills) <- names
total_bills <- total_bills[-1,]
filtered_bills <- total_bills %>%
  filter(grepl("knee", `TOSP Description`, ignore.case = TRUE))
unique(filtered_bills$Setting)
filtered_bills <- filtered_bills[filtered_bills$Setting %in% c("Public Hospitals/ Centres (Subsidised)","Public Hospitals/ Centres (Unsubsidised)"),]
filtered_bills <- filtered_bills[,c(1,2,6,8)]
new_data_frame <- filtered_bills %>%
  select(`TOSP Code`, `TOSP Description`, `Ward Type`, `P50 Bill`) %>%
  pivot_wider(names_from = `Ward Type`, values_from = `P50 Bill`)
expand_descriptors <- function(description) {
  if (grepl("Arthroscopy", description)) return("A minimally invasive procedure to diagnose and treat joint issues.")
  if (grepl("Ligament Reconstruction", description)) return("Surgical reconstruction or repair of damaged ligaments.")
  if (grepl("Meniscus Repair", description)) return("Surgery to fix a torn knee meniscus.")
  if (grepl("MIS", description)) return("Procedures performed through small incisions.")
  if (grepl("Total Joint Replacement", description)) return("Replacement of joint surfaces with artificial components.")
  if (grepl("Osteotomy", description)) return("Surgical cutting and realignment of bone.")
  if (grepl("Percutaneous Angioplasty", description)) return("Procedure to open narrowed blood vessels.")
  if (grepl("Arthrotomy", description)) return("Surgical incision into a joint.")
  if (grepl("Synovectomy", description)) return("Removal of inflamed joint tissue.")
  return("Other procedure.") # Default description
}
new_data_frame <- new_data_frame %>%
  rowwise() %>%
  mutate(`expanded descriptors` = expand_descriptors(`TOSP Description`)) %>%
  ungroup()
new_data_frame <- new_data_frame %>%
  select(`TOSP Code`, `TOSP Description`, `expanded descriptors`, everything())
final_data_frame <- new_data_frame %>%
  filter(grepl("total", `TOSP Description`, ignore.case = TRUE))
numeric_cols <- sapply(final_data_frame, is.numeric)
final_data_frame[numeric_cols] <- lapply(final_data_frame[numeric_cols], function(x) round(x, digits = 3))

# Create HTML table using kable
html_table <- final_data_frame %>%
  kable("html", escape = FALSE) %>%
  kable_styling(full_width = F, position = "center") %>%
  row_spec(0, bold = TRUE, extra_css = "border-bottom: 2px solid black; border-top: 2px solid black;") %>%
  row_spec(1:nrow(final_data_frame), extra_css = "border-bottom: 2px solid black; border-top: 2px solid black;") %>%
  column_spec(1:ncol(final_data_frame), border_left = TRUE, border_right = TRUE, extra_css = "border-left: 2px solid black; border-right: 2px solid black;")

# Write HTML table to file
writeLines(html_table, "final_data_frame.html")
