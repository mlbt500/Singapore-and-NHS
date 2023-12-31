library(rvest)
url <- "https://www.moh.gov.sg/resources-statistics/singapore-health-facts/admissions-and-outpatient-attendances"
SG <- read_html(url)
table <- SG %>%
  html_table(fill = TRUE) %>%
  .[[1]]