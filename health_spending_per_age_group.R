library(dplyr)
library(tidyverse)
library(WDI)
library(knitr)
library(kableExtra)

# Reading CSV
NTA <- read.csv("NTA.csv")

# Filtering data for 2013, Singapore, and the United Kingdom
NTA_2013 <- NTA[NTA$Year == 2013 & NTA$Country %in% c("United Kingdom", "Singapore"),]

# Removing unwanted rows
NTA_2013 <- NTA_2013[-c(5:6),]

# Transforming to long format
NTA_2013_long <- NTA_2013 %>%
  pivot_longer(cols = starts_with("Age"),
               names_to = "Age_Group",
               values_to = "Value")

# Selecting required columns
NTA1 <- NTA_2013_long[,c("Country", "Year", "Variable.Name", "Age_Group", "Value")]
NTA1 <- NTA1[NTA1$Age_Group != "Age.Groups", ]

# Multiplying UK's population by 1000
NTA1$Value <- ifelse(NTA1$Country == "United Kingdom" & NTA1$Variable.Name == "Population, Total", NTA1$Value * 1000, NTA1$Value)

# Summing public and private health spending
relevant_data <- NTA1[NTA1$Variable.Name %in% c("Public Consumption, Health", "Private Consumption, Health"),]
sum_data <- relevant_data %>%
  group_by(Country, Age_Group, Year) %>%
  summarise(Value = sum(Value), .groups = 'drop')
sum_data$Variable.Name <- "Public and Private, Health"
NTA2 <- bind_rows(NTA1, sum_data)

# Calculating Health*Population
health_data <- NTA2[NTA2$Variable.Name == "Public and Private, Health",]
pop_data <- NTA2[NTA2$Variable.Name == "Population, Total",]
combined_data <- merge(health_data, pop_data, by = c("Country", "Age_Group", "Year"))
combined_data$Value <- combined_data$Value.x * combined_data$Value.y
combined_data$Variable.Name <- "Health*Population"
combined_data <- combined_data[, c("Country", "Year", "Variable.Name", "Age_Group", "Value")]
NTA3 <- bind_rows(NTA2, combined_data)

#Calculating Health*Singapore with United Kingdom Population
health_data <- NTA2[NTA2$Variable.Name == "Public and Private, Health" & NTA2$Country == "Singapore",]
health_data$Country <- NULL
pop_data <- NTA2[NTA2$Variable.Name == "Population, Total" & NTA2$Country == "United Kingdom",]
pop_data$Country <- NULL
combined_data <- merge(health_data, pop_data, by = c("Age_Group", "Year"))
combined_data$Value <- combined_data$Value.x * combined_data$Value.y
combined_data$Variable.Name <- "Health*Population"
combined_data <- combined_data[, c("Year", "Variable.Name", "Age_Group", "Value")]
combined_data$Country <- "Singapore with United Kingdom Population"
combined_data <- combined_data[,c("Country", "Year", "Variable.Name", "Age_Group", "Value")]
NTA4 <- bind_rows(NTA3, combined_data)

# Function to calculate median age
calculate_median_age <- function(data) {
  cumulative_pop <- cumsum(data$Value)
  half_pop <- sum(data$Value, na.rm = TRUE) / 2
  median_age <- NA
  
  for (i in 1:length(cumulative_pop)) {
    if (cumulative_pop[i] >= half_pop) {
      median_age <- data$Age[i]
      break
    }
  }
  return(median_age)
}

# Median age Singapore
NTAS <- NTA4[NTA4$Country == "Singapore" & NTA4$Variable.Name == "Population, Total",]
NTAS$Age <- 0:110
pop_sing <- calculate_median_age(NTAS)
pop_sing

# Median age UK
NTAS <- NTA4[NTA4$Country == "United Kingdom" & NTA4$Variable.Name == "Population, Total",]
NTAS$Age <- 0:110
pop_UK <- calculate_median_age(NTAS)
pop_UK


# Summary table 
summary_table <- data.frame("Country" = c("United Kingdom", "Singapore", "Singapore with United Kingdom Population"),
                            "Total_Population*Health" = c(sum(NTA4[NTA4$Country == "United Kingdom" & NTA4$Variable.Name == "Health*Population",]$Value, na.rm = TRUE),
                                                          sum(NTA4[NTA4$Country == "Singapore" & NTA4$Variable.Name == "Health*Population",]$Value, na.rm = TRUE),
                                                          sum(NTA4[NTA4$Country == "Singapore with United Kingdom Population" & NTA4$Variable.Name == "Health*Population",]$Value, na.rm = TRUE)),
                            "Population" = c(sum(NTA4[NTA4$Country == "United Kingdom" & NTA4$Variable.Name == "Population, Total",]$Value, na.rm = TRUE),
                                             sum(NTA4[NTA4$Country == "Singapore" & NTA4$Variable.Name == "Population, Total",]$Value, na.rm = TRUE),
                                             sum(NTA4[NTA4$Country == "United Kingdom" & NTA4$Variable.Name == "Population, Total",]$Value, na.rm = TRUE)))

# Summary table per capita spend (local)
values <- numeric()
for(i in 1:3){
  values <- c(values, summary_table[i, "Total_Population.Health"]/summary_table[i, "Population"])
  
}
summary_table$Per_Capita_Spend_Local <- values

# Summary table per capita spend(USD)
summary_table$Currency_Conversion <- c(1.5647, 0.7993, 0.7993)
values <- numeric()
for(i in 1:3){
  USD <- summary_table[i,"Per_Capita_Spend_Local"] * summary_table[i, "Currency_Conversion"]
  values <- c(values, USD)
}
summary_table$Per_Capita_Spend_USD <- values

# Add median age
summary_table$Median_Age <- c(pop_UK, pop_sing, pop_UK)

# World Bank data
# List of countries to filter
countries_of_interest <- c("GB", "SG")

# Fetching the necessary data
Population <- WDI(indicator = "SP.POP.TOTL", country = countries_of_interest)
Health_per_capita_USD <- WDI(indicator = "SH.XPD.CHEX.PC.CD", country = countries_of_interest)

# Merging Population and Health_per_capita_USD
final_data <- merge(Population, Health_per_capita_USD, by = c("iso2c", "year"))

# Selecting relevant columns and renaming
final_data <- final_data %>%
  select(country = iso2c, year, Population = SP.POP.TOTL, health_care_per_capita_USD = SH.XPD.CHEX.PC.CD)

# Filter for all countries in 2013
data_2013 <- final_data[final_data$year == 2013,]
WB_pop <- data_2013$Population
WB_spend <- data_2013$health_care_per_capita_USD

#merge with summary table

summary_table$World_Bank_Population <- c(WB_pop, NA)
summary_table$World_Bank_Per_Capita_Health_Spend_USD <- c(WB_spend, NA)

# export html

# Create the main table
table_html <- summary_table %>%
  kable("html", escape = FALSE) %>%
  kable_styling(full_width = FALSE, position = "center") %>%
  row_spec(row = 0, extra_css = "border-bottom: 3px solid black; font-weight: bold;") %>%
  column_spec(column = 2:7, background = "lightgrey", border_right = TRUE, border_left = TRUE)

# Create a key table
key_table <- data.frame(
  Color = "Light Grey",
  Meaning = "National Transfer Accounts Data"
)

key_html <- key_table %>%
  kable("html", escape = FALSE) %>%
  kable_styling(full_width = FALSE, position = "center") %>%
  row_spec(row = 1, background = "lightgrey")

# Combine the main table and the key
final_html <- paste0(
  table_html,
  '<br><br>',
  "<strong>Key:</strong>",
  key_html
)

# Save the combined HTML to a file
writeLines(final_html, "summary_table_with_key.html")

