library(tidyverse)
library(WDI)
library(knitr)
library(ggplot2)
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

# Removing word Age from Age column values

NTA1$Age_Group <- gsub(pattern = "Age", replacement = "", x = NTA1$Age_Group)
NTA1$Age_Group <- as.numeric(NTA1$Age_Group)

#Multiply health spend values by conversion rate to give USD

NTA1[NTA1$Country == "United Kingdom" & NTA1$Variable.Name %in% c("Public Consumption, Health", "Private Consumption, Health"),]$Value <- NTA1[NTA1$Country == "United Kingdom" & NTA1$Variable.Name %in% c("Public Consumption, Health", "Private Consumption, Health"),]$Value/0.6396
NTA1[NTA1$Country == "Singapore" & NTA1$Variable.Name %in% c("Public Consumption, Health", "Private Consumption, Health"),]$Value <- NTA1[NTA1$Country == "Singapore" & NTA1$Variable.Name %in% c("Public Consumption, Health", "Private Consumption, Health"),]$Value*0.7993

# Remove rows for the UK where Age_Group is greater than 85 (SG only contains values up to 85)
NTA1 <- NTA1 %>%
  filter(!(Country == "United Kingdom" & Age_Group > 85))

NTA1 <- NTA1 %>%
  filter(!(Country == "Singapore" & Age_Group > 85))

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
NTAS$Age <- 0:85
pop_sing <- calculate_median_age(NTAS)
pop_sing

# Median age UK
NTAS <- NTA4[NTA4$Country == "United Kingdom" & NTA4$Variable.Name == "Population, Total",]
NTAS$Age <- 0:85
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
summary_table$Per_Capita_USD <- values

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

# graphs for comparison

# 1. Filter out the rows for the three mentioned variables.
subset_data <- NTA3[NTA3$Variable.Name %in% c("Public Consumption, Health", "Private Consumption, Health", "Public and Private, Health"),]

# 2. Duplicate these rows
new_data <- subset_data

# Singapore 
new_data$Age_Group <- gsub(pattern = "Age", replacement = "", x = new_data$Age_Group)
new_data$Age_Group <- as.numeric(new_data$Age_Group)
new_data_SG <- new_data[new_data$Country == "Singapore",]
new_data_UK <- new_data[new_data$Country == "United Kingdom",]
new_data_total <- new_data[new_data$Variable.Name == "Public and Private, Health", ]
# Set a font size
font_size <- 14

# Use a specific color palette (modify this based on your data categories)
ft_palette <- c("deepskyblue3", "darkorange2", "darkgrey")

# Singapore plot
sg_plot <- ggplot(new_data_SG, aes(x = Age_Group, y = Value, color = Variable.Name, group = Variable.Name)) +
  geom_line(size = 1) +
  scale_color_manual(values = ft_palette) +
  labs(
    title = "Health Expenditure 2013",
    x = "Age",
    y = "Value (USD)",
    color = "Expenditure Type"
  ) +
  theme_minimal(base_size = font_size) +
  theme(legend.position = c(0, 1), legend.justification = c(0, 1), panel.grid.minor = element_blank())

# UK plot
uk_plot <- ggplot(new_data_UK, aes(x = Age_Group, y = Value, color = Variable.Name)) +
  geom_line(size = 1) +
  scale_color_manual(values = ft_palette) +
  labs(
    title = "Health Expenditure 2013",
    x = "Age",
    y = "Expenditure (USD)",
    color = "Expenditure Type"
  ) +
  theme_minimal(base_size = font_size) +
  theme(legend.position = c(0, 1), legend.justification = c(0, 1), panel.grid.minor = element_blank())

# Total plot
total_plot <- ggplot(new_data_total, aes(x = Age_Group, y = Value, color = Country)) +
  geom_line(size = 1) +
  scale_color_manual(values = ft_palette) +
  labs(
    title = "Health Expenditure 2013",
    x = "Age",
    y = "Expenditure (USD)",
    color = "Country"
  ) +
  theme_minimal(base_size = font_size) +
  theme(legend.position = c(0, 1), legend.justification = c(0, 1), panel.grid.minor = element_blank())

# Print the plots
print(sg_plot)
print(uk_plot)
print(total_plot)
