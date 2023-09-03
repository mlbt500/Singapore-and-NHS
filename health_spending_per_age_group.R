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

# Calculating PPP adjusted health spending

# Create duplicated rows
duplicated_rows <- filter(NTA1, Variable.Name %in% c("Public Consumption, Health", "Private Consumption, Health"))
duplicated_rows$Variable.Name <- paste0(duplicated_rows$Variable.Name, " PPP")

# Adjust values for Singapore
duplicated_rows <- mutate(duplicated_rows,
                          Value = ifelse(Country == "Singapore", 
                                         Value * (1.251/0.859), 
                                         Value))

# Adjust values for the UK
duplicated_rows <- mutate(duplicated_rows,
                          Value = ifelse(Country == "United Kingdom", 
                                         Value * (0.695/0.640), 
                                         Value))

# Bind back to original tibble
NTA1_updated <- bind_rows(NTA1, duplicated_rows)

# Summing public and private health spending
relevant_data <- NTA1_updated[NTA1_updated$Variable.Name %in% c("Public Consumption, Health", "Private Consumption, Health"),]
sum_data <- relevant_data %>%
  group_by(Country, Age_Group, Year) %>%
  summarise(Value = sum(Value), .groups = 'drop')
sum_data$Variable.Name <- "Public and Private, Health"
NTA2 <- bind_rows(NTA1_updated, sum_data)

# Filter for the newly created PPP-adjusted values
relevant_ppp_data <- filter(duplicated_rows, Variable.Name %in% c("Public Consumption, Health PPP", "Private Consumption, Health PPP"))

# Sum these PPP-adjusted values
sum_ppp_data <- relevant_ppp_data %>%
  group_by(Country, Age_Group, Year) %>%
  summarise(Value = sum(Value), .groups = 'drop')
sum_ppp_data$Variable.Name <- "Public and Private, Health PPP"

# Now bind these aggregated PPP-adjusted rows back to NTA2 (since NTA2 already contains the original NTA1 and summed data for Public and Private Health)
NTA2 <- bind_rows(NTA2, sum_ppp_data)

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

# PPP adjusted Health*Population

health_data <- NTA2[NTA2$Variable.Name == "Public and Private, Health PPP",]
pop_data <- NTA2[NTA2$Variable.Name == "Population, Total",]
combined_data <- merge(health_data, pop_data, by = c("Country", "Age_Group", "Year"))
combined_data$Value <- combined_data$Value.x * combined_data$Value.y
combined_data$Variable.Name <- "Health*Population PPP"
combined_data <- combined_data[, c("Country", "Year", "Variable.Name", "Age_Group", "Value")]
NTA5 <- bind_rows(NTA4, combined_data)

# PPP adjsuted Health*Population Singapore with UK population
# Filter Singapore's Health PPP data
health_data <- NTA5 %>% 
  filter(Variable.Name == "Public and Private, Health PPP" & Country == "Singapore") %>%
  select(-Country)

# Filter UK's Population data
pop_data <- NTA5 %>%
  filter(Variable.Name == "Population, Total" & Country == "United Kingdom") %>%
  select(-Country)

# Merge the datasets
combined_data <- merge(health_data, pop_data, by = c("Age_Group", "Year"))

# Compute new value
combined_data$Value <- combined_data$Value.x * combined_data$Value.y

# Update variable name
combined_data$Variable.Name <- "Health*Population PPP"
combined_data$Country <- "Singapore with United Kingdom Population"
combined_data <- combined_data[, c("Country", "Year", "Variable.Name", "Age_Group", "Value")]

# Combine with NTA5
NTA6 <- bind_rows(NTA5, combined_data)

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
NTAS <- NTA6[NTA6$Country == "Singapore" & NTA6$Variable.Name == "Population, Total",]
NTAS$Age <- 0:85
pop_sing <- calculate_median_age(NTAS)
pop_sing

# Median age UK
NTAUK <- NTA6[NTA6$Country == "United Kingdom" & NTA6$Variable.Name == "Population, Total",]
NTAUK$Age <- 0:85
pop_UK <- calculate_median_age(NTAUK)
pop_UK

NTAP <- NTA6[NTA6$Variable.Name == "Population, Total",]

# Filter the data for Singapore
SG_data <- NTAP[NTAP$Country == "Singapore",]

# Calculate the total population for Singapore
total_pop <- sum(SG_data$Value, na.rm = TRUE)

# Calculate the population for age group 15-64
pop_15_64 <- sum(SG_data$Value[SG_data$Age_Group >= 15 & SG_data$Age_Group <= 64], na.rm = TRUE)

# Calculate the population for age group 65 and above
pop_65_plus <- sum(SG_data$Value[SG_data$Age_Group >= 65], na.rm = TRUE)

# Calculate percentages
perc_15_64 <- (pop_15_64 / total_pop) * 100
perc_65_plus <- (pop_65_plus / total_pop) * 100

# Calculate the population for age group 0-14
pop_0_14 <- sum(SG_data$Value[SG_data$Age_Group >= 0 & SG_data$Age_Group <= 14], na.rm = TRUE)

# Calculate percentages
perc_0_14 <- (pop_0_14 / total_pop) * 100

# Return the calculated percentages for all age groups
list(Percentage_0_to_14 = perc_0_14, Percentage_15_to_64 = perc_15_64, Percentage_65_plus = perc_65_plus)
# Filter the data for the United Kingdom
UK_data <- NTAP[NTAP$Country == "United Kingdom",]

# Calculate the total population for the United Kingdom
total_pop_UK <- sum(UK_data$Value, na.rm = TRUE)

# Calculate the population for age group 0-14
pop_0_14_UK <- sum(UK_data$Value[UK_data$Age_Group >= 0 & UK_data$Age_Group <= 14], na.rm = TRUE)

# Calculate the population for age group 15-64
pop_15_64_UK <- sum(UK_data$Value[UK_data$Age_Group >= 15 & UK_data$Age_Group <= 64], na.rm = TRUE)

# Calculate the population for age group 65 and above
pop_65_plus_UK <- sum(UK_data$Value[UK_data$Age_Group >= 65], na.rm = TRUE)

# Calculate percentages for each age group
perc_0_14_UK <- (pop_0_14_UK / total_pop_UK) * 100
perc_15_64_UK <- (pop_15_64_UK / total_pop_UK) * 100
perc_65_plus_UK <- (pop_65_plus_UK / total_pop_UK) * 100

# Return the calculated percentages for all age groups for the United Kingdom
list(UK_Percentage_0_to_14 = perc_0_14_UK, UK_Percentage_15_to_64 = perc_15_64_UK, UK_Percentage_65_plus = perc_65_plus_UK)

# Compute the summary table using dplyr for NTA6
# Extract UK's total population from NTA6
uk_population <- NTA6 %>%
  filter(Country == "United Kingdom", Variable.Name == "Population, Total") %>%
  pull(Value) %>%
  sum(na.rm = TRUE)

# Compute the summary table using dplyr for NTA6
summary_table <- NTA6 %>%
  filter(Variable.Name %in% c("Health*Population", "Population, Total", "Health*Population PPP")) %>%
  group_by(Country, Variable.Name) %>%
  summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Variable.Name, values_from = Value) %>%
  mutate(Health_Per_Capita = ifelse(Country == "Singapore with United Kingdom Population",
                                    `Health*Population` / uk_population,
                                    `Health*Population` / `Population, Total`),
         Health_Per_Capita_PPP = ifelse(Country == "Singapore with United Kingdom Population",
                                        `Health*Population PPP` / uk_population,
                                        `Health*Population PPP` / `Population, Total`),
         Median_Age = case_when(
           Country == "United Kingdom" ~ pop_UK,
           Country == "Singapore" ~ pop_sing,
           TRUE ~ pop_UK
         ))


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

# Merge with summary table
summary_table <- summary_table %>%
  mutate(World_Bank_Population = ifelse(Country == "United Kingdom", WB_pop[1],
                                        ifelse(Country == "Singapore", WB_pop[2], NA)),
         World_Bank_Per_Capita_Health_Spend_USD = ifelse(Country == "United Kingdom", WB_spend[1],
                                                         ifelse(Country == "Singapore", WB_spend[2], NA)))



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

# 1. Filter out the rows for the three mentioned variables with PPP adjustments.
subset_data <- NTA6[NTA6$Variable.Name %in% c("Public Consumption, Health PPP", "Private Consumption, Health PPP", "Public and Private, Health PPP"),]

# 2. Duplicate these rows
new_data <- subset_data

# Singapore 
new_data$Age_Group <- gsub(pattern = "Age", replacement = "", x = new_data$Age_Group)
new_data$Age_Group <- as.numeric(new_data$Age_Group)
new_data_SG <- new_data[new_data$Country == "Singapore",]
new_data_UK <- new_data[new_data$Country == "United Kingdom",]
new_data_total <- new_data[new_data$Variable.Name == "Public and Private, Health PPP", ]

# Set a font size
font_size <- 14

# Use a specific color palette
ft_palette <- c("deepskyblue3", "darkorange2", "darkgrey")

# Singapore plot
sg_plot <- ggplot(new_data_SG, aes(x = Age_Group, y = Value, color = Variable.Name, group = Variable.Name)) +
  geom_line(size = 1) +
  scale_color_manual(values = ft_palette) +
  labs(
    title = "Health Expenditure (PPP adjusted) 2013",
    x = "Age",
    y = "Value (USD PPP)",
    color = "Expenditure Type"
  ) +
  theme_minimal(base_size = font_size) +
  theme(legend.position = c(0, 1), legend.justification = c(0, 1), panel.grid.minor = element_blank())

# UK plot
uk_plot <- ggplot(new_data_UK, aes(x = Age_Group, y = Value, color = Variable.Name)) +
  geom_line(size = 1) +
  scale_color_manual(values = ft_palette) +
  labs(
    title = "Health Expenditure (PPP adjusted) 2013",
    x = "Age",
    y = "Expenditure (USD PPP)",
    color = "Expenditure Type"
  ) +
  theme_minimal(base_size = font_size) +
  theme(legend.position = c(0, 1), legend.justification = c(0, 1), panel.grid.minor = element_blank())

# Total plot
total_plot <- ggplot(new_data_total, aes(x = Age_Group, y = Value, color = Country)) +
  geom_line(size = 1) +
  scale_color_manual(values = ft_palette) +
  labs(
    title = "Health Expenditure (PPP adjusted) 2013",
    x = "Age",
    y = "Expenditure (USD PPP)",
    color = "Country"
  ) +
  theme_minimal(base_size = font_size) +
  theme(legend.position = c(0, 1), legend.justification = c(0, 1), panel.grid.minor = element_blank())

# Print the plots
print(sg_plot)
print(uk_plot)
print(total_plot)
