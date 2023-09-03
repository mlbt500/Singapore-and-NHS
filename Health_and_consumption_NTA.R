library(dplyr)
library(tidyr)
library(WDI)
library(stringr)

NTA <- read.csv("NTA_consumption.csv")

#filter for 2010 to 2015
NTA_2010_2015 <- NTA[NTA$Year >= 2010 & NTA$Year <= 2015, ]
#define developed countries
developed_countries <- c("Austria", "United Kingdom", "South Korea", "Canada", 
                         "Slovenia", "Australia", "France", "Taiwan", "US", "Singapore")
# Filter the dataset for developed countries
NTA_developed <- NTA_2010_2015[NTA_2010_2015$Country %in% developed_countries, ]

#Singapore only has 2013, filter for closest to 2013
NTA_closest_2013 <- NTA_developed %>%
  group_by(Country) %>%
  mutate(diff = abs(Year - 2013)) %>%
  filter(diff == min(diff)) %>%
  ungroup() %>%
  select(-diff)

unique(NTA_closest_2013$VarType)
#get rid of age columns -- now only age and value column

NTA_long <- NTA_closest_2013 %>%
  pivot_longer(
    cols = starts_with("Age"), 
    names_to = "Age", 
    values_to = "Value"
  ) %>%
  mutate(Age = as.numeric(str_extract(Age, "\\d+"))) %>%
  arrange(Country, Age)

#uniform units
NTA_normalised <- NTA_long %>%
  mutate(
    Value = case_when(
      Unit == "Millions"  ~ Value * 1e6,
      Unit == "Billions"  ~ Value * 1e9,
      Unit == "Thousands" ~ Value * 1e3,
      Unit == "Units"     ~ Value,
      Unit == "Unit"      ~ Value,
      TRUE                ~ Value
    )
  ) %>%
  select(-Unit)

# Filter data for the UK and relevant Variable.Name
uk_data <- NTA_normalised %>%
  filter(Country == "United Kingdom", Variable.Name %in% c("Private Consumption, Health", "Public Consumption, Health", "Population, Total"))

# Calculate NTA for private consumption
private_nta <- uk_data %>%
  filter(Variable.Name == "Private Consumption, Health") %>%
  left_join(uk_data %>% filter(Variable.Name == "Population, Total"), by = c("Age", "Year")) %>%
  mutate(NTA_Value = Value.x * Value.y) %>%
  select(Age, NTA_Value, Year)

# Calculate NTA for public consumption
public_nta <- uk_data %>%
  filter(Variable.Name == "Public Consumption, Health") %>%
  left_join(uk_data %>% filter(Variable.Name == "Population, Total"), by = c("Age", "Year")) %>%
  mutate(NTA_Value = Value.x * Value.y) %>%
  select(Age, NTA_Value, Year)

# Adjust the private_nta data frame to include Age and Year
private_nta_adjusted <- private_nta %>%
  mutate(Country = "United Kingdom",
         Variable.Name = "Private Consumption, Health",
         VarType = "NTA",
         Value = NTA_Value) %>%
  select(Country, Variable.Name, VarType, Value, Age, Year)

# Adjust the public_nta data frame to include Age and Year
public_nta_adjusted <- public_nta %>%
  mutate(Country = "United Kingdom",
         Variable.Name = "Public Consumption, Health",
         VarType = "NTA",
         Value = NTA_Value) %>%
  select(Country, Variable.Name, VarType, Value, Age, Year)

# Merge with the original data frame
NTA_combined <- bind_rows(NTA_normalised %>% select(Country, Variable.Name, VarType, Value, Age, Year), private_nta_adjusted, public_nta_adjusted)

# Filter out rows with NA values in the VarType column
NTA_filtered <- NTA_combined %>% filter(VarType == "NTA")

# Create a new dataframe where we'll sum the private and public consumption
health_combined <- NTA_filtered %>%
  filter(Variable.Name %in% c("Private Consumption, Health", "Public Consumption, Health")) %>%
  group_by(Country, Age, Year) %>%
  summarize(Consumption_Health_Value = sum(Value, na.rm = TRUE)) %>%
  mutate(Variable.Name = "Consumption, Health", VarType = "NTA") %>%
  select(Country, Variable.Name, VarType, Age, Year, Value = Consumption_Health_Value) %>%
  ungroup()

# Now bind this new dataframe with the original NTA_filtered to get combined_values
combined_values <- bind_rows(NTA_filtered, health_combined)

#There is an error value in the Singaporean values -- for NTA they say million when
# they should say thousands. The smoothed mean values multipled by population
#confirm this error

# Adjust values in combined_values for Singapore
combined_values <- combined_values %>%
  mutate(Value = case_when(
    Country == "Singapore" & Variable.Name %in% c("Private Consumption, Health", "Public Consumption, Health", "Consumption") ~ Value / 1000,
    TRUE ~ Value
  ))


# Extracting health spending from combined_values
health_spending <- combined_values %>%
  filter(Variable.Name == "Consumption, Health") %>%
  group_by(Country, Year) %>%
  summarize(total_health_spending = sum(Value, na.rm = TRUE))

# Extracting population from NTA_normalised
population_data <- NTA_normalised %>%
  filter(Variable.Name == "Population, Total") %>%
  group_by(Country, Year) %>%
  summarize(total_population = sum(Value, na.rm = TRUE))

# Joining and calculating per capita spending
health_per_capita <- left_join(health_spending, population_data, by = c("Country", "Year")) %>%
  mutate(Health_spending_per_capita = total_health_spending / total_population) %>%
  select(Country, Year, Health_spending_per_capita)

print(health_per_capita)

unique(NTA_combined$Variable.Name)

# Extracting 'Consumption' from combined_values
consumption_data <- combined_values %>%
  filter(Variable.Name == "Consumption") %>%
  group_by(Country, Year) %>%
  summarize(total_consumption = sum(Value, na.rm = TRUE))

# Joining with population_data and calculating consumption per capita
consumption_per_capita <- left_join(consumption_data, population_data, by = c("Country", "Year")) %>%
  mutate(Consumption_per_capita = total_consumption / total_population) %>%
  select(Country, Year, Consumption_per_capita)

print(consumption_per_capita)

 