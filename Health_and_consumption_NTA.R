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

#South Korea don't give population values

combined_values <- combined_values %>%
  filter(!is.na(Age) & !is.na(Value))

combined_values <- combined_values %>%
  filter(!is.na(Age) & !is.na(Value) & Age <= 85)

combined_values <- combined_values %>%
  filter(Country != "South Korea")

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

health_per_capita

# Extracting 'Consumption' from combined_values
consumption_data <- combined_values %>%
  filter(Variable.Name == "Consumption") %>%
  group_by(Country, Year) %>%
  summarize(total_consumption = sum(Value, na.rm = TRUE))


print(consumption_per_capita)

combined_per_capita <- left_join(health_per_capita, consumption_per_capita, by = c("Country", "Year"))

#Remove United Kingdom and Austria

combined_per_capita <- combined_per_capita %>%
  filter(!(Country %in% c("Austria", "United Kingdom", "Taiwan")))

combined_per_capita <- combined_per_capita %>%
  mutate(Health_spending_per_capita = if_else(Country == "Singapore", 
                                              Health_spending_per_capita / 1000, 
                                              Health_spending_per_capita))
combined_per_capita

# ISO2C country codes for the countries in your list
country_codes <- c("AUS", "CAN", "FRA", "SGP", "SVN", "TWN", "USA")

# Fetching CPI data for the countries
cpi_data <- WDI(indicator = "FP.CPI.TOTL", country = country_codes)

# Fetching PPP conversion factor for the countries
ppp_data <- WDI(indicator = "PA.NUS.PPP", country = country_codes)

#Singapore
SNG_ppp_2013 <- WDI(country="SGP", indicator="PA.NUS.PPP", start=2013, end=2013)

#Australia
cpi_data_australia <- cpi_data %>%
  filter(country == "Australia" & year %in% 2010:2013)
aus_ppp_2013 <- WDI(country="AUS", indicator="PA.NUS.PPP", start=2013, end=2013)
cpi_australia <- cpi_data_australia[1,5]/100
cpi_australia

#Canada
cpi_data_canada <- cpi_data %>%
  filter(country == "Canada" & year %in% 2011:2013)
can_ppp_2013 <- WDI(country="CAN", indicator="PA.NUS.PPP", start=2013, end=2013)
cpi_canada <- cpi_data_canada[1,5]/cpi_data_canada[3,5]

#France
cpi_data_france <- cpi_data %>%
  filter(country == "France" & year %in% 2011:2013)
fra_ppp_2013 <- WDI(country="FRA", indicator="PA.NUS.PPP", start=2013, end=2013)
cpi_france <- cpi_data_france[1,5]/cpi_data_france[3,5]

#Slovenia
cpi_data_slovenia <- cpi_data %>%
  filter(country == "Slovenia" & year %in% 2010:2015)
SVN_ppp_2013 <- WDI(country="SVN", indicator="PA.NUS.PPP", start=2013, end=2013)
cpi_slovenia <- cpi_data_slovenia[1,5]


#United States
cpi_data_us <- cpi_data %>%
  filter(country == "United States" & year %in% 2011:2013)
cpi_data_us
cpi_us <- cpi_data_us[1,5]/cpi_data_us[3,5]
