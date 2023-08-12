library(WDI)
library(dplyr)

# List of countries to filter, including additional countries Taiwan, South Korea, and Japan
countries_of_interest <- c("AU", "CH", "FI", "FR", "GB", "IE", "NL", "SG", "US", "ES", "GR", "PT", "IT", "NO", "KR", "JP")

# Fetching the necessary data
GDP_per_capita_USD <- WDI(indicator = "NY.GDP.PCAP.CD", country = countries_of_interest)
Health_per_capita <- WDI(indicator = "SH.XPD.CHEX.PC.CD", country = countries_of_interest)
conversion_factor <- WDI(indicator = "PA.NUS.PPPC.RF", country = countries_of_interest)

# Merging GDP per capita and Health per capita
final_data <- merge(GDP_per_capita_USD, Health_per_capita, by=c("iso2c", "year"))

# Merging conversion factor
final_data <- merge(final_data, conversion_factor, by=c("iso2c", "year"))

# Calculating health care percentage of GDP
final_data$health_care_percentage_of_GDP <- (final_data$SH.XPD.CHEX.PC.CD / final_data$NY.GDP.PCAP.CD) * 100

# Calculating health care per capita USD, PPP
final_data$health_care_per_capita_USD_PPP <- final_data$SH.XPD.CHEX.PC.CD / final_data$PA.NUS.PPPC.RF

# Calculating GDP per capita USD, PPP
final_data$GDP_per_capita_USD_PPP <- final_data$NY.GDP.PCAP.CD / final_data$PA.NUS.PPPC.RF

# Selecting relevant columns and renaming
final_data <- final_data %>%
  select(country = iso2c, year, health_care_percentage_of_GDP, health_care_per_capita_USD = SH.XPD.CHEX.PC.CD, health_care_per_capita_USD_PPP, conversion_factor = PA.NUS.PPPC.RF, GDP_per_capita_USD_PPP)

# Filter for all countries in 2019
data_2019 <- final_data[final_data$year == 2019,]

# Determine the y-axis limits
y_min <- min(data_2019$health_care_per_capita_USD_PPP)
y_max <- max(data_2019$health_care_per_capita_USD_PPP) * 1.2

# Create the plot with adjusted y-axis limits
plot(health_care_per_capita_USD_PPP ~ GDP_per_capita_USD_PPP, data = data_2019, xlab = "GDP per capita", ylab = "Health care spending per capita", ylim = c(y_min, y_max), main = "Health care versus GDP ppp adjusted 2019")

# Offset value to move the labels up
offset <- (y_max - y_min) * 0.1 # Adjust as needed

text(data_2019$GDP_per_capita_USD_PPP[data_2019$country == "IE"], data_2019$health_care_per_capita_USD_PPP[data_2019$country == "IE"] + offset, labels = "IE")
text(data_2019$GDP_per_capita_USD_PPP[data_2019$country == "US"], data_2019$health_care_per_capita_USD_PPP[data_2019$country == "US"] + offset, labels = "US")
text(data_2019$GDP_per_capita_USD_PPP[data_2019$country == "SG"], data_2019$health_care_per_capita_USD_PPP[data_2019$country == "SG"] + offset, labels = "SG")

plot()