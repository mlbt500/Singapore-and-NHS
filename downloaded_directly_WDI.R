library(WDI)
library(dplyr)

# List of countries to filter
countries_of_interest <- c("AU", "CH", "FI", "FR", "GB", "IE", "NL", "SG", "US", "ES", "GR", "PT", "IT", "NO", "TW", "KR", "JP")

# Fetching the necessary data
Health_per_capita_PPP <- WDI(indicator = "SH.XPD.CHEX.PP.CD", country = countries_of_interest)
Health_percentage_of_GDP <- WDI(indicator = "SH.XPD.CHEX.GD.ZS", country = countries_of_interest)
GDP_per_capita_PPP <- WDI(indicator = "NY.GDP.PCAP.PP.CD", country = countries_of_interest)
Health_per_capita_USD <- WDI(indicator = "SH.XPD.CHEX.PC.CD", country = countries_of_interest)
conversion_rate <- WDI(indicator = "PA.NUS.PPPC.RF", country = countries_of_interest)

# Merging all data
final_data <- merge(Health_per_capita_PPP, Health_percentage_of_GDP, by=c("iso2c", "year"))
final_data <- merge(final_data, GDP_per_capita_PPP, by=c("iso2c", "year"))
final_data <- merge(final_data, Health_per_capita_USD, by=c("iso2c", "year"))
final_data <- merge(final_data, conversion_rate, by=c("iso2c", "year"))

# Selecting relevant columns and renaming
final_data <- final_data %>%
  select(country = iso2c, year, health_care_percentage_of_GDP = SH.XPD.CHEX.GD.ZS, health_care_per_capita_PPP = SH.XPD.CHEX.PP.CD, GDP_per_capita_PPP = NY.GDP.PCAP.PP.CD, health_care_per_capita_USD = SH.XPD.CHEX.PC.CD, conversion_factor = PA.NUS.PPPC.RF)

# Filter for all countries in 2019
data_2019 <- final_data[final_data$year == 2019,]

# Determine the y-axis limits
y_min <- min(data_2019$health_care_per_capita_PPP)
y_max <- max(data_2019$health_care_per_capita_PPP) * 1.2

# Create the plot with adjusted y-axis limits
plot(health_care_per_capita_PPP ~ GDP_per_capita_PPP, data = data_2019, xlab = "GDP per capita (PPP Adjusted)", ylab = "Health care spending per capita (PPP Adjusted)", ylim = c(y_min, y_max))

# Offset value to move the labels up
offset <- (y_max - y_min) * 0.1 # Adjust as needed

text(data_2019$GDP_per_capita_PPP[data_2019$country == "IE"], data_2019$health_care_per_capita_PPP[data_2019$country == "IE"] + offset, labels = "IE")
text(data_2019$GDP_per_capita_PPP[data_2019$country == "US"], data_2019$health_care_per_capita_PPP[data_2019$country == "US"] + offset, labels = "US")
text(data_2019$GDP_per_capita_PPP[data_2019$country == "SG"], data_2019$health_care_per_capita_PPP[data_2019$country == "SG"] + offset, labels = "SG")

write.csv(data_2019, file = "data_20192.csv", row.names = FALSE)

# Filter for UK and Singapore in 2019
uk_sg_data_2019 <- data_2019[data_2019$country %in% c("GB", "SG"),]

# Select the desired columns
uk_sg_data_final <- uk_sg_data_2019 %>%
  select(country,
         health_care_per_capita_USD = health_care_per_capita_USD,
         health_care_per_capita_PPP = health_care_per_capita_PPP,
         ppp_adjustor = conversion_factor)

# Print the final table
unique(data_2019$country)
