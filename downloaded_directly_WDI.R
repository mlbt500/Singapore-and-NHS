library(WDI)
library(dplyr)
library(ggplot2)

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
# Set the outer margin space
par(oma = c(3, 0, 0, 0))  # Adds space to the bottom outer margin

# Plot data
par(oma = c(3, 0, 0, 0))  # Adds space to the bottom outer margin

plot(health_care_per_capita_PPP ~ GDP_per_capita_PPP, data = data_2019, 
     xlab = "GDP per capita", 
     ylab = "Health care spending per capita", 
     main = "Health spending versus GDP per capita (PPP adjusted)",  
     ylim = c(y_min, y_max))

# Add country labels with offset
offset <- (y_max - y_min) * 0.1  # Adjust as needed
text(data_2019$GDP_per_capita_PPP[data_2019$country == "IE"], 
     data_2019$health_care_per_capita_PPP[data_2019$country == "IE"] + offset, labels = "IE")
text(data_2019$GDP_per_capita_PPP[data_2019$country == "US"], 
     data_2019$health_care_per_capita_PPP[data_2019$country == "US"] + offset, labels = "US")
text(data_2019$GDP_per_capita_PPP[data_2019$country == "SG"], 
     data_2019$health_care_per_capita_PPP[data_2019$country == "SG"] + offset, labels = "SG")

# Add footnote to the outer margin
mtext("AU, CH, FI, FR, GB, IE, NL, SG, US, ES, GR, PT, IT, NO, TW, KR, JP", 
      side = 1, line = 1, outer = TRUE, cex = 0.8)

write.csv(data_2019, file = "data_20192.csv", row.names = FALSE)

# Filter for UK and Singapore in 2019
uk_sg_data_2019 <- data_2019[data_2019$country %in% c("GB", "SG"),]

# Select the desired columns
uk_sg_data_final <- uk_sg_data_2019 %>%
  select(country,
         health_care_per_capita_USD = health_care_per_capita_USD,
         health_care_per_capita_PPP = health_care_per_capita_PPP,
         ppp_adjustor = conversion_factor)

# Determine the y-axis limits
y_min <- min(data_2019$GDP_per_capita_USD_PPP * data_2019$conversion_factor)
y_max <- max(data_2019$GDP_per_capita_USD_PPP * data_2019$conversion_factor) * 1.2

# Plot the main graph with adjusted y-axis limits
plot((GDP_per_capita_USD_PPP*conversion_factor) ~ conversion_factor, data = data_2019, ylab = "GDP per capita", xlab = "PPP conversion factor")
# Offset value to move the labels down
offset <- (y_max - y_min) * 0.05 # Adjust as needed

# Add labels for Ireland and Singapore
text(data_2019$conversion_factor[data_2019$country == "IE"], (data_2019$GDP_per_capita_USD_PPP[data_2019$country == "IE"] * data_2019$conversion_factor[data_2019$country == "IE"]) - offset, labels = "IE")
text(data_2019$conversion_factor[data_2019$country == "SG"], (data_2019$GDP_per_capita_USD_PPP[data_2019$country == "SG"] * data_2019$conversion_factor[data_2019$country == "SG"]) - offset, labels = "SG")