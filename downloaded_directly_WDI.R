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

# Fetching the new data
consumption <- WDI(indicator = "NE.CON.TOTL.CD", country = countries_of_interest)
population <- WDI(indicator = "SP.POP.TOTL", country = countries_of_interest)

# Merging the new data
final_data <- merge(final_data, consumption, by=c("iso2c", "year"))
final_data <- merge(final_data, population, by=c("iso2c", "year"))

# Calculate consumption per capita (PPP adjusted)
final_data$consumption_per_capita_PPP <- (final_data$NE.CON.TOTL.CD / final_data$PA.NUS.PPPC.RF) / final_data$SP.POP.TOTL

# Filter for the year 2019
data_2019 <- final_data[final_data$year == 2019,]

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

# Plotting the data
plot(health_care_per_capita_PPP ~ consumption_per_capita_PPP, data = data_2019,
     xlab = "Consumption per capita (PPP adjusted)", 
     ylab = "Health care spending per capita (PPP adjusted)",
     main = "Health spending vs Consumption per capita (Both PPP adjusted)")

# Add labels for the US, GBR, and Singapore
offset <- (max(data_2019$health_care_per_capita_PPP) - min(data_2019$health_care_per_capita_PPP)) * 0.1  # Increased offset

# Label for US
text(data_2019$consumption_per_capita_PPP[data_2019$country == "US"], 
     data_2019$health_care_per_capita_PPP[data_2019$country == "US"] - offset, labels = "US")

# Label for GBR
text(data_2019$consumption_per_capita_PPP[data_2019$country == "GB"], 
     data_2019$health_care_per_capita_PPP[data_2019$country == "GB"] - offset, labels = "GBR")

# Label for Singapore
text(data_2019$consumption_per_capita_PPP[data_2019$country == "SG"], 
     data_2019$health_care_per_capita_PPP[data_2019$country == "SG"] - offset, labels = "SG")


# Plotting the data
plot(health_care_per_capita_PPP ~ consumption_per_capita_PPP, data = data_2019,
     xlab = "Consumption per capita (PPP adjusted)", 
     ylab = "Health care spending per capita (PPP adjusted)",
     main = "Health spending vs Consumption per capita (Both PPP adjusted)")

# Add labels for the US, GBR, and Singapore
offset <- (max(data_2019$health_care_per_capita_PPP) - min(data_2019$health_care_per_capita_PPP)) * 0.1  # Increased offset

# Label for US
text(data_2019$consumption_per_capita_PPP[data_2019$country == "US"], 
     data_2019$health_care_per_capita_PPP[data_2019$country == "US"] - offset, labels = "US")

# Label for GBR
text(data_2019$consumption_per_capita_PPP[data_2019$country == "GB"], 
     data_2019$health_care_per_capita_PPP[data_2019$country == "GB"] - offset, labels = "GBR")

# Label for Singapore
text(data_2019$consumption_per_capita_PPP[data_2019$country == "SG"], 
     data_2019$health_care_per_capita_PPP[data_2019$country == "SG"] - offset, labels = "SG")
