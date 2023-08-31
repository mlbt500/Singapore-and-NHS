# Ensure the necessary library is loaded
library(WDI)

# List of countries to filter
countries_of_interest <- c("AU", "CH", "FI", "FR", "GB", "IE", "NL", "SG", "US", "ES", "GR", "PT", "IT", "NO", "TW", "KR", "JP")

# Fetching the necessary data
data_list <- WDI(indicator = c("SH.XPD.CHEX.PP.CD", "SH.XPD.CHEX.GD.ZS", "NY.GDP.PCAP.PP.CD", "SH.XPD.CHEX.PC.CD", "PA.NUS.PPPC.RF", "NE.CON.TOTL.CD", "SP.POP.TOTL"), 
                 country = countries_of_interest, start = 2019, end = 2019)

# Rename columns for clarity and ease
names(data_list)[names(data_list) == "SH.XPD.CHEX.PP.CD"] <- "health_care_per_capita_PPP"
names(data_list)[names(data_list) == "NY.GDP.PCAP.PP.CD"] <- "GDP_per_capita_PPP"
names(data_list)[names(data_list) == "NE.CON.TOTL.CD"] <- "consumption"
names(data_list)[names(data_list) == "SP.POP.TOTL"] <- "population"
names(data_list)[names(data_list) == "PA.NUS.PPPC.RF"] <- "conversion_rate"
names(data_list)[names(data_list) == "SH.XPD.CHEX.GD.ZS"] <- "health_exp_as_gdp_pct"
names(data_list)[names(data_list) == "SH.XPD.CHEX.PC.CD"] <- "health_exp_per_capita_cd"


# Calculate consumption per capita (PPP adjusted)
data_list$consumption_per_capita_PPP <- (data_list$consumption / data_list$conversion_rate) / data_list$population

# Filter for the year 2019
data_2019 <- data_list[data_list$year == 2019,]

# Define y axis limits
y_min <- min(data_2019$health_care_per_capita_PPP, na.rm = TRUE) - 500
y_max <- max(data_2019$health_care_per_capita_PPP, na.rm = TRUE) + 500

# Plot 1: Health spending versus GDP per capita
par(oma = c(3, 0, 0, 0))  # Adds space to the bottom outer margin

plot(health_care_per_capita_PPP ~ GDP_per_capita_PPP, data = data_2019, 
     xlab = "GDP per capita", 
     ylab = "Health care spending per capita", 
     main = "Health spending versus GDP per capita (PPP adjusted)",  
     ylim = c(y_min, y_max))

offset <- (y_max - y_min) * 0.1  # Adjust offset for clarity
text(data_2019$GDP_per_capita_PPP[data_2019$iso2c == "IE"], data_2019$health_care_per_capita_PPP[data_2019$iso2c == "IE"] - offset, labels = "IE")
text(data_2019$GDP_per_capita_PPP[data_2019$iso2c == "US"], data_2019$health_care_per_capita_PPP[data_2019$iso2c == "US"] - offset, labels = "US")
text(data_2019$GDP_per_capita_PPP[data_2019$iso2c == "SG"], data_2019$health_care_per_capita_PPP[data_2019$iso2c == "SG"] - offset, labels = "SG")

# Add footnote to the outer margin
mtext("AU, CH, FI, FR, GB, IE, NL, SG, US, ES, GR, PT, IT, NO, TW, KR, JP", 
      side = 1, line = 1, outer = TRUE, cex = 0.8)

# Plot 2: Health spending versus Consumption per capita
plot(health_care_per_capita_PPP ~ consumption_per_capita_PPP, data = data_2019,
     xlab = "Consumption per capita", 
     ylab = "Health care spending per capita",
     main = "Health spending vs Consumption (Both PPP adjusted)")

offset <- (max(data_2019$health_care_per_capita_PPP) - min(data_2019$health_care_per_capita_PPP)) * 0.1
text(data_2019$consumption_per_capita_PPP[data_2019$iso2c == "US"], data_2019$health_care_per_capita_PPP[data_2019$iso2c == "US"] - offset, labels = "US")
text(data_2019$consumption_per_capita_PPP[data_2019$iso2c == "GB"], data_2019$health_care_per_capita_PPP[data_2019$iso2c == "GB"] - offset, labels = "GBR")
text(data_2019$consumption_per_capita_PPP[data_2019$iso2c == "SG"], data_2019$health_care_per_capita_PPP[data_2019$iso2c == "SG"] - offset, labels = "SG")
