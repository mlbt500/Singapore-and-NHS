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
# ... [your existing code above remains unchanged]

# Adjust x-axis limits for both graphs
x_max_gdp <- max(data_2019$GDP_per_capita_PPP, na.rm = TRUE) * 1.10
x_min_gdp <- min(data_2019$GDP_per_capita_PPP, na.rm = TRUE) * 0.90

x_max_consume <- max(data_2019$consumption_per_capita_PPP, na.rm = TRUE) * 1.10
x_min_consume <- min(data_2019$consumption_per_capita_PPP, na.rm = TRUE) * 0.90

# Exclude outliers for trend line calculations
data_for_trend1 <- subset(data_2019, !(iso2c %in% c("SG", "IE", "US")))
data_for_trend2 <- subset(data_2019, !(iso2c %in% c("SG")))

# Set up side-by-side plotting
par(mfrow=c(1,2), oma=c(4, 0, 0, 0))  # Sets up a 1x2 plotting area

# Plot 1: Health spending versus GDP per capita
plot(health_care_per_capita_PPP ~ GDP_per_capita_PPP, data = data_2019, 
     xlim = c(x_min_gdp, x_max_gdp), ylim = c(y_min, y_max),
     xlab = "GDP per capita", ylab = "Health care spending per capita", 
     main = "", pch=16, col=ifelse(data_2019$iso2c %in% c("IE", "US", "SG", "GB"), "red", "gray"))

# Adding trend line to the first graph (excluding outliers)
model1 <- lm(health_care_per_capita_PPP ~ GDP_per_capita_PPP, data = data_for_trend1)
abline(model1, lty = 2, lwd = 2)

# Label countries for the first graph
labels_to_add <- c("IE", "GB", "SG", "US")
for(label in labels_to_add) {
  text(data_2019$GDP_per_capita_PPP[data_2019$iso2c == label], 
       data_2019$health_care_per_capita_PPP[data_2019$iso2c == label] - offset, 
       labels = label, col = "red")
}

# Plot 2: Health spending versus Consumption per capita
plot(health_care_per_capita_PPP ~ consumption_per_capita_PPP, data = data_2019,
     xlim = c(x_min_consume, x_max_consume), ylim = c(y_min, y_max),
     xlab = "Consumption per capita", ylab = "Health care spending per capita",
     main = "", pch=16, col=ifelse(data_2019$iso2c %in% c("IE", "US", "SG", "GB"), "red", "gray"))

# Adding trend line to the second graph (excluding outliers)
model2 <- lm(health_care_per_capita_PPP ~ consumption_per_capita_PPP, data = data_for_trend2)
abline(model2, lty = 2, lwd = 2)

# Label countries for the second graph
for(label in labels_to_add) {
  text(data_2019$consumption_per_capita_PPP[data_2019$iso2c == label], 
       data_2019$health_care_per_capita_PPP[data_2019$iso2c == label] - offset, 
       labels = label, col = "red")
}


# Add a central title for both graphs with the title spanning two lines
title("Health Care Spending vs. Economic Indicators 2019\n World Bank Development Indicators (USD PPP adjusted)", outer = TRUE, line = -2)

# Add footnotes to the outer margin

# Add footnotes to the outer margin
mtext("**AU, CH, FI, FR, GB, IE, NL, SG, US, ES, GR, PT, IT, NO, TW, KR, JP", 
      side = 1, line = 2, outer = TRUE, cex = 0.8)
mtext("*Trend lines exclude outliers (IE, SG and US in the first graph; SG in the second graph)", 
      side = 1, line = 1, outer = TRUE, cex = 0.8)

# Reset graphics parameters
par(mfrow=c(1,1), oma=c(0,0,0,0))

# Ensure the necessary library is loaded
library(WDI)

# List of countries to filter
countries_of_interest <- c("AU", "CH", "FI", "FR", "GB", "IE", "NL", "SG", "US", "ES", "GR", "PT", "IT", "NO", "TW", "KR", "JP")

# Fetching the necessary data
data_list <- WDI(indicator = c("SH.XPD.CHEX.PP.CD", "SH.XPD.CHEX.GD.ZS", "NY.GDP.PCAP.PP.CD", "SH.XPD.CHEX.PC.CD", "PA.NUS.PPPC.RF", "NE.CON.TOTL.CD", "SP.POP.TOTL"), 
                 country = countries_of_interest, start = 2016, end = 2016)

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

# Filter for the year 2016
data_2016 <- data_list[data_list$year == 2016,]

# Define y axis limits
y_min <- min(data_2016$health_care_per_capita_PPP, na.rm = TRUE) - 500
y_max <- max(data_2016$health_care_per_capita_PPP, na.rm = TRUE) + 500

# Adjust x-axis limits for both graphs
x_max_gdp <- max(data_2016$GDP_per_capita_PPP, na.rm = TRUE) * 1.10
x_min_gdp <- min(data_2016$GDP_per_capita_PPP, na.rm = TRUE) * 0.90

x_max_consume <- max(data_2016$consumption_per_capita_PPP, na.rm = TRUE) * 1.10
x_min_consume <- min(data_2016$consumption_per_capita_PPP, na.rm = TRUE) * 0.90

# Exclude outliers for trend line calculations
data_for_trend1 <- subset(data_2016, !(iso2c %in% c("SG", "IE", "US")))
data_for_trend2 <- subset(data_2016, !(iso2c %in% c("SG")))

# Set up side-by-side plotting
par(mfrow=c(1,2), oma=c(4, 0, 0, 0))  # Sets up a 1x2 plotting area

# Plot 1: Health spending versus GDP per capita
plot(health_care_per_capita_PPP ~ GDP_per_capita_PPP, data = data_2016, 
     xlim = c(x_min_gdp, x_max_gdp), ylim = c(y_min, y_max),
     xlab = "GDP per capita", ylab = "Health care spending per capita", 
     main = "", pch=16, col=ifelse(data_2016$iso2c %in% c("IE", "US", "SG", "GB"), "red", "gray"))

# Adding trend line to the first graph (excluding outliers)
model1 <- lm(health_care_per_capita_PPP ~ GDP_per_capita_PPP, data = data_for_trend1)
abline(model1, lty = 2, lwd = 2)

# Label countries for the first graph
labels_to_add <- c("IE", "GB", "SG", "US")
for(label in labels_to_add) {
  text(data_2016$GDP_per_capita_PPP[data_2016$iso2c == label], 
       data_2016$health_care_per_capita_PPP[data_2016$iso2c == label], 
       labels = label, col = "red")
}

# Plot 2: Health spending versus Consumption per capita
plot(health_care_per_capita_PPP ~ consumption_per_capita_PPP, data = data_2016,
     xlim = c(x_min_consume, x_max_consume), ylim = c(y_min, y_max),
     xlab = "Consumption per capita", ylab = "Health care spending per capita",
     main = "", pch=16, col=ifelse(data_2016$iso2c %in% c("IE", "US", "SG", "GB"), "red", "gray"))

# Adding trend line to the second graph (excluding outliers)
model2 <- lm(health_care_per_capita_PPP ~ consumption_per_capita_PPP, data = data_for_trend2)
abline(model2, lty = 2, lwd = 2)

# Label countries for the second graph
for(label in labels_to_add) {
  text(data_2016$consumption_per_capita_PPP[data_2016$iso2c == label], 
       data_2016$health_care_per_capita_PPP[data_2016$iso2c == label], 
       labels = label, col = "red")
}

# Add a central title for both graphs with the title spanning two lines
title("Health Care Spending vs. Economic Indicators 2016\n World Bank Development Indicators (USD PPP adjusted)", outer = TRUE, line = -2)

# Add footnotes to the outer margin
mtext("**AU, CH, FI, FR, GB, IE, NL, SG, US, ES, GR, PT, IT, NO, TW, KR, JP", 
      side = 1, line = 2, outer = TRUE, cex = 0.8)
mtext("*Trend lines exclude outliers (IE, SG and US in the first graph; SG in the second graph)", 
      side = 1, line = 1, outer = TRUE, cex = 0.8)

# Reset graphics parameters
par(mfrow=c(1,1), oma=c(0,0,0,0))
