# Load the WDI package
library(WDI)

# Define countries of interest
countries_of_interest <- c("CH", "PT", "SG")

# Fetching the necessary data
data_list <- WDI(indicator = c("SH.XPD.OOPC.PC.CD", "NE.CON.TOTL.CD", "SP.POP.TOTL", "SH.XPD.OOPC.CH.ZS"), 
                 country = countries_of_interest, 
                 start = 2000, 
                 end = 2019)

# Define indicators
names(data_list)[names(data_list) == "NE.CON.TOTL.CD"] <- "consumption"
names(data_list)[names(data_list) == "SP.POP.TOTL"] <- "population"
names(data_list)[names(data_list) == "SH.XPD.OOPC.PC.CD"] <- "out_of_pocket_health_spend_USD_per_capita"
names(data_list)[names(data_list) == "SH.XPD.OOPC.CH.ZS"] <- "out_of_pocket_health_spend_pc_health_spend"

# Calculate consumption per capita
data_list$consumption_per_capita <- data_list$consumption / data_list$population

# Calculate ratio of consumption per capita to out-of-pocket health spending (USD per capita)
data_list$health_versus_consumption <- data_list$out_of_pocket_health_spend_USD_per_capita/ data_list$consumption_per_capita

data_list$out_of_pocket_health_spend_pc_health_spend <- data_list$out_of_pocket_health_spend_pc_health_spend/100

# Set up a 1x2 plotting area with larger outer margins for title and footnote
par(mfrow=c(1,2), oma=c(3, 0, 5, 0))  # Increase bottom and top outer margins

# Set colors
colors <- c("lightgray", "red", "black")

# First Plot
plot(NULL, xlim=c(min(data_list$year), max(data_list$year)), 
     ylim=c(min(data_list$out_of_pocket_health_spend_pc_health_spend, na.rm=TRUE), 
            max(data_list$out_of_pocket_health_spend_pc_health_spend, na.rm=TRUE)),
     ylab="Out-of-Pocket Health/ Health Spend")
for (country in unique(data_list$country)) {
  country_data <- data_list[data_list$country == country, ]
  lines(country_data$year, country_data$out_of_pocket_health_spend_pc_health_spend, 
        col=colors[which(unique(data_list$country) == country)], lwd=2)
}

# Add legend to the lower left in the margin under the first plot
par(xpd=NA) # Allow plotting in the margin
legend("bottomleft", inset=c(0.1, -0.6), legend=unique(data_list$country), col=colors, lty=1, lwd=2, horiz=TRUE)

# Second Plot
plot(NULL, xlim=c(min(data_list$year), max(data_list$year)), 
     ylim=c(min(data_list$health_versus_consumption, na.rm=TRUE), 
            max(data_list$health_versus_consumption, na.rm=TRUE)),
     xlab = "", ylab="Out-of-pocket health/ Consumption")
for (country in unique(data_list$country)) {
  country_data <- data_list[data_list$country == country, ]
  lines(country_data$year, country_data$health_versus_consumption, 
        col=colors[which(unique(data_list$country) == country)], lwd=2)
}

# Add overall title and footnote
mtext("Out-of-pocket health spend for \n Singapore, Switzerland, and Portugal", side= 3, line=1, outer=TRUE, cex=2)
mtext("Source: World Bank Development Indicators", side=1, line=1, adj=0, cex=0.7, outer=TRUE)

# Prepare data for Plot 3
data_for_plot3 <- data_list[, c("year", "out_of_pocket_health_spend_pc_health_spend")]
# Ensure unique rows if necessary or perform any aggregation

# Export the data for Plot 3
write.csv(data_for_plot3, "plot3_out_of_pocket_health_spend_percentage.csv", row.names = FALSE)

# Prepare data for Plot 4
data_for_plot4 <- data_list[, c("year", "health_versus_consumption")]
# Ensure unique rows if necessary or perform any aggregation

# Export the data for Plot 4
write.csv(data_for_plot4, "plot4_health_versus_consumption.csv", row.names = FALSE)
