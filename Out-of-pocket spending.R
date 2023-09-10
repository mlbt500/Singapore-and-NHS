library(WDI)

# Define countries, indicator, and date range
iso_codes <- c("PRT", "CHE", "SGP")
full_country_names <- c("Portugal", "Switzerland", "Singapore")
indicator <- "SH.XPD.OOPC.CH.ZS"
start_date <- 2000
end_date <- 2019

# Fetch data using the WDI package
data <- WDI(country=iso_codes, indicator=indicator, start=start_date, end=end_date, extra=TRUE)

# Subset data to only the required years (2000-2019)
data <- data[data$year >= start_date & data$year <= end_date, ]

# Set colors
colors <- c("lightgray", "red", "black")

# Create an empty plot
plot(NULL, xlim=c(start_date, end_date), ylim=c(min(data$`SH.XPD.OOPC.CH.ZS`, na.rm=TRUE), max(data$`SH.XPD.OOPC.CH.ZS`, na.rm=TRUE)),
     xlab="Year", ylab="Out-of-pocket spending (% of total health spending)",
     main="Out-of-pocket health spending: 2000-2019")

# Loop through each country to draw lines
for (country in full_country_names) {
  country_data <- data[data$country == country, ]
  lines(country_data$year, country_data$`SH.XPD.OOPC.CH.ZS`, col=colors[which(full_country_names == country)], lwd=2)
}

# Add legend
legend("topright", legend=full_country_names, col=colors, lty=1, lwd=2)

# Add footnote
mtext("Source: World Bank Development Indicators", side=1, line=4, adj=0, cex=0.7)


