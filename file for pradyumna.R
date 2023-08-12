library(WDI)
library(countrycode)
library(htmlTable)
valid_countries <- countrycode::codelist$iso3c

#conversion rate
conversion_rate <- WDI(indicator = "PA.NUS.PPPC.RF")
conversion_rate <- conversion_rate[conversion_rate$iso3c %in% valid_countries & conversion_rate$year == 2019,]
conversion_rate_ranked <- conversion_rate[order(-conversion_rate$PA.NUS.PPPC.RF), ]
conversion_rate_ranked$rank <- 1:nrow(conversion_rate_ranked)
#54 out 215

#per capita GDP current usd
GDP_per_capita_USD <- WDI(indicator = "NY.GDP.PCAP.CD")
GDP_per_capita_USD <- GDP_per_capita_USD[GDP_per_capita_USD$iso3c %in% valid_countries & GDP_per_capita_USD$year == 2019,]
GDP_per_capita_USD_ranked <- GDP_per_capita_USD[order(-GDP_per_capita_USD$NY.GDP.PCAP.CD), ]
GDP_per_capita_USD_ranked$rank <- 1:nrow(GDP_per_capita_USD_ranked)

# Merge the two ranked data frames by iso3c
merged_data <- merge(conversion_rate_ranked, GDP_per_capita_USD_ranked, by = "iso3c", suffixes = c("_ppp", "_gdp"))

# Create a new data frame with the desired columns
final_table <- data.frame(
  name = countrycode::countrycode(merged_data$iso3c, 'iso3c', 'country.name'),
  rank_of_gdp_per_capita = merged_data$rank_gdp,
  rank_of_ppp = merged_data$rank_ppp
)

# Order the data frame by rank_of_gdp_per_capita in ascending order
final_table <- final_table[order(final_table$rank_of_gdp_per_capita),]
rownames(final_table) <- NULL

# Create the HTML table
html_table <- htmlTable(final_table)

# Print the HTML table
print(html_table)
