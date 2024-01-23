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
data_list$health_versus_consumption <- data_list$consumption_per_capita / data_list$out_of_pocket_health_spend_USD_per_capita

