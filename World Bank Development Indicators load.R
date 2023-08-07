install.packages("WDI")
library(WDI)
# GDP current US$
GDP_USD <- WDI(indicator = "NY.GDP.MKTP.CD")

#Current health expenditure per capita (current US$)
Health <- WDI(indication = "SH.XPD.CHEX.PC.CD")

# Population, total
Population <- WDI(indication = "SP.POP.TOTL")

#Price level ratio of PPP conversion factor (GDP) to market exchange rate
conversion_factor <- (indication = "PA.NUS.PPPC.RF")
