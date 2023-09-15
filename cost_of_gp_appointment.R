#Average exchange rate for the 2018-19 financial year
#GBP to USD https://www.bankofengland.co.uk/boeapps/database/fromshowcolumns.asp?Travel=NIxRSxSUx&FromSeries=1&ToSeries=50&DAT=RNG&FD=1&FM=Apr&FY=2018&TD=31&TM=Mar&TY=2019&FNY=&CSVF=TT&html.x=152&html.y=19&C=C8P&Filter=N

GBER <- read.csv("GB_to_USD_2018-9.csv")
GBP_to_USD <- mean(GBER[,2])

#SGD to USD https://eservices.mas.gov.sg/Statistics/msb/ExchangeRates.aspx

SGER <- read.csv("SGD_to_USD_2018-19.csv")
SGER <- SGER[-c(1:6),-c(1)]
SGER[,3] <- as.numeric(SGER[,3])
SGD_to_USD <- 1/mean(SGER[,3])

#Data from National Healthcare Group Singapore Polyclinic
doctor_renumeration <- data.frame(
Category = c("Annual Renumeration", 
             "Acute Illness Visit", 
             "Complex Chronic Visit (4+ conditions)", 
             "Moderate Chronic Visit (2-3 conditions)", 
             "Simple Chronic Visit (1 condition)"),
FY18_Amount = c(213500, 17.66, 31.91, 24.92, 20.13)
)
doctor_renumeration$FY18_Amount_USD <- doctor_renumeration$FY18_Amount * SGD_to_USD

#Data from Unit Cost of Health and Social Care for NHS https://kar.kent.ac.uk/79286/11/UCFinalFeb20.pdf
