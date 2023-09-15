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
nhs_data <- data.frame(
  Category = c("GP Annual Renumeration", 
               "Premises", 
               "Other (Advertising, Promotion, Entertainment)", 
               "Annual (including travel)", 
               "Per Surgery Consultation (9.22 minutes)"),
  Annual_Amount_GBP = c(113400, 15660, 17053, 201003, 28)  # GBP for Great British Pounds
)

# Compute "GP costs per appointment"
total_costs = nhs_data$Annual_Amount_GBP[nhs_data$Category == "Annual (including travel)"]
premises_costs = nhs_data$Annual_Amount_GBP[nhs_data$Category == "Premises"]
other_costs = nhs_data$Annual_Amount_GBP[nhs_data$Category == "Other (Advertising, Promotion, Entertainment)"]
surgery_consultation_cost = nhs_data$Annual_Amount_GBP[nhs_data$Category == "Per Surgery Consultation (9.22 minutes)"]

effective_annual_gp_cost = total_costs - premises_costs - other_costs
number_of_appointments = total_costs / surgery_consultation_cost

gp_costs_per_appointment = effective_annual_gp_cost / number_of_appointments

# Add the new value to the dataframe
new_row <- data.frame(Category = "GP costs per appointment", Annual_Amount_GBP = gp_costs_per_appointment)
nhs_data <- rbind(nhs_data, new_row)

