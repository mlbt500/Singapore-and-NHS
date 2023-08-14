# Loading libraries
library(dplyr)
library(tidyverse)

# Reading CSV
NTA <- read.csv("NTA.csv")

# Filtering data for 2013, Singapore, and the United Kingdom
NTA_2013 <- NTA[NTA$Year == 2013 & NTA$Country %in% c("United Kingdom", "Singapore"),]

# Removing unwanted rows
NTA_2013 <- NTA_2013[-c(5:6),]

# Transforming to long format
NTA_2013_long <- NTA_2013 %>%
  pivot_longer(cols = starts_with("Age"),
               names_to = "Age_Group",
               values_to = "Value")

# Selecting required columns
NTA1 <- NTA_2013_long[,c("Country", "Year", "Variable.Name", "Age_Group", "Value")]
NTA1 <- NTA1[NTA1$Age_Group != "Age.Groups", ]

# Multiplying UK's population by 1000
NTA1$Value <- ifelse(NTA1$Country == "United Kingdom" & NTA1$Variable.Name == "Population, Total", NTA1$Value * 1000, NTA1$Value)

# Summing public and private health spending
relevant_data <- NTA1[NTA1$Variable.Name %in% c("Public Consumption, Health", "Private Consumption, Health"),]
sum_data <- relevant_data %>%
  group_by(Country, Age_Group, Year) %>%
  summarise(Value = sum(Value), .groups = 'drop')
sum_data$Variable.Name <- "Public and Private, Health"
NTA2 <- bind_rows(NTA1, sum_data)

# Calculating Health*Population
health_data <- NTA2[NTA2$Variable.Name == "Public and Private, Health",]
pop_data <- NTA2[NTA2$Variable.Name == "Population, Total",]
combined_data <- merge(health_data, pop_data, by = c("Country", "Age_Group", "Year"))
combined_data$Value <- combined_data$Value.x * combined_data$Value.y
combined_data$Variable.Name <- "Health*Population"
combined_data <- combined_data[, c("Country", "Year", "Variable.Name", "Age_Group", "Value")]
NTA3 <- bind_rows(NTA2, combined_data)

#Calculating Health*Singapore with United Kingdom Population
health_data <- NTA2[NTA2$Variable.Name == "Public and Private, Health" & NTA2$Country == "Singapore",]
health_data$Country <- NULL
pop_data <- NTA2[NTA2$Variable.Name == "Population, Total" & NTA2$Country == "United Kingdom",]
pop_data$Country <- NULL
combined_data <- merge(health_data, pop_data, by = c("Age_Group", "Year"))
combined_data$Value <- combined_data$Value.x * combined_data$Value.y
combined_data$Variable.Name <- "Health*Population"
combined_data <- combined_data[, c("Year", "Variable.Name", "Age_Group", "Value")]
combined_data$Country <- "Singapore with United Kingdom Population"
combined_data <- combined_data[,c("Country", "Year", "Variable.Name", "Age_Group", "Value")]
NTA4 <- bind_rows(NTA3, combined_data)


# Summary table 
summary_table <- data.frame("Country" = c("United Kingdom", "Singapore", "Singapore with United Kingdom Population"),
                            "Total_Population*Health" = c(sum(NTA4[NTA4$Country == "United Kingdom" & NTA4$Variable.Name == "Health*Population",]$Value, na.rm = TRUE),
                                                          sum(NTA4[NTA4$Country == "Singapore" & NTA4$Variable.Name == "Health*Population",]$Value, na.rm = TRUE),
                                                          sum(NTA4[NTA4$Country == "Singapore with United Kingdom Population" & NTA4$Variable.Name == "Health*Population",]$Value, na.rm = TRUE)),
                            "Population" = c(sum(NTA4[NTA4$Country == "United Kingdom" & NTA4$Variable.Name == "Population, Total",]$Value, na.rm = TRUE),
                                             sum(NTA4[NTA4$Country == "Singapore" & NTA4$Variable.Name == "Population, Total",]$Value, na.rm = TRUE),
                                             sum(NTA4[NTA4$Country == "United Kingdom" & NTA4$Variable.Name == "Population, Total",]$Value, na.rm = TRUE)))

# Summary table per capita spend (local)
values <- numeric()
for(i in 1:3){
  values <- c(values, summary_table[i, "Total_Population.Health"]/summary_table[i, "Population"])
    
}
summary_table$Per_Capita_Spend_Local <- values

# Summary table per capita spend(USD)
summary_table$Currency_Conversion <- c(1.23, 0.84, 0.84)
values <- numeric()
for(i in 1:3){
  USD <- summary_table[i,"Per_Capita_Spend_Local"] * summary_table[i, "Currency_Conversion"]
  values <- c(values, USD)
}
summary_table$Per_Capita_Spend_USD <- values

summary_table