# Loading necessary libraries
library(ggplot2)
library(dplyr)

life_expectancy <- read.csv("inequality-in-life-expectancy.csv")
life_expectancy <- read.csv("inequality-in-life-expectancy.csv")
JBM_peers <- c("GBR", "AUT", "CAN", "DNK", "DEU", "FIN", "FRA", "NLD", "NOR", "SWE", "CHE", "SGP", "HKG", "USA")

# Filtering the data for JBM_peers countries and years from 2010 to 2019
filtered_data <- life_expectancy %>%
  filter(Code %in% JBM_peers & 
           Year >= 2010 & Year <= 2019)

# Filter data for the year 2019
data_2019 <- filtered_data %>%
  filter(Year == 2019)

# Rank countries based on Inequality in Life Expectancy
ranked_data_2019 <- data_2019 %>%
  arrange(Inequality.in.life.expectancy) %>%
  mutate(rank = row_number())

# Display the ranked data
ranked_data_2019[, c("Entity", "Inequality.in.life.expectancy", "rank")]
