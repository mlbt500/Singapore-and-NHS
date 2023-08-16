unique(NTA3$Variable.Name)
colnames(NTA3)
library(ggplot2)

unique(NTA3$Variable.Name)
unique(NTA3$Country)
unique(NTA3$Year)
unique(NTA3$Age_Group)

# 1. Filter out the rows for the three mentioned variables.
subset_data <- NTA3[NTA3$Variable.Name %in% c("Public Consumption, Health", "Private Consumption, Health", "Public and Private, Health"),]

# 2. Duplicate these rows
new_data <- subset_data

# 3. Modify the Value column based on the country's conversion rate
new_data$Value[new_data$Country == "Singapore"] <- new_data$Value[new_data$Country == "Singapore"]/1.5647
new_data$Value[new_data$Country == "United Kingdom"] <- new_data$Value[new_data$Country == "United Kingdom"]/0.7993

# 4. Append "USD" to the Variable.Name column
new_data$Variable.Name <- paste(new_data$Variable.Name, "USD")

# Singapore 
new_data$Age_Group <- gsub(pattern = "Age", replacement = "", x = new_data$Age_Group)
new_data$Age_Group <- as.numeric(new_data$Age_Group)
new_data_SG <- new_data[new_data$Country == "Singapore",]
new_data_UK <- new_data[new_data$Country == "United Kingdom",]
new_data_total <- new_data[new_data$Variable.Name == "Public and Private, Health USD", ]

ggplot(new_data_SG, aes(x = Age_Group, y = Value, color = Variable.Name, group = Variable.Name)) +
  geom_line(size = 1) + # this creates the lines
  labs(
    title = "Health Expenditures in Singapore (2013)",
    x = "Age Group",
    y = "Value (USD)",
    color = "Expenditure Type"
  ) +
  theme_minimal() + # gives a clean theme
  theme(legend.position="top") # puts the legend at the top


ggplot(new_data_UK, aes(x = Age_Group, y = Value, color = Variable.Name)) +
  geom_line() +
  labs(title = "Health Expenditure in the United Kingdom (2013)",
       x = "Age",
       y = "Expenditure (USD)",
       color = "Expenditure Type") +
  theme_minimal()

ggplot(new_data_total, aes(x = Age_Group, y = Value, color = Country)) +
  geom_line() +
  labs(title = "Total Health Expenditure (Public and Private) for Singapore and UK (2013)",
       x = "Age",
       y = "Expenditure (USD)",
       color = "Country") +
  theme_minimal()
