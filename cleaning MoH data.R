library(dplyr)
library(stringr)
library(tidyr)

#name columns
names(table) <- c("Values", "2018", "2019", "2020")
table1 <- table[-1,]

#filter out unnecessary row
table1 <- table1[c(2:3, 50:51, 98:99, 147:148, 151:154, 157:158, 161, 166:169),]

#clean values column
table1 <- table1 %>%
  mutate(Values = str_replace_all(Values, "\t", "")) %>%
  mutate(Values = str_replace_all(Values, "\n[0-9]+", "")) %>%
  mutate(Values = str_replace_all(Values, "►", "")) %>%
  mutate(Values = str_trim(Values))

#some variables were over two rows -- I simplified them
table1[2,1] <- table1[1,1]
table1[4,1] <- table1[3,1]
table[6,1] <- table1[5,1]
table1 <- table1[-c(1,3,5),]

#make columns 2 to 4 numeric
table1 <- table1 %>%
  mutate_at(vars(`2018`, `2019`, `2020`), ~str_replace_all(., ",", ""))
table1 <- table1 %>%
  mutate(`2018` = as.numeric(`2018`),
         `2019` = as.numeric(`2019`),
         `2020` = as.numeric(`2020`))

#mutiple certain rows by 1000 so they are all the same units
table2 <- table1
table2[13:16, 2:4] <- table2[13:16, 2:4] * 1000
table2 <- table2 %>%
  mutate(Values = str_extract(Values, "^[^\\(]+"))