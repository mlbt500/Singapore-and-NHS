# I had to download this data manually
# it can be found on the National Transfer Accounts website -- select public and private health consumption; population total
# Singapore published the data for 2013 only
library(dplyr)
library(tidyverse)

read.csv("NTA.csv")
NTA_2013 <- NTA[NTA$Year == 2013 & NTA$Country %in% c("United Kingdom", "Singapore"),]
NTA_2013 <- NTA_2013[-c(5:6),]
NTA_2013_long <- NTA_2013 %>%
  pivot_longer(cols = starts_with("Age"),
               names_to = "Age_Group",
               values_to = "Value")
NTA_2013_long


