# Purpose: Clean raw data
library(tidyverse)

data <- read_csv("Janmohamed Project/Data/rawBeans.csv")

data <- data %>%
  #generate approval rating from qualitative responses
  mutate(meanQl = data %>% 
           select(lQ1, lQ2, lQ3) %>%
           rowMeans(.),
         meanQw = data %>% 
           select(wQ1, wQ2, wQ3) %>%
           rowMeans(.)) %>%
  #generate proportions sprouted per day
  mutate(prop48 = n48/`N seeds`, 
         prop72 = n72/`N seeds`,
         prop96 = n96/`N seeds`) %>%
  select(-c(n48, n72, n96, lQ1, lQ2, lQ3, wQ1, wQ2, wQ3, l24, w24))
write.csv(data, "Janmohamed Project/Data/cleanBeans.csv", row.names = F)
