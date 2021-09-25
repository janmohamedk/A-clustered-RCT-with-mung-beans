# Purpose: conduct difference in means analyses
library(tidyverse)
library(ggplot2)
library(Rmisc)
library(data.table)
library(ggpubr)
library(scales)
library(rsample)
library(estimatr)

# Load data ---------------------------------------------------------------
data <- read_csv("Janmohamed Project/Data/cleanBeans.csv")

# Calculate estimates ------------------------------------------------------
# Effect on quantitative length 
(lATE48 <- tidy(lm_robust(l48 ~ Z, clusters = Cluster, data = data)))
(lATE72 <- tidy(lm_robust(l72 ~ Z , clusters = Cluster, data = data)))
(lATE96 <- tidy(lm_robust(l96 ~ Z , clusters = Cluster, data = data)))  

# Effect on quantitative width
(wATE48 <- tidy(lm_robust(w48 ~ Z , clusters = Cluster, data = data)))
(wATE72 <- tidy(lm_robust(w72 ~ Z , clusters = Cluster, data = data)))
(wATE96 <- tidy(lm_robust(w96 ~ Z , clusters = Cluster, data = data)))

# Effect on proportion of seeds sprouted
(propATE48 <- tidy(lm_robust(prop48 ~ Z , clusters = Cluster, data = data)))
(propATE72 <- tidy(lm_robust(prop72 ~ Z , clusters = Cluster, data = data)))
(propATE96 <- tidy(lm_robust(prop96 ~ Z , clusters = Cluster, data = data)))

# Effect on qualitative length
(qLATE <- tidy(lm_robust(meanQl ~ Z , clusters = Cluster, data = data)))

# Effect on qualitative width
(qWATE <- tidy(lm_robust(meanQw ~ Z , clusters = Cluster, data = data)))

# Calculate covariate adjusted estimates ------------------------------------------------------
  # Effect on quantitative length 
(lATE48c <- tidy(lm_robust(l48 ~ Z + `N seeds`, clusters = Cluster, data = data)))
(lATE72c <- tidy(lm_robust(l72 ~ Z + `N seeds`, clusters = Cluster, data = data)))
(lATE96c <- tidy(lm_robust(l96 ~ Z + `N seeds`, clusters = Cluster, data = data)))  

# Effect on quantitative width
(wATE48c <- tidy(lm_robust(w48 ~ Z + `N seeds`, clusters = Cluster, data = data)))
(wATE72c <- tidy(lm_robust(w72 ~ Z + `N seeds`, clusters = Cluster, data = data)))
(wATE96c <- tidy(lm_robust(w96 ~ Z + `N seeds`, clusters = Cluster, data = data)))

# Effect on proportion of seeds sprouted
(propATE48c <- tidy(lm_robust(prop48 ~ Z + `N seeds`, clusters = Cluster, data = data)))
(propATE72c <- tidy(lm_robust(prop72 ~ Z + `N seeds`, clusters = Cluster, data = data)))
(propATE96c <- tidy(lm_robust(prop96 ~ Z + `N seeds`, clusters = Cluster, data = data)))

# Effect on qualitative length
(qLATEc <- tidy(lm_robust(meanQl ~ Z + `N seeds`, clusters = Cluster, data = data)))

# Effect on qualitative width
(qWATEc <- tidy(lm_robust(meanQw ~ Z + `N seeds`, clusters = Cluster, data = data)))


# Calculate estimates with interaction ------------------------------------
(lATE48i <- tidy(lm_robust(l48 ~ Z + `N seeds` + `N seeds`*Z, clusters = Cluster, data = data)))
(lATE72i <- tidy(lm_robust(l72 ~ Z + `N seeds` + `N seeds`*Z, clusters = Cluster, data = data)))
(lATE96i <- tidy(lm_robust(l96 ~ Z + `N seeds` + `N seeds`*Z, clusters = Cluster, data = data)))  

# Effect on quantitative width
(wATE48i <- tidy(lm_robust(w48 ~ Z + `N seeds` + `N seeds`*Z, clusters = Cluster, data = data)))
(wATE72i <- tidy(lm_robust(w72 ~ Z + `N seeds` + `N seeds`*Z, clusters = Cluster, data = data)))
(wATE96i <- tidy(lm_robust(w96 ~ Z + `N seeds` + `N seeds`*Z, clusters = Cluster, data = data)))

# Effect on proportion of seeds sprouted
(propATE48i <- tidy(lm_robust(prop48 ~ Z + `N seeds` + `N seeds`*Z, clusters = Cluster, data = data)))
(propATE72i <- tidy(lm_robust(prop72 ~ Z + `N seeds` + `N seeds`*Z, clusters = Cluster, data = data)))
(propATE96i <- tidy(lm_robust(prop96 ~ Z + `N seeds` + `N seeds`*Z, clusters = Cluster, data = data)))

# Effect on qualitative length
(qLATEi <- tidy(lm_robust(meanQl ~ Z + `N seeds` + `N seeds`*Z, clusters = Cluster, data = data)))

# Effect on qualitative width
(qWATEi <- tidy(lm_robust(meanQw ~ Z + `N seeds` + `N seeds`*Z, clusters = Cluster, data = data)))

# Comparing treatment effects over time -----------------------------------
# Length 96 vs 48
experimentalL <- with(lATE96c, estimate[term == "Z"]) - with(lATE48c, estimate[term == "Z"])

estimates <- vector()
for (i in 1:10000){
  sample <- data[sample(1:nrow(data), replace = T), ]
  
  l96 <- tidy(lm_robust(l96 ~ Z + `N seeds`, clusters = Cluster, data = sample))
  l48 <- tidy(lm_robust(l48 ~ Z + `N seeds`, clusters = Cluster, data = sample))
  
  est <- (with(l96, estimate[term == "Z"]) - with(l48, estimate[term == "Z"]))
  estimates <- c(estimates, est)
}
# GET CI
quantile(estimates, 0.025)
quantile(estimates, 0.975)
sd(estimates)

# Length 96 vs 72
experimentalL <- with(lATE96c, estimate[term == "Z"]) - with(lATE72c, estimate[term == "Z"])

estimates <- vector()
for (i in 1:10000){
  sample <- data[sample(1:nrow(data), replace = T), ]
  
  l96 <- tidy(lm_robust(l96 ~ Z + `N seeds`, clusters = Cluster, data = sample))
  l72 <- tidy(lm_robust(l72 ~ Z + `N seeds`, clusters = Cluster, data = sample))
  
  est <- (with(l96, estimate[term == "Z"]) - with(l72, estimate[term == "Z"]))
  estimates <- c(estimates, est)
}
# GET CI
quantile(estimates, 0.025)
quantile(estimates, 0.975)
sd(estimates)

# Length 72 vs 48
experimentalW <- with(lATE72c, estimate[term == "Z"]) - with(lATE48c, estimate[term == "Z"])

estimates <- vector()
for (i in 1:10000){
  sample <- data[sample(1:nrow(data), replace = T), ]
  
  l72 <- tidy(lm_robust(l72 ~ Z + `N seeds`, clusters = Cluster, data = sample))
  l48 <- tidy(lm_robust(l48 ~ Z + `N seeds`, clusters = Cluster, data = sample))
  
  est <- (with(l72, estimate[term == "Z"]) - with(l48, estimate[term == "Z"]))
  estimates <- c(estimates, est)
}
# GET CI
quantile(estimates, 0.025)
quantile(estimates, 0.975)
sd(estimates)

# Width 96 vs 48
experimentalW <- with(wATE96c, estimate[term == "Z"]) - with(wATE48c, estimate[term == "Z"])

estimates <- vector()
for (i in 1:10000){
  sample <- data[sample(1:nrow(data), replace = T), ]
  
  w96 <- tidy(lm_robust(w96 ~ Z + `N seeds`, clusters = Cluster, data = sample))
  w48 <- tidy(lm_robust(w48 ~ Z + `N seeds`, clusters = Cluster, data = sample))
  
  est <- (with(w96, estimate[term == "Z"]) - with(w48, estimate[term == "Z"]))
  estimates <- c(estimates, est)
}
# GET CI
quantile(estimates, 0.025)
quantile(estimates, 0.975)
sd(estimates)

# Width 96 vs 72
experimentalW <- with(wATE96c, estimate[term == "Z"]) - with(wATE72c, estimate[term == "Z"])

estimates <- vector()
for (i in 1:10000){
  sample <- data[sample(1:nrow(data), replace = T), ]
  
  w96 <- tidy(lm_robust(w96 ~ Z + `N seeds`, clusters = Cluster, data = sample))
  w72 <- tidy(lm_robust(w72 ~ Z + `N seeds`, clusters = Cluster, data = sample))
  
  est <- (with(w96, estimate[term == "Z"]) - with(w72, estimate[term == "Z"]))
  estimates <- c(estimates, est)
}
# GET CI
quantile(estimates, 0.025)
quantile(estimates, 0.975)
sd(estimates)

# Width 72 vs 48
experimentalW <- with(wATE72c, estimate[term == "Z"]) - with(wATE48c, estimate[term == "Z"])

estimates <- vector()
for (i in 1:10000){
  sample <- data[sample(1:nrow(data), replace = T), ]
  
  w72 <- tidy(lm_robust(w72 ~ Z + `N seeds`, clusters = Cluster, data = sample))
  w48 <- tidy(lm_robust(w48 ~ Z + `N seeds`, clusters = Cluster, data = sample))
  
  est <- (with(w72, estimate[term == "Z"]) - with(w48, estimate[term == "Z"]))
  estimates <- c(estimates, est)
}
# GET CI
quantile(estimates, 0.025)
quantile(estimates, 0.975)
sd(estimates)
