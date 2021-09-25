# Purpose: visualise group means and treatment effects
library(tidyverse)
library(ggplot2)
library(Rmisc)
library(data.table)
library(ggpubr)
library(scales)

# Load data ---------------------------------------------------------------
data <- read_csv("Janmohamed Project/Data/cleanBeans.csv")


# Calculate group means by outcome ----------------------------------------
# length
(l48 <- data %>%
   group_by(Z) %>%
   do(tidy(lm_robust(l48 ~ 1, data = .))) %>%
   as.data.frame())

(l72 <- data %>%
    group_by(Z) %>%
    do(tidy(lm_robust(l72 ~ 1, data = .))) %>%
    as.data.frame())

(l96 <- data %>%
    group_by(Z) %>%
    do(tidy(lm_robust(l96 ~ 1, data = .))) %>%
    as.data.frame())

# width
(w48 <- data %>%
    group_by(Z) %>%
    do(tidy(lm_robust(w48 ~ 1, data = .))) %>%
    as.data.frame())

(w72 <- data %>%
    group_by(Z) %>%
    do(tidy(lm_robust(w72 ~ 1, data = .))) %>%
    as.data.frame())

(w96 <- data %>%
    group_by(Z) %>%
    do(tidy(lm_robust(w96 ~ 1, data = .))) %>%
    as.data.frame())

# proportion of seeds sprouted
(prop48 <- data %>%
    group_by(Z) %>%
    do(tidy(lm_robust(prop48 ~ 1, data = .))) %>%
    as.data.frame())

(prop72 <- data %>%
    group_by(Z) %>%
    do(tidy(lm_robust(prop72 ~ 1, data = .))) %>%
    as.data.frame())

(prop96 <- data %>%
    group_by(Z) %>%
    do(tidy(lm_robust(prop96 ~ 1, data = .))) %>%
    as.data.frame())

# qualitative length
(qL <- data %>%
    group_by(Z) %>%
    do(tidy(lm_robust(meanQl ~ 1, data = .))) %>%
    as.data.frame())

# qualitative width
(qW <- data %>%
    group_by(Z) %>%
    do(tidy(lm_robust(meanQw ~ 1, data = .))) %>%
    as.data.frame())


# Make group means table for visualisation --------------------------------
assignment <- c(rep('Control mean', 3), rep('Treatment mean', 3), "Treatment effect")
day <- c(2,3,4,2,3,4, NA)
qual <- c(NA, "Length", "Width", NA, "Length", "Width", NA)

meanW <- c(w48[1,3], w72[1,3], w96[1,3], w48[2,3], w72[2,3], w96[2,3], NA)
lciW <- c(w48[1,7], w72[1,7], w96[1,7], w48[2,7], w72[2,7], w96[2,7], NA)
uciW <- c(w48[1,8], w72[1,8], w96[1,8], w48[2,8], w72[2,8], w96[2,8], NA)
meanL <- c(l48[1,3], l72[1,3], l96[1,3], l48[2,3], l72[2,3], l96[2,3], NA)
lciL <- c(l48[1,7], l72[1,7], l96[1,7], l48[2,7], l72[2,7], l96[2,7], NA)
uciL <- c(l48[1,8], l72[1,8], l96[1,8], l48[2,8], l72[2,8], l96[2,8], NA)
meanP <- c(prop48[1,3], prop72[1,3], prop96[1,3], prop48[2,3], prop72[2,3], prop96[2,3], NA)
lciP <- c(prop48[1,7], prop72[1,7], prop96[1,7], prop48[2,7], prop72[2,7], prop96[2,7], NA)
uciP <- c(prop48[1,8], prop72[1,8], prop96[1,8], prop48[2,8], prop72[2,8], prop96[2,8], NA)
meanQ <- c(NA, qL[1,3], qW[1,3], NA, qL[2,3], qW[2,3], NA)
lciQ <- c(NA, qL[1,7], qW[1,7], NA, qL[2,7], qW[2,7], NA)
uciQ <- c(NA, qL[1,8], qW[1,8], NA, qL[2,8], qW[2,8], NA)

means <- cbind(meanW, lciW, uciW, meanL, lciL, uciL, meanP, lciP, uciP, meanQ, lciQ, uciQ) %>%
  as.data.frame() %>%
  mutate_if(is.numeric, round, digits = 3) %>%
  mutate(assignment = fct_relevel(as_factor(assignment), "Control mean", "Treatment mean"),
         day = day,
         qual = qual)

propMean <- means %>%
  ggplot(aes(x = day, y = meanP, color = assignment)) +
  geom_point(position = position_dodge(0.2)) + 
  geom_errorbar(aes(ymin = lciP, ymax = uciP), position = position_dodge(0.2), width = 0.1) +
  scale_x_continuous(breaks=seq(0,4,1)) + 
  scale_y_continuous(breaks=seq(0.2,0.6,0.05)) + 
  scale_color_manual(values = c("steelblue3", "red2", "black")) + 
  xlab("Day") + 
  ylab("Proportion of seeds sprouted") +
  theme_bw() + 
  theme(legend.position = "left",
        legend.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
  
widthMean <- means %>%
  ggplot(aes(x = day, y = meanW, color = assignment)) +
  geom_point(position = position_dodge(0.2)) + 
  geom_errorbar(aes(ymin = lciW, ymax = uciW), position = position_dodge(0.2), width = 0.1) +
  scale_x_continuous(breaks=seq(0,4,1)) + 
  scale_y_continuous(breaks=seq(0,0.3,0.05)) + 
  scale_color_manual(values = c("steelblue3", "red2", "black")) + 
  xlab("Day") + 
  ylab("Width (cm)") +
  theme_bw() + 
  theme(legend.position = "left",
        legend.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

lengthMean <- means %>%
  ggplot(aes(x = day, y = meanL, color = assignment)) +
  geom_point(position = position_dodge(0.2)) + 
  geom_errorbar(aes(ymin = lciL, ymax = uciL), position = position_dodge(0.2), width = 0.1) + 
  scale_x_continuous(breaks=seq(0,4,1)) + 
  scale_y_continuous(breaks=seq(0,2.5,0.5)) + 
  scale_color_manual(values = c("steelblue3", "red2", "black")) + 
  xlab("Day") + 
  ylab("Length (cm)") +
  theme_bw() + 
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ratingMean <- na.omit(means) %>%
  ggplot(aes(x = qual, y = meanQ, color = assignment)) +
  geom_point(position = position_dodge(0.2)) + 
  geom_errorbar(aes(ymin = lciQ, ymax = uciQ), position = position_dodge(0.2), width = 0.1) + 
  xlab("Day 4") + 
  scale_color_manual(values = c("steelblue3", "red2", "black")) + 
  ylab("Approval rating") +
  #scale_x_discrete(labels = NULL, breaks = NULL) + 
  theme_bw() + 
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
# Calculate estimates ------------------------------------------------------
# Effect on quantitative length 
(lATE48 <- tidy(lm_robust(l48 ~ Z + `N seeds`, clusters = Cluster, data = data)))
(lATE72 <- tidy(lm_robust(l72 ~ Z + `N seeds`, clusters = Cluster, data = data)))
(lATE96 <- tidy(lm_robust(l96 ~ Z + `N seeds`, clusters = Cluster, data = data)))  

# Effect on quantitative width
(wATE48 <- tidy(lm_robust(w48 ~ Z + `N seeds`, clusters = Cluster, data = data)))
(wATE72 <- tidy(lm_robust(w72 ~ Z + `N seeds`, clusters = Cluster, data = data)))
(wATE96 <- tidy(lm_robust(w96 ~ Z + `N seeds`, clusters = Cluster, data = data)))

# Effect on proportion of seeds sprouted
(pATE48 <- tidy(lm_robust(prop48 ~ Z + `N seeds`, clusters = Cluster, data = data)))
(pATE72 <- tidy(lm_robust(prop72 ~ Z + `N seeds`, clusters = Cluster, data = data)))
(pATE96 <- tidy(lm_robust(prop96 ~ Z + `N seeds`, clusters = Cluster, data = data)))

# Effect on qualitative length
(qLATE <- tidy(lm_robust(meanQl ~ Z + `N seeds`, clusters = Cluster, data = data)))

# Effect on qualitative width
(qWATE <- tidy(lm_robust(meanQw ~ Z + `N seeds`, clusters = Cluster, data = data)))

# Prepare table of coefficients -------------------------------------------
day <- c(2,3,4)
estL <- c(lATE48[2,2], lATE72[2,2], lATE96[2,2])
uciL <- c(lATE48[2,6], lATE72[2,6], lATE96[2,6])
lciL <- c(lATE48[2,7], lATE72[2,7], lATE96[2,7])
estW <- c(wATE48[2,2], wATE72[2,2], wATE96[2,2])
uciW <- c(wATE48[2,6], wATE72[2,6], wATE96[2,6])
lciW <- c(wATE48[2,7], wATE72[2,7], wATE96[2,7])
estP <- c(pATE48[2,2], pATE72[2,2], pATE96[2,2])
uciP <- c(pATE48[2,6], pATE72[2,6], pATE96[2,6])
lciP <- c(pATE48[2,7], pATE72[2,7], pATE96[2,7])
estQ <- c(NA, qLATE[2,2], qWATE[2,2])
uciQ <- c(NA, qLATE[2,6], qWATE[2,6])
lciQ <- c(NA, qLATE[2,7], qWATE[2,7])
qualMeasure <- c(NA, "Length", "Width")

estimates <- as.data.frame(cbind(estL, uciL, lciL, estW, uciW, lciW, estP, lciP, uciP, estQ, lciQ, uciQ)) %>%
  mutate_if(is.character, as.numeric) %>%
  mutate_if(is.numeric, round, digits = 3) %>%
  mutate(qualMeasure = qualMeasure, 
         day = day)

propE <- estimates %>%
  filter(day != 1) %>%
  ggplot(aes(x = day, y = estP)) +
  geom_point(position = position_dodge(0.2), color = "black") + 
  geom_errorbar(aes(ymin = lciP, ymax = uciP), position = position_dodge(0.2), width = 0.1, color = "black") + 
  geom_hline(yintercept = 0, linetype="dashed") + 
  scale_y_continuous(breaks = seq(-0.1, 0.22, 0.05)) + 
  scale_x_continuous(breaks=seq(0,4,1)) + 
  xlab("Day") + 
  ylab("Proportion of seeds sprouted") +
  theme_bw() + 
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


widthE <- estimates %>%
  filter(day != 1) %>%
  ggplot(aes(x = day, y = estW)) +
  geom_point(position = position_dodge(0.2), color = "black") + 
  geom_errorbar(aes(ymin = lciW, ymax = uciW), position = position_dodge(0.2), width = 0.1, color = "black") + 
  geom_hline(yintercept = 0, linetype="dashed") + 
  scale_y_continuous(breaks = seq(0.0, 0.16, 0.02)) + 
  scale_x_continuous(breaks=seq(0,4,1)) + 
  xlab("Day") + 
  ylab("Width (cm)") +
  theme_bw() + 
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

lengthE <- estimates %>%
  filter(day != 1) %>%
  ggplot(aes(x = day, y = estL)) +
  geom_point(position = position_dodge(0.2), color = "black") + 
  geom_errorbar(aes(ymin = lciL, ymax = uciL), position = position_dodge(0.2), width = 0.1, color = "black") + 
  geom_hline(yintercept = 0, linetype="dashed") + 
  scale_y_continuous(breaks = seq(0, 1.5, 0.2)) +
  scale_x_continuous(breaks=seq(0,4,1)) + 
  xlab("Day") + 
  ylab("Length (cm)") +
  theme_bw() + 
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ratingE <- na.omit(estimates) %>%
  ggplot(aes(x = qualMeasure, y = estQ)) +
  geom_point(position = position_dodge(0.2), color = "black") + 
  geom_errorbar(aes(ymin = lciQ, ymax = uciQ), position = position_dodge(0.2), width = 0.1, color = "black") + 
  geom_hline(yintercept = 0, linetype="dashed") + 
  scale_y_continuous(breaks = seq(0, 1.2, 0.2)) +
  xlab("Day 3") + 
  ylab("Approval rating") +
  #scale_x_discrete(labels = NULL, breaks = NULL) + 
  theme_bw() + 
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


# Make plot ---------------------------------------------------------------
ggarrange(widthMean, lengthMean, ratingMean, widthE, lengthE, ratingE, common.legend = TRUE, ncol = 3, nrow =2, legend = "bottom")
ggsave("Janmohamed Project/Figures/estMeans.pdf", width=9, height=6)

ggarrange(propMean, propE, common.legend = TRUE, ncol = 2, nrow =1, legend = "bottom")
ggsave("Janmohamed Project/Figures/propFig.pdf", width=9, height=4)


