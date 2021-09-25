library(fabricatr)
library(randomizr)
library(DeclareDesign)
library(tidyverse)
library(ggplot2)
library(Rmisc)
library(data.table)
library(ggpubr)

# Load data ---------------------------------------------------------------
fake <- read_csv("Janmohamed Project/Data/preAnalysis.csv")
# Add a column with mean masss by cluster
fake <- fake %>%
  group_by(cluster) %>%
  mutate(meanY_mass24 = mean(Y_mass24),
         meanY_mass48 = mean(Y_mass48),
         meanY_mass72 = mean(Y_mass72))

# Effect on length --------------------------------------------------------
# Day 1
lm_robust(Y_length24 ~ Z, clusters = cluster, data = fake, subset = s24 == 1)
lm_robust(Y_length24 ~ Z + N_seeds_per_cluster, clusters = cluster, data = fake, subset = s24 == 1)
# Day 2
lm_robust(Y_length48 ~ Z, clusters = cluster, data = fake, subset = s48 == 1)
lm_robust(Y_length48 ~ Z + N_seeds_per_cluster, clusters = cluster, data = fake, subset = s48 == 1)
# Day 3
lm_robust(Y_length72 ~ Z, clusters = cluster, data = fake, subset = s72 == 1)
lm_robust(Y_length72 ~ Z + N_seeds_per_cluster, clusters = cluster, data = fake, subset = s72 == 1)

# Effect on width --------------------------------------------------------
# Day 1
lm_robust(Y_width24 ~ Z, clusters = cluster, data = fake, subset = s24 == 1)
lm_robust(Y_width24 ~ Z + N_seeds_per_cluster, clusters = cluster, data = fake, subset = s24 == 1)
# Day 2
lm_robust(Y_width48 ~ Z, clusters = cluster, data = fake, subset = s48 == 1)
lm_robust(Y_width48 ~ Z + N_seeds_per_cluster, clusters = cluster, data = fake, subset = s48 == 1)
# Day 3
lm_robust(Y_width72 ~ Z, clusters = cluster, data = fake, subset = s72 == 1)
lm_robust(Y_width72 ~ Z + N_seeds_per_cluster, clusters = cluster, data = fake, subset = s72 == 1)

# Effect on mass --------------------------------------------------------
# Day 1
lm_robust(meanY_mass24 ~ Z, clusters = cluster, data = fake, subset = s24 == 1)
lm_robust(meanY_mass24 ~ Z + N_seeds_per_cluster, clusters = cluster, data = fake, subset = s24 == 1)
# Day 2
lm_robust(meanY_mass48 ~ Z, clusters = cluster, data = fake, subset = s48 == 1)
lm_robust(meanY_mass48 ~ Z + N_seeds_per_cluster, clusters = cluster, data = fake, subset = s48 == 1)
# Day 3
lm_robust(meanY_mass72 ~ Z, clusters = cluster, data = fake, subset = s72 == 1)
lm_robust(meanY_mass72 ~ Z + N_seeds_per_cluster, clusters = cluster, data = fake, subset = s72 == 1)


# Effect on qualitative approval score ------------------------------------
# Day 1
lm_robust(Y_meanPreference ~ Z, clusters = cluster, data = fake)
lm_robust(Y_meanPreference ~ Z + N_seeds_per_cluster, clusters = cluster, data = fake)


# Plots -------------------------------------------------------------------
v24 <- fake %>% 
  select(block, cluster, N_seeds_per_cluster, Y_width24, Y_length24, meanY_mass24, Y_meanPreference,s24, Z) %>%
  filter(s24 == 1)

v48 <- fake %>% 
  select(cluster, Y_width48, Y_length48, meanY_mass48,s48) %>%
  filter(s48 == 1)

v72 <- fake %>% 
  select(cluster, Y_width72, Y_length72, meanY_mass72,s72) %>%
  filter(s72 == 1)


fig <- inner_join(inner_join(v24, v48, by = "cluster"), v72, by = "cluster") %>%
  group_by(Z) %>%
  select(-c(block, cluster, N_seeds_per_cluster, s24, s48, s72)) %>%
  summarise_all("mean") %>%
  dplyr::rename("Width, 24hrs (cm)" = "Y_width24",
         "Width, 48hrs (cm)" = "Y_width48",
         "Width, 72hrs (cm)" = "Y_width72",
         "Length, 24hrs (cm)" = "Y_length24",
         "Length, 48hrs (cm)" = "Y_length48",
         "Length, 72hrs (cm)" = "Y_length72",
         "Average mass, 24hrs (g)" = "meanY_mass24",
         "Average mass, 48hrs (g)" = "meanY_mass48",
         "Average mass, 72hrs (g)" = "meanY_mass72",
         "Mean approval rating" = "Y_meanPreference", 
         "Assignment" = "Z")
fig <- fig[, c("Assignment", "Width, 24hrs (cm)", "Width, 48hrs (cm)", "Width, 72hrs (cm)" , "Length, 24hrs (cm)", "Length, 48hrs (cm)", "Length, 72hrs (cm)", "Average mass, 24hrs (g)", "Average mass, 48hrs (g)", "Average mass, 72hrs (g)", "Mean approval rating")]

fig <- gather(fig, factor_key = T) %>%
  mutate(Z = factor(rep(c(0,1), times = 11))) %>%
  filter(key != "Assignment") 

ggplot(fig, aes(key, value, fill = Z)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  facet_wrap(~key, scales = "free", nrow = 4) +
  scale_fill_discrete(name = "Assignment", labels = c("Control", "Treatment")) +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(), 
        axis.title.x = element_blank(), 
        legend.position = "bottom")
ggsave("Janmohamed Project/Figures/preanalysisMeans.pdf", width=9, height=6)


# Group mean graph data -------------------------------------------
v24 <- fake %>% 
  select(block, cluster, N_seeds_per_cluster, Y_width24, Y_length24, s24, Z) %>%
  filter(s24 == 1) %>%
  dplyr::rename(width = Y_width24,
         length = Y_length24) %>%
  mutate(day = 1, 
         rating = NA) %>%
  select(-c(s24))

v48 <- fake %>% 
  select(cluster, cluster, N_seeds_per_cluster, Y_width48, Y_length48 ,s48, Z) %>%
  filter(s48 == 1) %>%
  dplyr::rename(width = Y_width48,
                length = Y_length48) %>%
  mutate(day = 2, 
         rating = NA) %>%
  select(-c(s48))

v72 <- fake %>% 
  select(cluster, cluster, N_seeds_per_cluster, Y_width72, Y_length72 ,s72, Z, Y_meanPreference) %>%
  filter(s72 == 1) %>%
  dplyr::rename(width = Y_width72,
                length = Y_length72,
                rating = Y_meanPreference) %>%
  mutate(day = 3) %>%
  select(-c(s72))

fig <- rbind(rbind(v24, v48), v72) %>%
  dplyr::rename(Assignment = Z) %>%
  mutate(Assignment = case_when(Assignment == 0 ~ "Control mean",
                                Assignment == 1 ~ "Treatment mean"))

means <- fig %>%
  group_by(Assignment, day) %>%
  dplyr::summarise(meanW = mean(width), 
                   uciW = CI(width)[1], 
                   lciW = CI(width)[3],
                   meanL = mean(length), 
                   uciL = CI(length)[1], 
                   lciL = CI(length)[3],
                   meanR= mean(rating), 
                   uciR = CI(rating)[1], 
                   lciR = CI(rating)[3]) %>%
  ungroup() %>%
  add_row(Assignment = "Treatment effect") %>%
  mutate(day = as.factor(day),
         Assignment = as.factor(Assignment),
         Assignment = fct_relevel(Assignment, "Control mean", "Treatment mean")) %>%
  add_column(qual = NA) %>%
  group_by(Assignment, day) 

  

means$meanR[2] <- 0.8
means$meanR[5] <- 0.133
means$uciR[2] <- 1.03
means$uciR[5] <- 0.298
means$lciR[2] <- 0.571 
means$lciR[5] <- -0.0310

means$qual[3] <- "Width"
means$qual[6] <- "Width"
means$qual[2] <- "Length"
means$qual[5] <- "Length"

means$qual <- as.factor(means$qual)



widthMean <- means %>%
  ggplot(aes(x = day, y = meanW, color = Assignment)) +
  geom_point(position = position_dodge(0.2)) + 
  geom_errorbar(aes(ymin = lciW, ymax = uciW), position = position_dodge(0.2), width = 0.1) + 
  scale_color_manual(values = c("steelblue3", "red2", "black")) + 
  xlab("Day") + 
  ylab("Width (cm)") +
  theme_bw() + 
  theme(legend.position = "left",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

lengthMean <- means %>%
  ggplot(aes(x = day, y = meanL, color = Assignment)) +
  geom_point(position = position_dodge(0.2)) + 
  geom_errorbar(aes(ymin = lciL, ymax = uciL), position = position_dodge(0.2), width = 0.1) + 
  scale_color_manual(values = c("steelblue3", "red2", "black")) + 
  xlab("Day") + 
  ylab("Length (cm)") +
  theme_bw() + 
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ratingMean <- na.omit(means) %>%
  ggplot(aes(x = qual, y = meanR, color = Assignment)) +
  geom_point(position = position_dodge(0.2)) + 
  geom_errorbar(aes(ymin = lciR, ymax = uciR), position = position_dodge(0.2), width = 0.1) + 
  scale_fill_discrete(name = "Assignment", labels = c("Control", "Treatment")) +
  xlab("Day 3") + 
  ylab("Approval rating") +
  #scale_x_discrete(labels = NULL, breaks = NULL) + 
  theme_bw() + 
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


# Group estimate graph ----------------------------------------------------
estimates <- read_csv("Janmohamed Project/Data/estimates.csv")

widthE <- estimates %>%
  ggplot(aes(x = day, y = estW)) +
  geom_point(position = position_dodge(0.2)) + 
  geom_errorbar(aes(ymin = lciW, ymax = uciW), position = position_dodge(0.2), width = 0.1) + 
  geom_hline(yintercept = 0, linetype="dashed") + 
  scale_x_continuous(breaks=seq(0,3,1)) + 
  xlab("Day") + 
  ylab("Width (cm)") +
  theme_bw() + 
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

lengthE <- estimates %>%
  ggplot(aes(x = day, y = estL)) +
  geom_point(position = position_dodge(0.2)) + 
  geom_errorbar(aes(ymin = lciL, ymax = uciL), position = position_dodge(0.2), width = 0.1) + 
  geom_hline(yintercept = 0, linetype="dashed") + 
  scale_x_continuous(breaks=seq(0,3,1)) + 
  xlab("Day") + 
  ylab("Length (cm)") +
  theme_bw() + 
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ratingE <- na.omit(estimates) %>%
  ggplot(aes(x = qual, y = estR)) +
  geom_point(position = position_dodge(0.2)) + 
  geom_errorbar(aes(ymin = lciR, ymax = uciR), position = position_dodge(0.2), width = 0.1) + 
  geom_hline(yintercept = 0, linetype="dashed") + 
  xlab("Day 3") + 
  ylab("Approval rating") +
  #scale_x_discrete(labels = NULL, breaks = NULL) + 
  theme_bw() + 
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggarrange(widthMean, lengthMean, ratingMean, widthE, lengthE, ratingE, common.legend = TRUE, ncol = 3, nrow =2, legend = "bottom")
ggsave("Janmohamed Project/Figures/preAnalysisEst.pdf", width=9, height=6)
