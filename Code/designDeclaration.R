library(fabricatr)
library(randomizr)
library(DeclareDesign)
library(tidyverse)

N_blocks <- 15
N_clusters_in_block <- 2
N_clusters <- N_blocks * N_clusters_in_block
N_seeds_in_cluster <- rep(sequence(15, from = 2, by = 2), each = 2)
N_seeds <- sum(N_seeds_in_cluster)


planA <- declare_model(block = add_level(N = N_blocks),
                       cluster = add_level(N = N_clusters_in_block,
                                           N_seeds_per_cluster = N_seeds_in_cluster,
                                           # Cluster level preferences (independent, blinded reviewers)
                                           Y0_preference_r1 = sample(c(0,0.5,1), N_clusters, replace = T, prob = c(0.70, 0.10, 0.20)),
                                           Y1_preference_r1 = (1 - Y0_preference_r1),
                                           Y0_preference_r2 = sample(c(0,0.5,1), N_clusters, replace = T, prob = c(0.70, 0.10, 0.20)),
                                           Y1_preference_r2 = (1 - Y0_preference_r2),
                                           # The actual potential outcome is the mean of reviewer scores
                                           Y0_meanPreference = (Y0_preference_r1 + Y0_preference_r2)/2,
                                           Y1_meanPreference = (Y1_preference_r1 + Y1_preference_r2)/2),
                       seeds = add_level(N = N_seeds_in_cluster,
                                         #mass gain measured in the control group at 24, 48 and 72 hour marks
                                         Y0_mass24 = runif(N_seeds, 0.5, 1),
                                         Y0_mass48 = Y0_mass24  + runif(N_seeds, 0.5, 1),
                                         Y0_mass72 = Y0_mass48  + runif(N_seeds, 0.5, 1),
                                         #mass gain measured in the treatment group at 24, 48 and 72 hour marks
                                         Y1_mass24 = runif(N_seeds, 0.75, 1.5),
                                         Y1_mass48 = Y1_mass24 + runif(N_seeds, 0.75, 1.5),
                                         Y1_mass72 = Y1_mass48 + runif(N_seeds, 0.75, 1.5),
                                         # shoot length (cm) in the control group at 24, 48 and 72 hour marks
                                         Y0_length24 = runif(N_seeds, 0.25, 0.5),
                                         Y0_length48 = runif(N_seeds, 0.5, 0.9),
                                         Y0_length72 = runif(N_seeds, 0.9, 1.5),
                                         # shoot length (cm) in the treatment group at 24, 48 and 72 hour marks
                                         Y1_length24 = runif(N_seeds, 0.1, 0.3),
                                         Y1_length48 = runif(N_seeds, 0.3, 0.7),
                                         Y1_length72 = runif(N_seeds, 0.7, 1.2),
                                         # shoot width (cm) in the control group at 24, 48 and 72 hour marks
                                         Y0_width24 = runif(N_seeds, 0.0, 0.2),
                                         Y0_width48 = runif(N_seeds, 0.2, 0.3),
                                         Y0_width72 = runif(N_seeds, 0.3, 0.4),
                                         # shoot width (cm) in the treatment group at 24, 48 and 72 hour marks
                                         Y1_width24 = runif(N_seeds, 0.0, 0.3),
                                         Y1_width48 = runif(N_seeds, 0.3, 0.5),
                                         Y1_width72 = runif(N_seeds, 0.5, 0.7))) +
  declare_assignment(Z = block_and_cluster_ra(clusters = cluster, blocks = block),
                     s24 = strata_rs(strata = cluster, n = 1),
                     s48 = strata_rs(strata = cluster, n = 1), 
                     s72 = strata_rs(strata = cluster, n = 1), legacy = F) + 
  declare_inquiry (ATE_mass24 = mean(Y1_mass24 - Y0_mass24)) +
  declare_inquiry (ATE_mass48 = mean(Y1_mass48 - Y0_mass48)) +
  declare_inquiry (ATE_mass72 = mean(Y1_mass72 - Y0_mass72)) +
  declare_inquiry (ATE_length24 =  mean(Y1_length24 - Y0_length24)) +
  declare_inquiry (ATE_length48 =  mean(Y1_length48 - Y0_length48)) +
  declare_inquiry (ATE_length72 =  mean(Y1_length72 - Y0_length72)) +
  declare_inquiry (ATE_width24 =  mean(Y1_width24 - Y0_width24)) +
  declare_inquiry (ATE_width48 =  mean(Y1_width48 - Y0_width48)) +
  declare_inquiry (ATE_width72 =  mean(Y1_width72 - Y0_width72)) +
  declare_inquiry (ATE_preference = mean(Y1_meanPreference - Y0_meanPreference)) +
  declare_measurement(Y_mass24 = case_when(Z == 0 ~ Y0_mass24,
                                           Z == 1 ~ Y1_mass24)) +
  declare_measurement(Y_mass48 = case_when(Z == 0 ~ Y0_mass48,
                                           Z == 1 ~ Y1_mass48)) +
  declare_measurement(Y_mass72 = case_when(Z == 0 ~ Y0_mass72,
                                           Z == 1 ~ Y1_mass72)) +
  declare_measurement(Y_length24 = case_when(Z == 0  ~ Y0_length24,
                                             Z == 1 ~ Y1_length24)) +
  declare_measurement(Y_length48 = case_when(Z == 0 ~ Y0_length48,
                                             Z == 1 ~ Y1_length48)) +
  declare_measurement(Y_length72 = case_when(Z == 0 ~ Y0_length72,
                                             Z == 1 ~ Y1_length72)) +
  declare_measurement(Y_width24 = case_when(Z == 0  ~ Y0_width24,
                                            Z == 1 ~ Y1_width24)) +
  declare_measurement(Y_width48 = case_when(Z == 0 ~ Y0_width48,
                                            Z == 1 ~ Y1_width48)) +
  declare_measurement(Y_width72 = case_when(Z == 0 ~ Y0_width72,
                                            Z == 1 ~ Y1_width72)) +
  declare_measurement(Y_preference_r1 = case_when(Z == 0 ~ Y0_preference_r1,
                                               Z == 1 ~ Y1_preference_r1)) +
  declare_measurement(Y_preference_r2 = case_when(Z == 0 ~ Y0_preference_r2,
                                                  Z == 1 ~ Y1_preference_r2)) +
  declare_measurement(Y_meanPreference = 0.5*(Y_preference_r1 + Y_preference_r1)) +
  declare_estimator(Y_mass24 ~ Z, inquiry = "ATE_mass24", clusters = cluster,
                    model = lm_robust, label = "DIM mass 24") +
  declare_estimator(Y_mass48 ~ Z, inquiry = "ATE_mass48", clusters = cluster,
                    model = lm_robust, label = "DIM mass 48") +
  declare_estimator(Y_mass72 ~ Z, inquiry = "ATE_mass72", clusters = cluster,
                    model = lm_robust, label = "DIM mass 72") +
  declare_estimator(Y_mass24 ~ Z + N_seeds_per_cluster, inquiry = "ATE_mass24", clusters = cluster,
                    model = lm_robust, label = "DIM mass 24 cov") +
  declare_estimator(Y_mass48 ~ Z + N_seeds_per_cluster, inquiry = "ATE_mass48", clusters = cluster,
                    model = lm_robust, label = "DIM mass 48 cov") +
  declare_estimator(Y_mass72 ~ Z + N_seeds_per_cluster, inquiry = "ATE_mass72", clusters = cluster,
                    model = lm_robust, label = "DIM mass 72 cov") +
  declare_estimator(Y_length24 ~ Z, inquiry = "ATE_length24", clusters = cluster,
                    model = lm_robust, subset = s24 == 1,label = "DIM length 24") +
  declare_estimator(Y_length48 ~ Z, inquiry = "ATE_length48", clusters = cluster,
                    model = lm_robust, subset = s48 == 1,label = "DIM length 48") +
  declare_estimator(Y_length72 ~ Z, inquiry = "ATE_length72", clusters = cluster,
                    model = lm_robust, subset = s72 == 1,label = "DIM length 72") +
  declare_estimator(Y_length24 ~ Z + N_seeds_per_cluster, inquiry = "ATE_length24", clusters = cluster,
                    model = lm_robust, subset = s24 == 1,label = "DIM length 24 cov") +
  declare_estimator(Y_length48 ~ Z + N_seeds_per_cluster, inquiry = "ATE_length48", clusters = cluster,
                    model = lm_robust, subset = s48 == 1,label = "DIM length 48 cov") +
  declare_estimator(Y_length72 ~ Z + N_seeds_per_cluster, inquiry = "ATE_length72", clusters = cluster,
                    model = lm_robust, subset = s72 == 1,label = "DIM length 72 cov") +
  declare_estimator(Y_width24 ~ Z, inquiry = "ATE_width24", clusters = cluster,
                    model = lm_robust, subset = s24 == 1,label = "DIM width 24") +
  declare_estimator(Y_width48 ~ Z, inquiry = "ATE_width48", clusters = cluster,
                    model = lm_robust, subset = s48 == 1,label = "DIM width 48") +
  declare_estimator(Y_width72 ~ Z, inquiry = "ATE_width72", clusters = cluster,
                    model = lm_robust, subset = s72 == 1, label = "DIM width 72") +
  declare_estimator(Y_width24 ~ Z + N_seeds_per_cluster, inquiry = "ATE_width24", clusters = cluster,
                    model = lm_robust, subset = s24 == 1,label = "DIM width 24 cov") +
  declare_estimator(Y_width48 ~ Z + N_seeds_per_cluster, inquiry = "ATE_width48", clusters = cluster,
                    model = lm_robust, subset = s48 == 1,label = "DIM width 48 cov") +
  declare_estimator(Y_width72 ~ Z + N_seeds_per_cluster, inquiry = "ATE_width72", clusters = cluster,
                    model = lm_robust, subset = s72 == 1,label = "DIM width 72 cov") +
  declare_estimator(Y_meanPreference ~ Z, inquiry = ('ATE_preference'), clusters = cluster, model = lm_robust, label = "DIM preference") + 
  declare_estimator(Y_meanPreference ~ Z + N_seeds_per_cluster, inquiry = ('ATE_preference'), clusters = cluster, model = lm_robust, label = "DIM preference cov")

run_design(planA)
diagnose_design(planA)
dataA <- draw_data(planA)
write.csv(dataA, "Janmohamed Project/Data/preAnalysis.csv", row.names = F)
