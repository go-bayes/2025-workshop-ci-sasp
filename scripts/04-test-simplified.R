# simplified test of script 04
library(grf)
library(tidyverse)
library(here)

# load data
df <- readRDS(here("data", "religion_hetero_sim.rds"))

# prepare data
X <- as.matrix(df[, c("age", "male", "urban_rural", "political_conservatism")])
Y <- df$t2_charity_donate_z
W <- df$t1_religious_service
weights <- df$census_weight

# fit simple causal forest
cat("Fitting causal forest...\n")
cf <- causal_forest(X, Y, W,
                   sample.weights = weights,
                   num.trees = 500,  # fewer trees for speed
                   honesty = TRUE,
                   seed = 2025)

# get ate
ate <- average_treatment_effect(cf)
cat(sprintf("ATE: %.3f (%.3f)\n", ate[1], ate[2]))

# simple subgroup
df$conservative <- df$political_conservatism > 5
conservatives <- which(df$conservative)

if (length(conservatives) > 100) {
  sub_ate <- average_treatment_effect(cf, subset = conservatives)
  cat(sprintf("Conservative ATE: %.3f (%.3f)\n", sub_ate[1], sub_ate[2]))
}

cat("Test completed successfully!\n")

