# ==============================================================================
# simulation script: population estimates and causal inference
# adapted from quarto document
# ==============================================================================

# load required libraries --------------------------------------------------
library(tidyverse)
library(stdReg)
library(skimr)
library(parameters)
library(kableExtra)
library(gtsummary)
library(clarify)
library(grf)
library(glue)

# check for margot package
if (!require(margot, quietly = TRUE)) {
  devtools::install_github("go-bayes/margot")
  library("margot")
}

# ==============================================================================
# s2/s3: generalisability and transportability simulation
# ==============================================================================

# simulate data with different distributions of effect modifiers
# between sample and population
set.seed(123)

data <- margot::simulate_ate_data_with_weights(
  n_sample = 10000,
  n_population = 100000,
  p_z_sample = 0.1,
  p_z_population = 0.5,
  beta_a = 1,
  beta_z = 2.5,
  noise_sd = 0.5
)

# extract sample and population data
sample_data <- data$sample_data
population_data <- data$population_data

# check imbalance in effect modifier distribution
cat("\nSample distribution of effect modifier:\n")
print(table(sample_data$z_sample))

cat("\nPopulation distribution of effect modifier:\n")
print(table(population_data$z_population))

# model coefficients: sample
model_sample <- glm(y_sample ~ a_sample * z_sample,
  data = sample_data
)

cat("\n=== Model coefficients: Sample ===\n")
print(parameters::model_parameters(model_sample, ci_method = "wald"))

# model coefficients: weighted sample
model_weighted_sample <- glm(y_sample ~ a_sample * z_sample,
  data = sample_data, weights = weights
)

cat("\n=== Model coefficients: Weighted Sample ===\n")
print(parameters::model_parameters(model_weighted_sample, ci_method = "wald"))

# model coefficients: population
model_population <- glm(y_population ~ a_population * z_population,
  data = population_data
)

cat("\n=== Model coefficients: Population ===\n")
print(parameters::model_parameters(model_population, ci_method = "wald"))

# marginal effect estimates using stdreg
# sample ate
fit_std_sample <- stdReg::stdGlm(model_sample,
  data = sample_data, X = "a_sample"
)

cat("\n=== Sample ATE ===\n")
print(summary(fit_std_sample, contrast = "difference", reference = 0))

# population ate (oracle)
fit_std_population <- stdReg::stdGlm(model_population,
  data = population_data, X = "a_population"
)

cat("\n=== Population ATE (Oracle) ===\n")
print(summary(fit_std_population, contrast = "difference", reference = 0))

# weighted sample ate
fit_std_weighted_sample_weights <- stdReg::stdGlm(model_weighted_sample,
  data = sample_data, X = "a_sample"
)

cat("\n=== Weighted Sample ATE ===\n")
print(summary(fit_std_weighted_sample_weights, contrast = "difference", reference = 0))

# ==============================================================================
# s2/s3 alternative: manual ate calculation using predict function
# ==============================================================================

cat("\n=== Manual ATE Calculation Using Predict Function ===\n")
cat("This section explicitly shows what stdReg is doing under the hood\n\n")

# sample ate using predict
# create counterfactual datasets where everyone gets a = 0 and a = 1
sample_data_a0 <- sample_data %>% mutate(a_sample = 0)
sample_data_a1 <- sample_data %>% mutate(a_sample = 1)

# predict outcomes under each treatment level
y_pred_a0_sample <- predict(model_sample, newdata = sample_data_a0)
y_pred_a1_sample <- predict(model_sample, newdata = sample_data_a1)

# calculate ate as mean difference in predicted outcomes
ate_sample_manual <- mean(y_pred_a1_sample - y_pred_a0_sample)

cat("Sample ATE (manual calculation):", round(ate_sample_manual, 3), "\n")

# population ate using predict
population_data_a0 <- population_data %>% mutate(a_population = 0)
population_data_a1 <- population_data %>% mutate(a_population = 1)

y_pred_a0_population <- predict(model_population, newdata = population_data_a0)
y_pred_a1_population <- predict(model_population, newdata = population_data_a1)

ate_population_manual <- mean(y_pred_a1_population - y_pred_a0_population)

cat("Population ATE (manual calculation):", round(ate_population_manual, 3), "\n")

# weighted sample ate using predict
# use the sample model with weighted regression
y_pred_a0_weighted <- predict(model_weighted_sample, newdata = sample_data_a0)
y_pred_a1_weighted <- predict(model_weighted_sample, newdata = sample_data_a1)

ate_weighted_sample_manual <- mean(y_pred_a1_weighted - y_pred_a0_weighted)

cat("Weighted Sample ATE (manual calculation):", round(ate_weighted_sample_manual, 3), "\n")

# show the key insight: coefficients are the same but ates differ
cat("\n=== Key Insight ===\n")
cat("Notice that:\n")
cat("1. Model coefficients are nearly identical across sample, weighted sample, and population\n")
cat(
  "2. BUT the marginal ATEs differ between sample (", round(ate_sample_manual, 3),
  ") and population (", round(ate_population_manual, 3), ")\n"
)
cat("3. Weighting corrects this, giving ATE = ", round(ate_weighted_sample_manual, 3), "\n")
cat("4. This happens because the distribution of the effect modifier differs\n")
cat("   Sample: z=1 is rare; Population: z=1 is common\n\n")

# optional: show distribution of predicted potential outcomes
cat("=== Distribution of Predicted Potential Outcomes ===\n")

# sample
cat(
  "Sample - Mean Y(a=0):", round(mean(y_pred_a0_sample), 3),
  " | Mean Y(a=1):", round(mean(y_pred_a1_sample), 3), "\n"
)

# population
cat(
  "Population - Mean Y(a=0):", round(mean(y_pred_a0_population), 3),
  " | Mean Y(a=1):", round(mean(y_pred_a1_population), 3), "\n"
)

# weighted sample
cat(
  "Weighted Sample - Mean Y(a=0):", round(mean(y_pred_a0_weighted), 3),
  " | Mean Y(a=1):", round(mean(y_pred_a1_weighted), 3), "\n\n"
)

# ==============================================================================
# s4: cross-sectional data ambiguity simulation
# ==============================================================================

# simulate data where l is a mediator between a and y
set.seed(123)

n <- 1000
p <- 0.5
alpha <- 0
beta <- 2
gamma <- 1
delta <- 1.5
sigma_L <- 1
sigma_Y <- 1.5

# simulate the data: fully mediated effect by l
A <- rbinom(n, 1, p)
L <- alpha + beta * A + rnorm(n, 0, sigma_L)
Y <- gamma + delta * L + rnorm(n, 0, sigma_Y)

# create data frame
data_cross <- data.frame(A = A, L = L, Y = Y)

# fit regression models
# model 1: control for l (assuming l is confounder)
fit_1 <- lm(Y ~ A + L, data = data_cross)

# model 2: omit l (assuming l is mediator)
fit_2 <- lm(Y ~ A, data = data_cross)

# create comparison table
table1 <- gtsummary::tbl_regression(fit_1)
table2 <- gtsummary::tbl_regression(fit_2)

table_comparison <- gtsummary::tbl_merge(
  list(table1, table2),
  tab_spanner = c(
    "Model: L assumed confounder",
    "Model: L assumed mediator"
  )
)

cat("\n=== Cross-sectional Model Comparison ===\n")
print(as_kable_extra(table_comparison, format = "markdown"))

# calculate ate using clarify
set.seed(2025)
sim_coefs_fit_1 <- sim(fit_1)
sim_coefs_fit_2 <- sim(fit_2)

sim_est_fit_1 <- sim_ame(
  sim_coefs_fit_1,
  var = "A",
  subset = A == 1,
  contrast = "RD",
  verbose = FALSE
)

sim_est_fit_2 <- sim_ame(
  sim_coefs_fit_2,
  var = "A",
  subset = A == 1,
  contrast = "RD",
  verbose = FALSE
)

summary_sim_est_fit_1 <- summary(sim_est_fit_1, null = c(`RD` = 0))
summary_sim_est_fit_2 <- summary(sim_est_fit_2, null = c(`RD` = 0))

ATE_fit_1 <- glue::glue(
  "ATE = {round(summary_sim_est_fit_1[3, 1], 2)}, ",
  "CI = [{round(summary_sim_est_fit_1[3, 2], 2)}, ",
  "{round(summary_sim_est_fit_1[3, 3], 2)}]"
)

ATE_fit_2 <- glue::glue(
  "ATE = {round(summary_sim_est_fit_2[3, 1], 2)}, ",
  "CI = [{round(summary_sim_est_fit_2[3, 2], 2)}, ",
  "{round(summary_sim_est_fit_2[3, 3], 2)}]"
)

cat("\n=== ATE Estimates ===\n")
cat("Model 1 (L as confounder):", ATE_fit_1, "\n")
cat("Model 2 (L as mediator):", ATE_fit_2, "\n")

# ==============================================================================
# appendix d: confounding control strategies simulation
# ==============================================================================

set.seed(123)
n <- 10000

# baseline covariates
U <- rnorm(n)
A_0 <- rbinom(n, 1, prob = plogis(U))
Y_0 <- rnorm(n, mean = U, sd = 1)
L_0 <- rnorm(n, mean = U, sd = 1)

# coefficients for treatment assignment
beta_A0 <- 0.25
beta_Y0 <- 0.3
beta_L0 <- 0.2
beta_U <- 0.1

# simulate treatment assignment
A_1 <- rbinom(n, 1, prob = plogis(
  -0.5 +
    beta_A0 * A_0 +
    beta_Y0 * Y_0 +
    beta_L0 * L_0 +
    beta_U * U
))

# coefficients for continuous outcome
delta_A1 <- 0.3
delta_Y0 <- 0.9
delta_A0 <- 0.1
delta_L0 <- 0.3
theta_A0Y0L0 <- 0.5
delta_U <- 0.05

# simulate continuous outcome
Y_2 <- rnorm(n,
  mean = 0 +
    delta_A1 * A_1 +
    delta_Y0 * Y_0 +
    delta_A0 * A_0 +
    delta_L0 * L_0 +
    theta_A0Y0L0 * Y_0 * A_0 * L_0 +
    delta_U * U,
  sd = .5
)

# create data frame
data_confound <- data.frame(Y_2, A_0, A_1, L_0, Y_0, U)

# fit models
fit_no_control <- lm(Y_2 ~ A_1, data = data_confound)
fit_standard <- lm(Y_2 ~ A_1 + L_0, data = data_confound)
fit_interaction <- lm(Y_2 ~ A_1 * (L_0 + A_0 + Y_0), data = data_confound)

# create gtsummary tables
tbl_fit_no_control <- tbl_regression(fit_no_control)
tbl_fit_standard <- tbl_regression(fit_standard)
tbl_fit_interaction <- tbl_regression(fit_interaction)

# filter to show only treatment variable
tbl_list_modified <- lapply(
  list(tbl_fit_no_control, tbl_fit_standard, tbl_fit_interaction),
  function(tbl) {
    tbl %>%
      modify_table_body(~ .x %>% dplyr::filter(variable == "A_1"))
  }
)

# merge tables
table_comparison_confound <- tbl_merge(
  tbls = tbl_list_modified,
  tab_spanner = c("No Control", "Standard", "Interaction")
) %>%
  modify_table_styling(
    column = c(p.value_1, p.value_2, p.value_3),
    hide = TRUE
  )

cat("\n=== Confounding Control Strategy Comparison ===\n")
print(as_kable_extra(table_comparison_confound, format = "markdown"))

# calculate ate using clarify
set.seed(123)
sim_coefs_fit_no_control <- sim(fit_no_control)
sim_coefs_fit_std <- sim(fit_standard)
sim_coefs_fit_int <- sim(fit_interaction)

sim_est_fit_no_control <- sim_ame(
  sim_coefs_fit_no_control,
  var = "A_1",
  subset = A_1 == 1,
  contrast = "RD",
  verbose = FALSE
)

sim_est_fit_std <- sim_ame(
  sim_coefs_fit_std,
  var = "A_1",
  subset = A_1 == 1,
  contrast = "RD",
  verbose = FALSE
)

sim_est_fit_int <- sim_ame(
  sim_coefs_fit_int,
  var = "A_1",
  subset = A_1 == 1,
  contrast = "RD",
  verbose = FALSE
)

summary_sim_coefs_fit_no_control <- summary(sim_est_fit_no_control, null = c(`RD` = 0))
summary_sim_est_fit_std <- summary(sim_est_fit_std, null = c(`RD` = 0))
summary_sim_est_fit_int <- summary(sim_est_fit_int, null = c(`RD` = 0))

ATE_fit_no_control <- glue::glue(
  "ATE = {round(summary_sim_coefs_fit_no_control[3, 1], 2)}, ",
  "CI = [{round(summary_sim_coefs_fit_no_control[3, 2], 2)}, ",
  "{round(summary_sim_coefs_fit_no_control[3, 3], 2)}]"
)

ATE_fit_std <- glue::glue(
  "ATE = {round(summary_sim_est_fit_std[3, 1], 2)}, ",
  "CI = [{round(summary_sim_est_fit_std[3, 2], 2)}, ",
  "{round(summary_sim_est_fit_std[3, 3], 2)}]"
)

ATE_fit_int <- glue::glue(
  "ATE = {round(summary_sim_est_fit_int[3, 1], 2)}, ",
  "CI = [{round(summary_sim_est_fit_int[3, 2], 2)}, ",
  "{round(summary_sim_est_fit_int[3, 3], 2)}]"
)

cat("\n=== ATE Estimates for Confounding Control Strategies ===\n")
cat("No Control:", ATE_fit_no_control, "\n")
cat("Standard Control:", ATE_fit_std, "\n")
cat("Interaction Model:", ATE_fit_int, "\n")

# ==============================================================================
# appendix e: causal forest estimation
# ==============================================================================

# prepare data for causal forest
W <- as.matrix(data_confound$A_1)
Y <- as.matrix(data_confound$Y_2)
X <- as.matrix(data_confound[, c("L_0", "A_0", "Y_0")])

# fit causal forest model
fit_causal_forest <- causal_forest(X, Y, W)

# estimate average treatment effect
ate_cf <- average_treatment_effect(fit_causal_forest)
ate_cf_df <- data.frame(ate_cf)

ATE_fit_causal_forest <- glue::glue(
  "ATE = {round(ate_cf_df[1, 1], 2)}, se = {round(ate_cf_df[2, 1], 2)}"
)

cat("\n=== Causal Forest ATE Estimate ===\n")
cat(ATE_fit_causal_forest, "\n")

# ==============================================================================
# end of script
# ==============================================================================

cat("\n=== All simulations completed successfully ===\n")
