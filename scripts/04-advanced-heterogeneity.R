# lecture 2: advanced heterogeneity analysis
# explores who benefits most from religious participation
# joseph bulbulia, workshop 2025

# load required packages
library(grf)         # for causal forests
library(tidyverse)   # for data manipulation
library(here)        # for path management
library(policytree)  # for policy learning
library(ggplot2)     # for visualisation
library(patchwork)   # for combining plots

# set seed for reproducibility
set.seed(2025)

# load rich simulation data
df <- readRDS(here("data", "religion_hetero_sim.rds"))

# prepare data
outcome_vars <- c("t2_charity_donate_z", "t2_log_hours_charity_z",
                 "t2_belong_z", "t2_support_z")
treatment <- "t1_religious_service"

# all covariates for heterogeneity discovery
covariates <- c("age", "education", "hours_housework", "hours_commuting",
               "household_income", "male", "urban_rural", "political_conservatism",
               "social_dominance_orientation", "right_wing_authoritarianism",
               "t0_charity_donate_z", "t0_log_hours_charity_z",
               "t0_belong_z", "t0_support_z")

X <- as.matrix(df[, covariates])
W <- df[[treatment]]
weights <- df$census_weight

# store results
hetero_results <- list()

# ---- part 1: fit causal forests and explore heterogeneity ----
cat("========================================\n")
cat("part 1: heterogeneity discovery\n")
cat("========================================\n\n")

for (outcome in outcome_vars) {
  cat("\nanalysing heterogeneity for:", outcome, "\n")
  cat("----------------------------------------\n")

  Y <- df[[outcome]]

  # fit causal forest
  cf <- causal_forest(X, Y, W,
                     sample.weights = weights,
                     num.trees = 4000,  # more trees for heterogeneity
                     honesty = TRUE,
                     tune.parameters = "all",
                     seed = 2025)

  # get treatment effects
  tau_hat <- predict(cf)$predictions

  # ate, att, atc, ato
  ate <- average_treatment_effect(cf)
  att <- average_treatment_effect(cf, target.sample = "treated")
  atc <- average_treatment_effect(cf, target.sample = "control")
  ato <- average_treatment_effect(cf, target.sample = "overlap")

  cat(sprintf("ate: %.3f (%.3f)\n", ate[1], ate[2]))
  cat(sprintf("att: %.3f (%.3f)\n", att[1], att[2]))
  cat(sprintf("atc: %.3f (%.3f)\n", atc[1], atc[2]))
  cat(sprintf("ato: %.3f (%.3f)\n", ato[1], ato[2]))

  # variable importance for heterogeneity
  varimp <- variable_importance(cf)
  varimp_df <- data.frame(
    variable = covariates,
    importance = as.numeric(varimp)
  ) %>%
    arrange(desc(importance)) %>%
    slice_head(n = 10)

  cat("\ntop 10 variables for heterogeneity:\n")
  print(varimp_df)

  # store results
  hetero_results[[outcome]] <- list(
    cf = cf,
    tau_hat = tau_hat,
    ate = ate,
    att = att,
    atc = atc,
    ato = ato,
    varimp = varimp_df
  )
}

# ---- part 2: best linear projection on key moderators ----
cat("\n========================================\n")
cat("part 2: exploring key moderators\n")
cat("========================================\n\n")

# focus on belonging (most interesting heterogeneity)
cf_belong <- hetero_results[["t2_belong_z"]]$cf

# select key moderators based on theory and variable importance
key_mods <- c("age", "urban_rural", "political_conservatism",
             "hours_housework", "hours_commuting")
X_mods <- df[, key_mods]

# best linear projection
blp <- best_linear_projection(cf_belong, X_mods)

cat("best linear projection for belonging:\n")
print(blp)

# ---- part 3: subgroup analysis ----
cat("\n========================================\n")
cat("part 3: subgroup treatment effects\n")
cat("========================================\n\n")

# define meaningful subgroups
df <- df %>%
  mutate(
    age_group = cut(age, breaks = c(0, 35, 50, 65, 100),
                   labels = c("Young", "Middle", "Older", "Elderly")),
    time_constrained = (hours_housework + hours_commuting) > 25,
    urban = urban_rural <= 2,
    conservative = political_conservatism > 5,
    high_income = household_income > 80000
  )

# calculate subgroup effects for belonging
subgroups <- c("age_group", "time_constrained", "urban", "conservative", "high_income")
subgroup_results <- list()

for (subgroup in subgroups) {
  cat("\nsubgroup analysis by:", subgroup, "\n")

  levels <- unique(df[[subgroup]])

  for (level in levels) {
    idx <- which(df[[subgroup]] == level)
    if (length(idx) > 100) {  # only if sufficient sample
      sub_ate <- average_treatment_effect(cf_belong, subset = idx)

      cat(sprintf("  %s (n=%d): %.3f (%.3f)\n",
                 as.character(level), length(idx),
                 sub_ate[1], sub_ate[2]))

      subgroup_results[[paste(subgroup, level, sep = "_")]] <- sub_ate
    }
  }
}

# ---- part 4: policy learning ----
cat("\n========================================\n")
cat("part 4: optimal targeting policies\n")
cat("========================================\n\n")

# get doubly robust scores
dr_scores <- double_robust_scores(cf_belong)

# policy tree for ate
cat("policy tree for maximising average effect:\n")
pt_ate <- policy_tree(X, dr_scores, depth = 2)
print(pt_ate)
pt_ate

# plot policy tree
plot(pt_ate, leaf.labels = c("No_Encourage", "Encourage_Attend"))

# policy tree for atc (targeting non-attendees)
cat("\npolicy tree for non-attendees (atc):\n")
control_idx <- which(W == 0)
pt_atc <- policy_tree(X[control_idx, ], dr_scores[control_idx, ], depth = 2)
print(pt_atc)

# ---- part 5: visualisations ----
cat("\n========================================\n")
cat("part 5: creating visualisations\n")
cat("========================================\n\n")

# 1. variable importance plot
p_varimp <- ggplot(hetero_results[["t2_belong_z"]]$varimp,
                  aes(x = reorder(variable, importance), y = importance)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Variable Importance for Heterogeneity",
       subtitle = "Sense of belonging outcome",
       x = "", y = "Importance") +
  theme_minimal()

# 2. heterogeneous effects by age and urban/rural
tau_belong <- hetero_results[["t2_belong_z"]]$tau_hat
df$tau_belong_pred <- tau_belong

p_hetero <- ggplot(df, aes(x = age, y = tau_belong_pred,
                           color = factor(urban_rural))) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess", se = TRUE) +
  scale_color_viridis_d(name = "Urban-Rural\n(1=Urban, 5=Rural)") +
  labs(title = "Heterogeneous Treatment Effects",
       subtitle = "Belonging: Effect varies by age and location",
       x = "Age", y = "Predicted treatment effect") +
  theme_minimal()

# 3. ate/att/atc comparison
ate_comparison <- data.frame(
  outcome = rep(c("Donations", "Volunteering", "Belonging", "Support"), each = 4),
  estimand = rep(c("ATE", "ATT", "ATC", "ATO"), 4),
  effect = c(
    hetero_results[["t2_charity_donate_z"]]$ate[1],
    hetero_results[["t2_charity_donate_z"]]$att[1],
    hetero_results[["t2_charity_donate_z"]]$atc[1],
    hetero_results[["t2_charity_donate_z"]]$ato[1],
    hetero_results[["t2_log_hours_charity_z"]]$ate[1],
    hetero_results[["t2_log_hours_charity_z"]]$att[1],
    hetero_results[["t2_log_hours_charity_z"]]$atc[1],
    hetero_results[["t2_log_hours_charity_z"]]$ato[1],
    hetero_results[["t2_belong_z"]]$ate[1],
    hetero_results[["t2_belong_z"]]$att[1],
    hetero_results[["t2_belong_z"]]$atc[1],
    hetero_results[["t2_belong_z"]]$ato[1],
    hetero_results[["t2_support_z"]]$ate[1],
    hetero_results[["t2_support_z"]]$att[1],
    hetero_results[["t2_support_z"]]$atc[1],
    hetero_results[["t2_support_z"]]$ato[1]
  )
)

p_comparison <- ggplot(ate_comparison,
                      aes(x = outcome, y = effect, fill = estimand)) +
  geom_col(position = "dodge") +
  labs(title = "ATE vs ATT vs ATC vs ATO",
       subtitle = "Different effects for different populations",
       x = "", y = "Standardised effect",
       fill = "Estimand") +
  theme_minimal() +
  theme(legend.position = "top")

# 4. qini curves
# calculate rank-based metrics
qini_belong <- rank_average_treatment_effect(cf_belong, tau_belong)
qini_donate <- rank_average_treatment_effect(
  hetero_results[["t2_charity_donate_z"]]$cf,
  hetero_results[["t2_charity_donate_z"]]$tau_hat
)

# save plots
ggsave(here("outputs", "variable_importance.png"), p_varimp,
       width = 8, height = 6, dpi = 300)
ggsave(here("outputs", "heterogeneous_effects.png"), p_hetero,
       width = 10, height = 6, dpi = 300)
ggsave(here("outputs", "ate_comparison.png"), p_comparison,
       width = 10, height = 6, dpi = 300)

cat("plots saved to outputs/\n")



# ---- part 6: key insights in meaningful units ----
cat("\n========================================\n")
cat("part 6: key insights\n")
cat("========================================\n\n")

# back-transform key findings
# since all incomes are similar, use median split
income_median <- median(df$household_income)
# donations for conservatives vs liberals
conservatives <- which(df$conservative)
liberals <- which(!df$conservative)

donate_cons <- average_treatment_effect(
  hetero_results[["t2_charity_donate_z"]]$cf,
  subset = conservatives
)
donate_lib <- average_treatment_effect(
  hetero_results[["t2_charity_donate_z"]]$cf,
  subset = liberals
)

# convert to dollars
sd_donate <- sd(df$t2_charity_donate)
cat("donation effects by subgroup:\n")
cat(sprintf("  conservatives: $%.0f\n", donate_cons[1] * sd_donate))
cat(sprintf("  liberals: $%.0f\n", donate_lib[1] * sd_donate))

# belonging for rural elderly vs urban young
rural_elderly <- which(df$urban_rural >= 4 & df$age > 65)
urban_young <- which(df$urban_rural <= 2 & df$age < 35)

belong_rural_old <- average_treatment_effect(cf_belong, subset = rural_elderly)
belong_urban_young <- average_treatment_effect(cf_belong, subset = urban_young)

sd_belong <- sd(df$t2_belong)
cat("\nbelonging effects by subgroup:\n")
cat(sprintf("  rural elderly: %.2f units\n", belong_rural_old[1] * sd_belong))
cat(sprintf("  urban young: %.2f units\n", belong_urban_young[1] * sd_belong))

# policy recommendations
cat("\n========================================\n")
cat("policy recommendations\n")
cat("========================================\n\n")

cat("1. target young urban non-attendees for belonging benefits\n")
cat("   - they gain most in social connection\n")
cat("   - atc > ate for belonging outcome\n\n")

cat("2. don't target time-constrained individuals\n")
cat("   - practical barriers limit effectiveness\n")
cat("   - focus on those with capacity to participate\n\n")

cat("3. donation appeals work best for conservatives\n")
cat("   - they have stronger religious giving norms\n")
cat("   - tailor messaging accordingly\n\n")

cat("4. rural elderly already well-connected\n")
cat("   - smaller marginal gains from attendance\n")
cat("   - focus resources elsewhere\n\n")

# save results
saveRDS(hetero_results, here("outputs", "lecture2_hetero_results.rds"))
cat("\nanalysis complete! results saved.\n")

