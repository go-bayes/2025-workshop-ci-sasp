# lecture 1 analysis: baseline adjustment and ate/att/atc comparison
# demonstrates selection bias and different treatment effect estimands
# joseph bulbulia, workshop 2025

# load required packages
library(grf)       # for causal forests
library(tidyverse) # for data manipulation
library(here)      # for path management
library(knitr)     # for nice tables

# set seed for reproducibility
set.seed(2025)

# load simulated data
df <- readRDS(here::here("data", "religion_baseline_sim.rds"))

# prepare data for analysis
# define outcomes
outcome_vars <- c("t2_charity_donate_z", "t2_log_hours_charity_z",
                 "t2_belong_z", "t2_support_z")

# define treatment
treatment <- "t1_religious_service"

# ---- part 1: naive analysis (no baseline adjustment) ----
cat("========================================\n")
cat("part 1: naive analysis (no adjustment)\n")
cat("========================================\n\n")

# simple t-tests without adjustment
naive_results <- data.frame(
  outcome = character(),
  naive_diff = numeric(),
  se = numeric(),
  pvalue = numeric(),
  stringsAsFactors = FALSE
)

for (outcome in outcome_vars) {
  # extract outcome values by treatment group
  y1 <- df[[outcome]][df[[treatment]] == 1]
  y0 <- df[[outcome]][df[[treatment]] == 0]

  # t-test
  ttest <- t.test(y1, y0)

  # store results
  naive_results <- rbind(naive_results, data.frame(
    outcome = outcome,
    naive_diff = ttest$estimate[1] - ttest$estimate[2],
    se = ttest$stderr,
    pvalue = ttest$p.value,
    stringsAsFactors = FALSE
  ))
}

# display naive results
cat("naive differences (standardised scale):\n")
print(kable(naive_results, digits = 3))

# ---- part 2: causal forest with baseline adjustment ----
cat("\n========================================\n")
cat("part 2: causal forest with adjustment\n")
cat("========================================\n\n")

# define covariates
# demographics only (simple model)
covar_demos <- c("age", "education", "household_income", "male")

# demographics + baseline outcomes + baseline attendance (proper adjustment)
covar_baseline <- c("age", "education", "household_income", "male",
                   "t0_religious_attend",  # KEY: baseline religious attendance
                   "t0_charity_donate_z", "t0_log_hours_charity_z",
                   "t0_belong_z", "t0_support_z")

# prepare matrices
X_demos <- as.matrix(df[, covar_demos])
X_baseline <- as.matrix(df[, covar_baseline])
W <- df[[treatment]]

# extract census weights
weights <- df$census_weight

# store results
cf_results <- list()

# fit causal forests for each outcome
for (outcome in outcome_vars) {
  cat("\nanalysing:", outcome, "\n")

  Y <- df[[outcome]]

  # model 1: demographics only
  cf_demos <- causal_forest(X_demos, Y, W,
                           sample.weights = weights,
                           num.trees = 2000,
                           honesty = TRUE,
                           seed = 2025)

  # model 2: with baseline adjustment
  cf_baseline <- causal_forest(X_baseline, Y, W,
                              sample.weights = weights,
                              num.trees = 2000,
                              honesty = TRUE,
                              seed = 2025)

  # extract ate, att, atc, ato
  ate_demos <- average_treatment_effect(cf_demos)
  att_demos <- average_treatment_effect(cf_demos, target.sample = "treated")
  atc_demos <- average_treatment_effect(cf_demos, target.sample = "control")
  ato_demos <- average_treatment_effect(cf_demos, target.sample = "overlap")

  ate_baseline <- average_treatment_effect(cf_baseline)
  att_baseline <- average_treatment_effect(cf_baseline, target.sample = "treated")
  atc_baseline <- average_treatment_effect(cf_baseline, target.sample = "control")
  ato_baseline <- average_treatment_effect(cf_baseline, target.sample = "overlap")

  # store results
  cf_results[[outcome]] <- list(
    demos = list(ate = ate_demos, att = att_demos, atc = atc_demos, ato = ato_demos),
    baseline = list(ate = ate_baseline, att = att_baseline, atc = atc_baseline, ato = ato_baseline),
    cf_baseline = cf_baseline  # keep for later use
  )

  # print comparison
  cat("\ndemographics only:\n")
  cat(sprintf("  ate: %.3f (%.3f)\n", ate_demos[1], ate_demos[2]))
  cat(sprintf("  att: %.3f (%.3f)\n", att_demos[1], att_demos[2]))
  cat(sprintf("  atc: %.3f (%.3f)\n", atc_demos[1], atc_demos[2]))

  cat("\nwith baseline adjustment:\n")
  cat(sprintf("  ate: %.3f (%.3f)\n", ate_baseline[1], ate_baseline[2]))
  cat(sprintf("  att: %.3f (%.3f)\n", att_baseline[1], att_baseline[2]))
  cat(sprintf("  atc: %.3f (%.3f)\n", atc_baseline[1], atc_baseline[2]))
}

# ---- part 3: create comparison table ----
cat("\n========================================\n")
cat("part 3: summary comparison\n")
cat("========================================\n\n")

# create summary table
summary_table <- data.frame(
  outcome = character(),
  estimand = character(),
  naive = numeric(),
  demos_only = numeric(),
  with_baseline = numeric(),
  stringsAsFactors = FALSE
)

for (i in 1:length(outcome_vars)) {
  outcome <- outcome_vars[i]

  # ate row
  summary_table <- rbind(summary_table, data.frame(
    outcome = outcome,
    estimand = "ATE",
    naive = naive_results$naive_diff[i],
    demos_only = cf_results[[outcome]]$demos$ate[1],
    with_baseline = cf_results[[outcome]]$baseline$ate[1],
    stringsAsFactors = FALSE
  ))

  # att row
  summary_table <- rbind(summary_table, data.frame(
    outcome = outcome,
    estimand = "ATT",
    naive = NA,  # not applicable
    demos_only = cf_results[[outcome]]$demos$att[1],
    with_baseline = cf_results[[outcome]]$baseline$att[1],
    stringsAsFactors = FALSE
  ))

  # atc row
  summary_table <- rbind(summary_table, data.frame(
    outcome = outcome,
    estimand = "ATC",
    naive = NA,  # not applicable
    demos_only = cf_results[[outcome]]$demos$atc[1],
    with_baseline = cf_results[[outcome]]$baseline$atc[1],
    stringsAsFactors = FALSE
  ))
}

cat("effect estimates comparison (standardised scale):\n")
print(kable(summary_table, digits = 3))

# ---- part 4: back-transform to meaningful units ----
cat("\n========================================\n")
cat("part 4: effects in meaningful units\n")
cat("========================================\n\n")

# helper function to back-transform
back_transform <- function(effect_z, outcome_name, df) {
  if (outcome_name == "t2_charity_donate_z") {
    # get sd of donations
    sd_donate <- sd(df$t2_charity_donate)
    return(effect_z * sd_donate)
  } else if (outcome_name == "t2_log_hours_charity_z") {
    # this is trickier - we need to think about meaningful change
    # for now, report standardised effect
    return(effect_z)
  } else if (outcome_name %in% c("t2_belong_z", "t2_support_z")) {
    # get sd of 1-7 scale
    var_name <- gsub("_z$", "", outcome_name)
    sd_scale <- sd(df[[var_name]])
    return(effect_z * sd_scale)
  }
  return(effect_z)
}

# show key results with baseline adjustment
cat("causal effects with proper baseline adjustment:\n\n")

# donations
donate_ate <- cf_results[["t2_charity_donate_z"]]$baseline$ate
donate_effect_dollars <- back_transform(donate_ate[1], "t2_charity_donate_z", df)
cat(sprintf("charitable donations:\n"))
cat(sprintf("  ate: $%.0f nzd (se: $%.0f)\n",
            donate_effect_dollars,
            back_transform(donate_ate[2], "t2_charity_donate_z", df)))
cat(sprintf("  att: $%.0f (religious attendees)\n",
            back_transform(cf_results[["t2_charity_donate_z"]]$baseline$att[1],
                          "t2_charity_donate_z", df)))
cat(sprintf("  atc: $%.0f (non-attendees if they attended)\n",
            back_transform(cf_results[["t2_charity_donate_z"]]$baseline$atc[1],
                          "t2_charity_donate_z", df)))

# volunteering (keep as standardised for now)
cat(sprintf("\nvolunteering (standardised log hours):\n"))
cat(sprintf("  ate: %.3f sd\n", cf_results[["t2_log_hours_charity_z"]]$baseline$ate[1]))
cat(sprintf("  att: %.3f sd\n", cf_results[["t2_log_hours_charity_z"]]$baseline$att[1]))
cat(sprintf("  atc: %.3f sd\n", cf_results[["t2_log_hours_charity_z"]]$baseline$atc[1]))

# belonging
belong_ate <- cf_results[["t2_belong_z"]]$baseline$ate
belong_effect <- back_transform(belong_ate[1], "t2_belong_z", df)
cat(sprintf("\nsense of belonging (1-7 scale):\n"))
cat(sprintf("  ate: %.2f units (se: %.2f)\n", belong_effect,
            back_transform(belong_ate[2], "t2_belong_z", df)))
cat(sprintf("  att: %.2f (religious attendees)\n",
            back_transform(cf_results[["t2_belong_z"]]$baseline$att[1], "t2_belong_z", df)))
cat(sprintf("  atc: %.2f (non-attendees if they attended)\n",
            back_transform(cf_results[["t2_belong_z"]]$baseline$atc[1], "t2_belong_z", df)))

# support
support_ate <- cf_results[["t2_support_z"]]$baseline$ate
support_effect <- back_transform(support_ate[1], "t2_support_z", df)
support_pval <- 2 * pnorm(-abs(support_ate[1] / support_ate[2]))
cat(sprintf("\nsocial support (1-7 scale):\n"))
cat(sprintf("  ate: %.2f units (se: %.2f, p = %.3f)\n",
            support_effect,
            back_transform(support_ate[2], "t2_support_z", df),
            support_pval))

# ---- part 5: visualise selection ----
cat("\n========================================\n")
cat("part 5: understanding selection\n")
cat("========================================\n\n")

# get propensity scores from one of the forests
cf_example <- cf_results[["t2_belong_z"]]$cf_baseline
propensity_scores <- cf_example$W.hat

# create plot
library(ggplot2)
p_selection <- ggplot(data.frame(
  propensity = propensity_scores,
  treatment = factor(W, labels = c("Non-attendees", "Attendees"))
), aes(x = propensity, fill = treatment)) +
  geom_histogram(alpha = 0.7, position = "identity", bins = 30) +
  labs(
    title = "Selection into Religious Attendance",
    subtitle = "Propensity scores show who is likely to attend",
    x = "Probability of attending religious services",
    y = "Count",
    fill = "Actual attendance"
  ) +
  theme_minimal() +
  theme(legend.position = "top")

print(p_selection)

# save plot
ggsave(here("outputs", "selection_propensity.png"), p_selection,
       width = 8, height = 6, dpi = 300)

cat("propensity score plot saved to outputs/selection_propensity.png\n")

# ---- key takeaways ----
cat("\n========================================\n")
cat("key takeaways\n")
cat("========================================\n\n")

cat("1. naive analysis dramatically overestimates effects\n")
cat("   - confuses selection with causation\n")
cat("   - religious people already more prosocial\n\n")

cat("2. baseline adjustment reveals true effects\n")
cat("   - donations: ~$500 increase (not $1500+)\n")
cat("   - meaningful but modest effects\n\n")

cat("3. att ≠ atc reveals selection patterns\n")
cat("   - donations: att > atc (attendees more responsive)\n")
cat("   - belonging: atc > att (non-attendees more isolated)\n\n")

cat("4. some effects not statistically significant\n")
cat("   - social support: small effect, high variance\n")
cat("   - important for honest reporting\n\n")

# save results for later use
saveRDS(cf_results, here("outputs", "lecture1_cf_results.rds"))
cat("\nresults saved for future reference\n")

