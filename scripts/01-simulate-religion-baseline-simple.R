# simple simulation for lecture 1: baseline adjustment demonstration
# demonstrates selection bias and importance of baseline controls
# joseph bulbulia, workshop 2025

# load required packages
library(MASS)      # for mvrnorm
library(tidyverse) # for data manipulation
library(here)      # for path management

# set seed for reproducibility
set.seed(2025)

# simulation parameters
n <- 5000  # number of individuals

# create correlated baseline and outcome measures
# this creates realistic persistence in outcomes over time

# correlation matrix for variables
# structure: age, education, income,
#           baseline_donate, baseline_volunteer, baseline_belong, baseline_support
#           religious_attendance_propensity

cor_matrix <- matrix(c(
  1.00, 0.10, 0.30, 0.20, 0.15, 0.10, 0.05, 0.25,  # age
  0.10, 1.00, 0.40, 0.25, 0.20, 0.15, 0.10, -0.10, # education
  0.30, 0.40, 1.00, 0.35, 0.20, 0.15, 0.10, 0.10,  # income
  0.20, 0.25, 0.35, 1.00, 0.30, 0.25, 0.20, 0.30,  # baseline_donate
  0.15, 0.20, 0.20, 0.30, 1.00, 0.25, 0.20, 0.25,  # baseline_volunteer
  0.10, 0.15, 0.15, 0.25, 0.25, 1.00, 0.40, 0.20,  # baseline_belong
  0.05, 0.10, 0.10, 0.20, 0.20, 0.40, 1.00, 0.15,  # baseline_support
  0.25, -0.10, 0.10, 0.30, 0.25, 0.20, 0.15, 1.00  # religious_propensity
), nrow = 8, byrow = TRUE)

# generate multivariate normal data
mu <- c(50, 14, 11, 6.5, 2.0, 4.5, 4.0, 0)  # means: age 50, education 14 years, etc.
sigma <- c(15, 3, 1, 1.5, 2.5, 1.2, 1.2, 1)  # standard deviations

# create covariance matrix
cov_matrix <- diag(sigma) %*% cor_matrix %*% diag(sigma)

# generate correlated variables
sim_data <- mvrnorm(n = n, mu = mu, Sigma = cov_matrix)
colnames(sim_data) <- c("age", "education", "income_log",
                        "baseline_donate_log", "baseline_volunteer_log",
                        "baseline_belong", "baseline_support",
                        "religious_propensity")

# convert to data frame
df <- as.data.frame(sim_data)

# add gender indicator (for weighting)
set.seed(2025)
df$male <- sample(c(1, 0), n, replace = TRUE, prob = c(0.40, 0.60))  # 40% male, 60% female

# transform variables to realistic scales
df <- df %>%
  mutate(
    # age: keep as is (already in years)
    age = pmax(18, pmin(90, age)),

    # education: years of schooling (bound between 8 and 20)
    education = pmax(8, pmin(20, round(education))),

    # income: exp transform to get dollars (log-normal)
    household_income = exp(income_log) * 50000,  # realistic median
    household_income = pmax(20000, pmin(200000, household_income)),  # reasonable range

    # donations: exp transform, bound to realistic range
    t0_charity_donate = exp(baseline_donate_log) * 50,  # reduced multiplier
    t0_charity_donate = pmax(0, pmin(10000, t0_charity_donate)),  # cap at $10k

    # volunteering: exp transform, many zeros
    t0_hours_charity = exp(baseline_volunteer_log) - 1,
    t0_hours_charity = pmax(0, pmin(40, t0_hours_charity)),
    t0_hours_charity = ifelse(runif(n) < 0.3, 0, t0_hours_charity), # 30% don't volunteer

    # belonging and support: 1-7 scales
    t0_belong = pmax(1, pmin(7, baseline_belong)),
    t0_support = pmax(1, pmin(7, baseline_support))
  )

# create census weights
# oversample older people, undersample males
df <- df %>%
  mutate(
    # age-based weight (older people oversampled)
    age_weight = case_when(
      age < 30 ~ 0.7,
      age < 50 ~ 0.9,
      age < 65 ~ 1.2,
      TRUE ~ 1.5
    ),
    # gender weight (males undersampled)
    gender_weight = ifelse(male == 1, 0.8, 1.2),
    # combined weight
    census_weight = age_weight * gender_weight
  )

# create religious attendance with STRONG selection bias
# first standardize variables safely
df <- df %>%
  mutate(
    # safe standardization
    donate_z = (t0_charity_donate - mean(t0_charity_donate)) / sd(t0_charity_donate),
    hours_z = (t0_hours_charity - mean(t0_hours_charity)) / sd(t0_hours_charity),
    belong_z = (t0_belong - mean(t0_belong)) / sd(t0_belong),
    income_z = (household_income - mean(household_income)) / sd(household_income),

    # handle any NAs from zero variance
    donate_z = ifelse(is.na(donate_z), 0, donate_z),
    hours_z = ifelse(is.na(hours_z), 0, hours_z),
    belong_z = ifelse(is.na(belong_z), 0, belong_z),
    income_z = ifelse(is.na(income_z), 0, income_z)
  )

# now create attendance
df <- df %>%
  mutate(
    # create baseline religious attendance (previous period)
    t0_religious_attend = rbinom(n, 1,
      plogis(-1 +
        0.8 * religious_propensity +  # strong religious tendency
        0.4 * donate_z +  # charitable people attend
        0.3 * hours_z +  # volunteers attend
        0.2 * (age > 50)  # older more likely
      )
    ),

    # current attendance STRONGLY predicted by past attendance and charity
    propensity = plogis(
      -2 +  # low base rate
      2.5 * t0_religious_attend +  # VERY strong persistence
      0.8 * donate_z +  # charitable much more likely
      0.4 * hours_z +  # volunteers more likely
      0.3 * belong_z +  # belonging seekers
      0.2 * male +  # males slightly more likely
      0.1 * income_z  # rich slightly more likely
    ),

    # binary treatment
    t1_religious_service = rbinom(n, 1, propensity),

    # for realism, monthly attendance
    monthly_attendance = ifelse(
      t1_religious_service == 1,
      sample(2:5, n, replace = TRUE, prob = c(0.4, 0.3, 0.2, 0.1)),
      sample(0:1, n, replace = TRUE, prob = c(0.7, 0.3))
    )
  )

# check for NAs in propensity first
if (any(is.na(df$propensity))) {
  cat("\nDEBUG: NA values in propensity score\n")
  cat("Number of NAs:", sum(is.na(df$propensity)), "\n")
  # set NA propensities to a default
  df$propensity[is.na(df$propensity)] <- 0.2
}

# check attendance distribution
cat("\nattendance distribution:\n")
if (!any(is.na(df$t1_religious_service))) {
  cat("- baseline attendance rate:", round(mean(df$t0_religious_attend), 3), "\n")
  cat("- current attendance rate:", round(mean(df$t1_religious_service), 3), "\n")

  # ensure we have both groups
  if (sum(df$t1_religious_service == 1) == n || sum(df$t1_religious_service == 0) == n) {
    stop("Error: All individuals in same treatment group. Adjust attendance parameters.")
  }
} else {
  cat("\nERROR: NA values in t1_religious_service\n")
  cat("Number of NAs:", sum(is.na(df$t1_religious_service)), "\n")
  stop("Cannot proceed with NA values in treatment")
}

# generate outcomes with treatment effects
# STRONG effect modification by gender and income
df <- df %>%
  mutate(
    # donations: base effect + strong modification by income and gender
    tau_donate = 300 +  # base effect
      400 * (household_income > 80000) +  # rich donate MUCH more
      200 * male +  # males donate more
      300 * (household_income > 80000) * male,  # rich males donate most

    # volunteering: effect modification by gender
    tau_volunteer = 0.3 +  # base effect
      0.4 * male +  # males volunteer more when attending
      0.2 * (age > 65),  # retirees also benefit

    # belonging: effect modification by baseline and gender
    tau_belong = 0.4 +  # base effect
      0.3 * male +  # males benefit more socially
      0.3 * (t0_belong < 4),  # lonely benefit more

    # support: weak effect with gender modification
    tau_support = 0.15 + 0.1 * male,  # males get slightly more support

    # generate outcomes with STRONG baseline persistence
    # charitable donations at t2
    t2_charity_donate = t0_charity_donate * 1.2 +  # strong persistence
      tau_donate * t1_religious_service +
      100 * t0_religious_attend +  # past attendance boosts donations
      rnorm(n, 0, 400),  # noise
    t2_charity_donate = pmax(0, t2_charity_donate),

    # volunteering hours at t2
    t2_hours_charity = t0_hours_charity * 1.1 +  # persistence
      tau_volunteer * t1_religious_service +
      0.2 * t0_religious_attend +  # past attendance matters
      rnorm(n, 0, 0.4),
    t2_hours_charity = pmax(0, t2_hours_charity),

    # belonging at t2
    t2_belong = 0.8 * t0_belong + 0.2 * 4 +  # strong persistence
      tau_belong * t1_religious_service +
      0.3 * t0_religious_attend +  # past attendance helps
      rnorm(n, 0, 0.7),
    t2_belong = pmax(1, pmin(7, t2_belong)),

    # support at t2
    t2_support = 0.8 * t0_support + 0.2 * 4 +  # persistence
      tau_support * t1_religious_service +
      0.2 * t0_religious_attend +  # past attendance
      rnorm(n, 0, 0.8),
    t2_support = pmax(1, pmin(7, t2_support))
  )

# create standardised versions for analysis
df <- df %>%
  mutate(
    # standardise continuous outcomes (handle potential NAs)
    t2_charity_donate_z = as.numeric(scale(t2_charity_donate)),
    t2_log_hours_charity_z = as.numeric(scale(log(t2_hours_charity + 1))),
    t2_belong_z = as.numeric(scale(t2_belong)),
    t2_support_z = as.numeric(scale(t2_support)),

    # also standardise baseline versions
    t0_charity_donate_z = as.numeric(scale(t0_charity_donate)),
    t0_log_hours_charity_z = as.numeric(scale(log(t0_hours_charity + 1))),
    t0_belong_z = as.numeric(scale(t0_belong)),
    t0_support_z = as.numeric(scale(t0_support))
  )

# check for any NAs in final dataset
na_summary <- sapply(df, function(x) sum(is.na(x)))
if (any(na_summary > 0)) {
  cat("\nWARNING: NA values found in variables:\n")
  print(na_summary[na_summary > 0])
}

# select final variables for analysis
final_df <- df %>%
  dplyr::select(
    # demographics
    age, education, household_income, male,

    # census weight
    census_weight,

    # religious attendance (including baseline!)
    t0_religious_attend, monthly_attendance, t1_religious_service,

    # baseline outcomes (standardised)
    t0_charity_donate_z, t0_log_hours_charity_z, t0_belong_z, t0_support_z,

    # outcomes (standardised)
    t2_charity_donate_z, t2_log_hours_charity_z, t2_belong_z, t2_support_z,

    # keep raw outcomes for back-transformation
    t0_charity_donate, t0_hours_charity, t0_belong, t0_support,
    t2_charity_donate, t2_hours_charity, t2_belong, t2_support,

    # keep treatment effects for checking
    tau_donate, tau_volunteer, tau_belong, tau_support
  )

# save the data
if (!dir.exists(here("data"))) {
  dir.create(here("data"))
}

saveRDS(final_df, here("data", "religion_baseline_sim.rds"))

# print summary
cat("simulation complete!\n")
cat("sample size:", nrow(final_df), "\n")
cat("proportion male:", mean(final_df$male), "\n")
cat("proportion attending religious services:", mean(final_df$t1_religious_service), "\n")
cat("\ncensus weight summary:\n")
cat("  min:", round(min(final_df$census_weight), 2), "\n")
cat("  mean:", round(mean(final_df$census_weight), 2), "\n")
cat("  max:", round(max(final_df$census_weight), 2), "\n")
cat("\nheterogeneous treatment effects:\n")
cat("- donations: $300-$1200 nzd (varies by income and gender)\n")
cat("- volunteering: 0.3-0.9 hours/week (varies by gender and age)\n")
cat("- belonging: 0.4-1.0 units (varies by gender and baseline)\n")
cat("- support: 0.15-0.25 units (varies by gender)\n")
cat("\nkey: effects much larger for males and high income individuals\n")

# check for any issues with the data
cat("\ndata check:\n")
cat("- columns in final_df:", paste(names(final_df), collapse = ", "), "\n")
if ("t2_charity_donate" %in% names(final_df)) {
  cat("- range of t2_charity_donate:", range(final_df$t2_charity_donate, na.rm = TRUE), "\n")
  cat("- any NA in t2_charity_donate?", any(is.na(final_df$t2_charity_donate)), "\n")
  cat("- any Inf in t2_charity_donate?", any(is.infinite(final_df$t2_charity_donate)), "\n")
}
if ("t2_hours_charity" %in% names(final_df)) {
  cat("- range of t2_hours_charity:", range(final_df$t2_hours_charity, na.rm = TRUE), "\n")
  cat("- any NA in t2_hours_charity?", any(is.na(final_df$t2_hours_charity)), "\n")
}
cat("- religious attendees:", sum(final_df$t1_religious_service == 1), "\n")
cat("- non-attendees:", sum(final_df$t1_religious_service == 0), "\n")

# show selection bias (naive differences)
cat("\nnaive differences (no baseline adjustment):\n")
cat("(these should be much larger than true effects due to selection)\n")

# check if we have the raw outcome columns
if ("t2_charity_donate" %in% names(final_df)) {
  donate_diff <- mean(final_df$t2_charity_donate[final_df$t1_religious_service == 1], na.rm = TRUE) -
    mean(final_df$t2_charity_donate[final_df$t1_religious_service == 0], na.rm = TRUE)
  cat("- donations: $", round(donate_diff), " nzd (true average: ~$650)\n", sep = "")
} else {
  cat("- donations: column not found\n")
}

if ("t2_hours_charity" %in% names(final_df)) {
  hours_diff <- mean(final_df$t2_hours_charity[final_df$t1_religious_service == 1], na.rm = TRUE) -
    mean(final_df$t2_hours_charity[final_df$t1_religious_service == 0], na.rm = TRUE)
  cat("- volunteering:", round(hours_diff, 2), " hours/week (true average: ~0.65)\n")
} else {
  cat("- volunteering: column not found\n")
}

if ("t2_belong" %in% names(final_df)) {
  belong_diff <- mean(final_df$t2_belong[final_df$t1_religious_service == 1], na.rm = TRUE) -
    mean(final_df$t2_belong[final_df$t1_religious_service == 0], na.rm = TRUE)
  cat("- belonging:", round(belong_diff, 2), " units (true average: ~0.65)\n")
}

cat("\ndata saved to:", here("data", "religion_baseline_sim.rds"), "\n")

