# rich simulation for lecture 2: heterogeneous treatment effects
# creates realistic data with many covariates and complex effect patterns
# joseph bulbulia, workshop 2025

# load required packages
library(MASS)      # for mvrnorm
library(tidyverse) # for data manipulation
library(here)      # for path management

# set seed for reproducibility
set.seed(2025)

# simulation parameters
n <- 8000  # larger sample for heterogeneity detection

# add gender for census weights (60% female, 40% male)
set.seed(2025)
male <- sample(c(1, 0), n, replace = TRUE, prob = c(0.40, 0.60))

# define variable structure
# demographics: age, education
# time constraints: hours_housework, hours_commuting
# socioeconomic: household_income
# geographic: urban_rural (1-5)
# ideological: political_conservatism, social_dominance_orientation, right_wing_authoritarianism
# baseline outcomes: donate, volunteer, belong, support

# create correlation matrix (15 variables)
# realistic correlation structure
cor_matrix <- matrix(0.15, nrow = 15, ncol = 15)  # baseline correlation
diag(cor_matrix) <- 1

# specific correlations
# age correlations
cor_matrix[1, 4] <- cor_matrix[4, 1] <- -0.20  # age-commuting (older commute less)
cor_matrix[1, 7] <- cor_matrix[7, 1] <- 0.30   # age-conservatism
cor_matrix[1, 8] <- cor_matrix[8, 1] <- 0.25   # age-sdo
cor_matrix[1, 9] <- cor_matrix[9, 1] <- 0.35   # age-rwa

# education correlations
cor_matrix[2, 5] <- cor_matrix[5, 2] <- 0.40   # education-income
cor_matrix[2, 6] <- cor_matrix[6, 2] <- -0.25  # education-rural (educated more urban)
cor_matrix[2, 7] <- cor_matrix[7, 2] <- -0.20  # education-conservatism
cor_matrix[2, 8] <- cor_matrix[8, 2] <- -0.15  # education-sdo

# time constraint correlations
cor_matrix[3, 4] <- cor_matrix[4, 3] <- 0.30   # housework-commuting

# income correlations
cor_matrix[5, 6] <- cor_matrix[6, 5] <- -0.30  # income-rural (urban higher income)

# ideological correlations (strong cluster)
cor_matrix[7, 8] <- cor_matrix[8, 7] <- 0.60   # conservatism-sdo
cor_matrix[7, 9] <- cor_matrix[9, 7] <- 0.55   # conservatism-rwa
cor_matrix[8, 9] <- cor_matrix[9, 8] <- 0.50   # sdo-rwa

# baseline outcome correlations
cor_matrix[10, 11] <- cor_matrix[11, 10] <- 0.35  # donate-volunteer
cor_matrix[10, 12] <- cor_matrix[12, 10] <- 0.25  # donate-belong
cor_matrix[11, 12] <- cor_matrix[12, 11] <- 0.30  # volunteer-belong
cor_matrix[12, 13] <- cor_matrix[13, 12] <- 0.45  # belong-support

# religious propensity correlations
cor_matrix[14, 1] <- cor_matrix[1, 14] <- 0.30   # age
cor_matrix[14, 6] <- cor_matrix[6, 14] <- 0.25   # rural
cor_matrix[14, 7] <- cor_matrix[7, 14] <- 0.40   # conservatism
cor_matrix[14, 9] <- cor_matrix[9, 14] <- 0.35   # rwa

# generate multivariate normal data
mu <- c(50, 14, 15, 5, 11.5, 3, 4, 4, 4, 6.5, 2, 4.5, 4, 0, 0)
sigma <- c(15, 3, 8, 4, 0.7, 1.2, 1.5, 1.5, 1.5, 1.5, 2.5, 1.2, 1.2, 1, 1)

# create covariance matrix
cov_matrix <- diag(sigma) %*% cor_matrix %*% diag(sigma)

# generate data
sim_data <- mvrnorm(n = n, mu = mu, Sigma = cov_matrix)
colnames(sim_data) <- c("age", "education", "hours_housework", "hours_commuting",
                        "income_log", "urban_rural", "political_conservatism",
                        "social_dominance_orientation", "right_wing_authoritarianism",
                        "baseline_donate_log", "baseline_volunteer_log",
                        "baseline_belong", "baseline_support",
                        "religious_propensity", "heterogeneity_factor")

# convert to data frame and transform
df <- as.data.frame(sim_data) %>%
  mutate(
    # add gender indicator
    male = male,

    # demographics
    age = pmax(18, pmin(90, age)),
    education = pmax(8, pmin(20, round(education))),

    # time constraints
    hours_housework = pmax(0, pmin(60, hours_housework)),
    hours_commuting = pmax(0, pmin(20, hours_commuting)),

    # income (log-normal)
    household_income = exp(income_log) * 50000,  # realistic median
    household_income = pmax(20000, pmin(200000, household_income)),  # reasonable range

    # geographic (1-5 scale, round to integers)
    urban_rural = pmax(1, pmin(5, round(urban_rural))),

    # ideological scales (1-7)
    political_conservatism = pmax(1, pmin(7, political_conservatism)),
    social_dominance_orientation = pmax(1, pmin(7, social_dominance_orientation)),
    right_wing_authoritarianism = pmax(1, pmin(7, right_wing_authoritarianism)),

    # baseline outcomes
    t0_charity_donate = exp(baseline_donate_log) * 100,
    t0_charity_donate = pmax(0, pmin(50000, t0_charity_donate)),

    t0_hours_charity = exp(baseline_volunteer_log) - 1,
    t0_hours_charity = pmax(0, pmin(40, t0_hours_charity)),
    t0_hours_charity = ifelse(runif(n) < 0.3, 0, t0_hours_charity),

    t0_belong = pmax(1, pmin(7, baseline_belong)),
    t0_support = pmax(1, pmin(7, baseline_support))
  )

# create census weights (same pattern as simple simulation)
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

# create religious attendance with simple selection
df <- df %>%
  mutate(
    # time constraints reduce attendance
    time_burden = as.numeric(scale(hours_housework + hours_commuting)),
    time_burden = ifelse(is.na(time_burden), 0, time_burden),

    # simple propensity score approach
    propensity = 0.25 +  # base rate 25%
      0.10 * (age > 50) +  # older more likely
      -0.05 * (time_burden > 1) +  # time constrained less likely
      0.05 * (urban_rural > 3) +  # rural more likely
      0.10 * (political_conservatism > 5) +  # conservatives more likely
      0.05 * (t0_charity_donate > median(t0_charity_donate)) +  # charitable more likely
      0.05 * (t0_belong > 4) +  # those seeking belonging
      0.05 * religious_propensity +  # latent religiosity
      rnorm(n, 0, 0.1),  # small random variation

    # ensure propensity stays in reasonable bounds
    propensity = pmax(0.1, pmin(0.9, propensity)),

    # binary treatment
    t1_religious_service = rbinom(n, 1, propensity),

    # for realism, also create monthly attendance
    monthly_attendance = ifelse(
      t1_religious_service == 1,
      sample(2:5, n, replace = TRUE, prob = c(0.4, 0.3, 0.2, 0.1)),
      sample(0:1, n, replace = TRUE, prob = c(0.7, 0.3))
    )
  )

# generate heterogeneous treatment effects
# following grf examples with smooth functions

# standardize key variables for heterogeneous effects
df <- df %>%
  mutate(
    income_z = as.numeric(scale(household_income)),
    age_z = as.numeric(scale(age)),
    rural_z = as.numeric(scale(urban_rural)),
    conserv_z = as.numeric(scale(political_conservatism)),
    belong_z = as.numeric(scale(t0_belong)),

    # handle any NAs from scaling
    income_z = ifelse(is.na(income_z), 0, income_z),
    age_z = ifelse(is.na(age_z), 0, age_z),
    rural_z = ifelse(is.na(rural_z), 0, rural_z),
    conserv_z = ifelse(is.na(conserv_z), 0, conserv_z),
    belong_z = ifelse(is.na(belong_z), 0, belong_z),

    # donations: smooth effect based on income and conservatism
    tau_donate = 300 +
      200 * pmax(0, income_z) +  # richer donate more (positive only)
      150 * conserv_z * (conserv_z > 0),  # conservatives donate more when above median

    # volunteering: based on time constraints and age
    tau_volunteer = 0.3 +
      0.4 * pmax(0, 1 - time_burden) +  # less time burden = more volunteering
      0.3 * (age > 65),  # retirees volunteer more

    # belonging: rural and age interaction
    tau_belong = 0.5 +
      0.3 * rural_z * (rural_z > 0) +  # rural benefit more
      0.2 * (age > 50) +  # older benefit more
      -0.2 * pmax(0, belong_z),  # those already connected benefit less

    # support: small constant effect with slight moderation
    tau_support = 0.2 +
      0.1 * conserv_z  # slight conservative moderation
  )

# generate outcomes with heterogeneous effects
df <- df %>%
  mutate(
    # donations
    t2_charity_donate = t0_charity_donate * 1.1 +
      tau_donate * t1_religious_service +
      rnorm(n, 0, 500),
    t2_charity_donate = pmax(0, t2_charity_donate),

    # volunteering
    t2_hours_charity = t0_hours_charity * 0.9 +
      tau_volunteer * t1_religious_service +
      rnorm(n, 0, 0.5),
    t2_hours_charity = pmax(0, t2_hours_charity),

    # belonging
    t2_belong = 0.7 * t0_belong + 0.3 * 4 +
      tau_belong * t1_religious_service +
      rnorm(n, 0, 0.8),
    t2_belong = pmax(1, pmin(7, t2_belong)),

    # support
    t2_support = 0.7 * t0_support + 0.3 * 4 +
      tau_support * t1_religious_service +
      rnorm(n, 0, 0.9),
    t2_support = pmax(1, pmin(7, t2_support))
  )

# create standardised versions
df <- df %>%
  mutate(
    # standardise outcomes
    t2_charity_donate_z = as.numeric(scale(t2_charity_donate)),
    t2_log_hours_charity_z = as.numeric(scale(log(t2_hours_charity + 1))),
    t2_belong_z = as.numeric(scale(t2_belong)),
    t2_support_z = as.numeric(scale(t2_support)),

    # standardise baseline
    t0_charity_donate_z = as.numeric(scale(t0_charity_donate)),
    t0_log_hours_charity_z = as.numeric(scale(log(t0_hours_charity + 1))),
    t0_belong_z = as.numeric(scale(t0_belong)),
    t0_support_z = as.numeric(scale(t0_support))
  )

# select final variables
final_df <- df %>%
  select(
    # demographics and constraints
    age, education, hours_housework, hours_commuting, household_income, male,

    # census weight
    census_weight,

    # geographic and ideological
    urban_rural, political_conservatism, social_dominance_orientation,
    right_wing_authoritarianism,

    # baseline outcomes (standardised)
    t0_charity_donate_z, t0_log_hours_charity_z, t0_belong_z, t0_support_z,

    # religious attendance
    monthly_attendance, t1_religious_service,

    # outcomes (standardised)
    t2_charity_donate_z, t2_log_hours_charity_z, t2_belong_z, t2_support_z,

    # keep raw for back-transformation
    t0_charity_donate, t0_hours_charity, t0_belong, t0_support,
    t2_charity_donate, t2_hours_charity, t2_belong, t2_support,

    # keep true effects for validation
    tau_donate, tau_volunteer, tau_belong, tau_support
  )

# save the data
saveRDS(final_df, here("data", "religion_hetero_sim.rds"))

# print summary
cat("rich simulation complete!\n")
cat("sample size:", nrow(final_df), "\n")
cat("proportion male:", mean(final_df$male), "\n")
cat("\nattendance patterns:\n")
cat("- mean monthly attendance:", round(mean(final_df$monthly_attendance), 2), "times\n")
cat("- proportion regular attendees (2+/month):", round(mean(final_df$t1_religious_service), 3), "\n")
cat("\nheterogeneity patterns created:\n")
cat("- donations: income × conservatism interaction\n")
cat("- volunteering: time constraints × rural interaction\n")
cat("- belonging: rural × age × conservatism (3-way)\n")
cat("- support: weak effect with rwa moderation\n")

# show variation in treatment effects
cat("\ntreatment effect heterogeneity (among treated):\n")
treated <- final_df[final_df$t1_religious_service == 1, ]
cat("donations: $", round(min(treated$tau_donate)), " to $",
    round(max(treated$tau_donate)), " (mean: $",
    round(mean(treated$tau_donate)), ")\n", sep = "")
cat("volunteering:", round(min(treated$tau_volunteer), 2), "to",
    round(max(treated$tau_volunteer), 2), "hours\n")
cat("belonging:", round(min(treated$tau_belong), 2), "to",
    round(max(treated$tau_belong), 2), "units\n")

cat("\ndata saved to:", here("data", "religion_hetero_sim.rds"), "\n")

