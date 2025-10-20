#  Data Import and Preparation
# Script 1: Religion → Cooperation Analysis
# This script imports synthetic NZAVS data and prepares variables for causal analysis

# restart fresh session for clean workspace
rstudioapi::restartSession(clean = TRUE)

# set seed for workshop reproducibility
set.seed(2025)

# essential library ---------------------------------------------------------
# install and load 'margot' from GitHub if missing
if (!require(margot, quietly = TRUE)) {
  devtools::install_github("go-bayes/margot")
  library(margot)
}

# min version of margot
min_version <- "1.0.230"
if (utils::packageVersion("margot") < min_version) {
  stop(
    "please install margot >= ", min_version, ":\n",
    "  devtools::install_github('go-bayes/margot')"
  )
}

# call library
library("margot")

# check version
packageVersion(pkg = 'margot')


# load packages -------------------------------------------------------------
# pacman will install missing packages automatically
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(
  tidyverse,       # data wrangling + plotting
  qs,              # fast data i/o
  here,            # project-relative file paths
  data.table,      # fast data manipulation
  fastDummies,     # dummy variable creation
  naniar,          # missing data handling
  skimr,           # summary statistics
  grf,             # machine learning forests
  kableExtra,      # tables
  ggplot2,         # graphs
  doParallel,       # parallel processing
  grf,             # causal forests
  janitor,          # variables names
  stringr,          # variable names
  patchwork,        # graphs
  table1,          # tables,
  cli
)


# set up data directory structure
data_dir    <- here::here('/Users/joseph/v-project Dropbox/Joseph Bulbulia/00-nazvs-data-backup/r/2015/')
push_mods   <- here::here("/Users/joseph/v-project Dropbox/data/25/grf-church-coop")

# load data -----------------------------------------------------------------
df_nz_long <- margot::here_read_qs("nzavs_data", data_dir)

# initial data prep ---------------------------------------------------------
# prepare intial data
# define labels for rural classification
rural_labels <- c(
  "High Urban Accessibility",
  "Medium Urban Accessibility",
  "Low Urban Accessibility",
  "Remote",
  "Very Remote"
)

dat_prep <- df_nz_long |>
  arrange("id", "time_factor") |>
  margot::remove_numeric_attributes() |>
  # mutate(
  #   # cap extreme values
  #   alcohol_intensity = pmin(alcohol_intensity, 15),
  #   # flag heavy drinkers: freq ≥3 → 1, ≤2 → 0, else NA
  #   heavy_drinker = case_when(
  #     alcohol_frequency >= 3 ~ 1,
  #     alcohol_frequency <= 2 ~ 0,
  #     TRUE                  ~ NA_real_
  #   ),
  #   # map freq categories to weekly counts
  #   alcohol_frequency_weekly = recode(
  #     alcohol_frequency,
  #     `0` = 0, `1` = 0.25,
  #     `2` = 1, `3` = 2.5,
  #     `4` = 4.5,
  #     .default = NA_real_
  #   ),
  #   # relabel rural factor
  #   rural_gch_2018_l = factor(
  #     rural_gch_2018_l,
  #     levels = 1:5,
  #     labels = rural_labels,
  #     ordered = TRUE
  #   )
  # ) |>
  droplevels()



# view variable names -----------------------------------------------------
print(colnames(df_nz_long))

# get total participants
n_total = length(unique(df_nz_long$id))

# pretty number
n_total = margot::pretty_number(n_total)

# view
n_total

# save
here_save(n_total, "n_total")

# define study variables ----------------------------------------------------
# ** key decision 1: define your three study waves **
# **  define your study waves **
baseline_wave      <- "Time 10"        # baseline measurement
exposure_waves     <- "Time 11"    # when exposure is measured
outcome_wave       <- "Time 12"        # when outcomes are measured
all_waves          <- c(baseline_wave, exposure_waves, outcome_wave)
time_factor           <- "time_factor"

cli::cli_h1("set waves for three-wave study ✔")

# define exposure variable ----------------------------------------------------
# ** key decision 2: define your exposure variable **
# +--------------------------+
name_exposure <- "religion_church"

# exposure variable labels
var_labels_exposure <- list(
  "religion_church" = "Religious Service (weekly)",
  "religion_church_binary" = "Religious Service (weekly)"
)

cli::cli_h1("set variable name for exposure ✔")

# define outcome variables -----------------------------------------------
# ** key decision 3: define outcome variables **
# here, we are focussing on a subset of wellbeing outcomes
# chose outcomes relevant to * your * study. Might be all/some/none/exactly
# these:
outcome_vars <- c(
  "log_charity_donate",
  "log_hours_charity",
  "support",
  "belong"
)
cli::cli_h1("set variable name for outcomes ✔")

# read and sort outcome variables -----------------------------------------
# we do this by domain: health, psych, present, life, social
read_and_sort <- function(key) {
  raw  <- here_read(key, push_mods)
  vars <- paste0("t2_", raw, "_z")
  sort(vars)
}
t2_outcome_z  <- read_and_sort("outcome_vars")

# save for script 3
here_save(t2_outcome_z, "t2_outcome_z")


label_mapping_all <- list(
  "t2_belong_z" = "Sense of Belonging",
  "t2_charity_donate_z" = "Charitable Donations (annual, log)",
  "t2_log_hours_charity_z" = "Volunteering (weekly, log)",
  "t2_support_z" = "Social Support"
)
# save
here_save(label_mapping_all, "label_mapping_all")

cli::cli_h1("created and saved label_mapping for use in graphs/tables ✔")

# define names for titles -------------------------------------------------
nice_exposure_name <- name_exposure %>%
  str_replace_all("_", " ") %>%
  stringr::str_to_title()

nice_exposure_name =  "Religious Service (binary)" #stringr::str_to_sentence(name_exposure)
nice_outcome_name = "Multi-Dimensional Cooperation"
title = glue::glue("Effect of {nice_exposure_name} on {nice_outcome_name}")
# save for final rport
here_save(title, "title")


# plot title --------------------------------------------------------------
title_binary = "Effects of {{name_exposure}} on {{name_outcomes}}"
filename_prefix = "grf_extraversion_wb"

# for manuscript later
margot::here_save(title_binary, "title_binary")


# +--------------------------+
# +--------------------------+
# define baseline variables -----------------------------------------------
# key decision 4 **  define baseline covariates **
# these are demographics, traits, etc. measured at baseline, that are common
# causes of the exposure and outcome.
# note we will automatically include baseline measures of the exposure and outcome
# later in the workflow.

baseline_vars <- c(
  # demographics
  "age", "born_nz_binary", "education_level_coarsen",
  "employed_binary", "eth_cat", "male_binary",
  "not_heterosexual_binary", "parent_binary", "partner_binary",
  "rural_gch_2018_l", "sample_frame_opt_in_binary",

  # personality traits (excluding exposure)
  "agreeableness", "conscientiousness", "neuroticism", "openness",

  # health and lifestyle
  "alcohol_frequency_weekly", "alcohol_intensity", "hlth_disability_binary",
  "log_hours_children", "log_hours_commute", "who_hours_exercise_num", # better measures
  "log_hours_housework", "log_household_inc",
  "short_form_health", "smoker_binary", "bmi_cat_num", # new & better measures

  # social and psychological
  "belong", "nz_dep2018", "nzsei_13_l",
  "political_conservative"#, #"religion_identification_level", #<- taken by religion_church

  # # religious denominations
  # "religion_bigger_denominations" # <-*OPTIONAL
)

cli::cli_h1("set baseline covariate names  ✔")

# +--------------------------+

# after selecting your exposure/ baseline / outcome variables do not modify this
# code

# make binary variable (UNLESS YOUR EXPOSURE IS A BINARY VARIABLE)
exposure_var_binary = paste0(name_exposure, "_binary")

# make exposure variable list (we will keep both the continuous and binary variable)
exposure_var  <- c(name_exposure, paste0(name_exposure, "_binary"))

# sort for easier reference
baseline_vars <- sort(baseline_vars)
outcome_vars <- sort(outcome_vars)

# save key variables --------------------------------------------------------
ordinal_columns <- c("t0_education_level_coarsen",
                     "t0_eth_cat",
                     "t0_rural_gch_2018_l",
                     "t0_religion_bigger_denominations") # not used "t0_religion_church_cat"


margot::here_save(name_exposure, "name_exposure")
margot::here_save(var_labels_exposure,"var_labels_exposure")
margot::here_save(baseline_vars,"baseline_vars")
margot::here_save(exposure_var, "exposure_var")
margot::here_save(exposure_var_binary, "exposure_var_binary")
margot::here_save(outcome_vars, "outcome_vars")
margot::here_save(baseline_wave, "baseline_wave")
margot::here_save(exposure_waves, "exposure_waves")
margot::here_save(outcome_wave, "outcome_wave")
margot::here_save(ordinal_columns,"ordinal_columns")
margot::here_save(name_exposure, "name_exposure")


cli::cli_h1("saved names and labels to be used for manuscript  ✔")


# +--------------------------+
# select eligible participants ----------------------------------------------
# only include participants who have exposure data at baseline

# You might require tighter conditions
# for example, if you are interested in the effects of hours of childcare,
# you might want to select only those who were parents at baseline.
# talk to me if you think you might night tighter eligibility criteria.

ids_baseline <- dat_prep |>
  # allow missing exposure at baseline
  # this would give us greater confidence that we generalise to the target population
  # filter(wave == baseline_wave) |>
  # option: do not allow missing exposure at baseline
  # this gives us greater confidence that we recover a incident effect
  filter(time_factor == baseline_wave, !is.na(!!sym(name_exposure))) |>
  pull(id)

# make the data
dat_long_1 <- dat_prep |>
  filter(id %in% ids_baseline & time_factor %in% c(baseline_wave, exposure_waves, outcome_wave)) |>
  mutate(alert_level_error = ifelse(alert_level_combined == "no_alert" | alert_level_combined == "early_covid", 0, 1)) |>
  droplevels()# note that we might have more than one exposure wave

# censor if alert level 2 or 4
table(dat_long_1$alert_level_error)


test_error_df <- dat_prep |>  filter(year_measured == 1 & !is.na(alert_level_combined)) |> droplevels()
table1::table1(~ time_factor |alert_level_combined, test_error_df)

test_error_df_1<- dat_long_1 |>  filter(!is.na(alert_level_error)) |> droplevels()
table1::table1(~ time_factor |as.factor(alert_level_error), test_error_df_1)

# logic -- we don't want measurement error in religious service
# so we treat those with alert level > 1 as loss to follow up in the following wave, and then censored.
dat_long_censored  <- margot_censor_lead(
  dt = dat_long_1,
  id_var = "id",
  # cluster_id = "id",
  wave_var = "wave",
  condition_var = "alert_level_error",
  condition_value = 1,
  year_measured_var = "year_measured")

# 5930 censored

n_censored = 5930
here_save(n_censored, "n_censored")

length(ids_baseline)
length(unique(dat_long_censored$id))

# check data
table1::table1(~ religion_church | as.factor(wave), dat_long_censored)
table1::table1(~  religion_church| as.factor(wave), dat_long_1)
# +--------------------------+
# plot distribution to help with cutpoint decision
dat_long_exposure <- dat_long_1 |> filter(time_factor %in% exposure_waves)

outcome_data <- dat_long_1 |> filter(time_factor %in% outcome_wave)

mean(outcome_data$charity_donate,na.rm=TRUE)
sd(outcome_data$charity_donate,na.rm=TRUE)

# define cutpoints for graph ----------------------------------------------

# define cutpoints *-- these can be adjusted --*
cut_points = c(0, 1)

# to use later in positivity graph in manuscript
lower_cut <- cut_points[[1]]

upper_cut <- cut_points[[2]]
threshold <- '>' # if upper
inverse_threshold <- '<='
scale_range = 'scale range 1-7'


# save for manuscript
here_save(lower_cut, "lower_cut")
here_save(upper_cut, "upper_cut")
here_save(threshold, "threshold")
here_save(inverse_threshold, "inverse_threshold")
here_save(scale_range, "scale_range")


cli::cli_h1("set thresholds for binary variable (if variable is continuous) ✔")

df_graph <- dat_long_exposure |>  mutate(religion_church = round(ifelse(religion_church > 8, 8, religion_church)), 1)


# make graph
graph_cut <- margot::margot_plot_categorical(
  df_graph,
  col_name         = name_exposure,
  sd_multipliers = c(-1, 1), # select to suit
  binwidth = 1,
  # either use n_divisions for equal-sized groups:
  # n_divisions      = 2,
  # or use custom_breaks for specific values:
  custom_breaks    = cut_points,  # ** adjust as needed **
  # could be "lower", no difference in this case, as no one == 4
  cutpoint_inclusive = "lower",
  show_mean        = FALSE,
  show_median      = FALSE,
  show_sd          = FALSE
)
print(graph_cut)

# save your graph
margot::here_save(graph_cut, "graph_cut", push_mods)
margot_save_png(graph_cut,
                height = 10,
                width = 12,
                base_filename = "2025_church_graph_cut")



# create binary exposure variable based on chosen cutpoint
# we already have a binary variable
# dat_long_2 <- margot::create_ordered_variable(
#   dat_long_1,
#   var_name           = name_exposure,
#   custom_breaks      = cut_points,  # ** -- adjust based on your decision above -- **
#   cutpoint_inclusive = "upper"
# )


cli::cli_h1("created binary variable (if variable is continuous) ✔")

# process binary variables and log-transform --------------------------------
# convert binary factors to 0/1 format
dat_long_3 <- margot::margot_process_binary_vars(dat_long_1) #*--- note this is dat_long_1 for this study

# log-transform hours and income variables: tables for analysis (only logged versions of vars)
dat_long_final <- margot::margot_log_transform_vars(
  dat_long_3,
  vars            = c(starts_with("hours_"), "household_inc", "charity_donate"), # **--- think about this ---***
  prefix          = "log_",
  keep_original   = FALSE,
  exceptions = exposure_var  # omit original variables#  **--- think about this ---***
) |>
  # select only variables needed for analysis
  select(all_of(c(baseline_vars, exposure_var, outcome_vars, "id", "time_factor", "year_measured", "sample_weights"))) |>
  droplevels()


# check missing data --------------------------------------------------------
# this is crucial to understand potential biases
missing_summary <- naniar::miss_var_summary(dat_long_final)
print(missing_summary)
margot::here_save(missing_summary, "missing_summary", push_mods)

# visualise missing data pattern
# ** -- takes a while to render **
vis_miss <- naniar::vis_miss(dat_long_final, warn_large_data = FALSE)
print(vis_miss)
margot::here_save(vis_miss, "vis_miss", push_mods)

# calculate percentage of missing data at baseline
dat_baseline <- dat_long_final |> filter(time_factor == baseline_wave)
percent_missing_baseline <- naniar::pct_miss(dat_baseline)
margot::here_save(percent_missing_baseline, "percent_missing_baseline", push_mods)

# save prepared dataset for next stage --------------------------------------
margot::here_save(dat_long_final, "dat_long_final", push_mods)


cli::cli_h1("made and saved final long data set for further processign in script 02 ✔")


# check positivity --------------------------------------------------------

# check
threshold # defined above
upper_cut # defined above
name_exposure # defined above


# create transition matrices to check positivity ----------------------------
# this helps assess whether there are sufficient observations in all exposure states
dt_positivity <- dat_long_final |>
  filter(time_factor %in% c(baseline_wave, exposure_waves)) |>
  select(!!sym(name_exposure), id, time_factor) |>
  mutate(exposure = round(as.numeric(!!sym(name_exposure)), 0)) |>
  mutate(exposure = if_else(exposure > 8, 8, exposure)) |>
  # create binary exposure based on cutpoint
  mutate(exposure_binary = ifelse(exposure > upper_cut, 1, 0)) |> # check
  ## *-- modify this --*
  mutate(time_factor = as.numeric(time_factor) -1 )

# create transition tables
transition_tables <- margot::margot_transition_table(
  dt_positivity,
  state_var = "exposure",
  id_var = "id",
  waves = c(0, 1),
  wave_var  = "time_factor",
  table_name = "transition_table"
)

# check
print(transition_tables$tables[[1]])

# save
margot::here_save(transition_tables, "transition_tables", push_mods)

# create binary transition tables
transition_tables_binary <- margot::margot_transition_table(
  dt_positivity,
  state_var = "exposure_binary",
  id_var = "id",
  waves = c(0, 1),
  wave_var = "time_factor",
  table_name = "transition_table_binary"
)

# check
print(transition_tables_binary$tables[[1]])

# save
margot::here_save(transition_tables_binary, "transition_tables_binary", push_mods)

# create tables -----------------------------------------------------------
# baseline variable labels
df_nz_long$bmi_cat_num
var_labels_baseline <- list(
  # demographics
  "age" = "Age",
  "born_nz_binary" = "Born in New Zealand",
  "education_level_coarsen" = "Education Level",
  "employed_binary" = "Employed",
  "eth_cat" = "Ethnicity",
  "male_binary" = "Male",
  "not_heterosexual_binary" = "Non-heterosexual",
  "parent_binary" = "Parent",
  "partner_binary" = "Has Partner",
  "rural_gch_2018_l" = "Rural Classification",
  "sample_frame_opt_in_binary" = "Sample Frame Opt-In",

  # economic & social status
  "household_inc" = "Household Income",
  "log_household_inc" = "Log Household Income",
  "nz_dep2018" = "NZ Deprivation Index",
  "nzsei_13_l" = "Occupational Prestige Index",
  "household_inc" = "Household Income",


  # personality traits
  "agreeableness" = "Agreeableness",
  "conscientiousness" = "Conscientiousness",
  "neuroticism" = "Neuroticism",
  "openness" = "Openness",

  # beliefs & attitudes
  "political_conservative" = "Political Conservatism",
  "religion_identification_level" = "Religious Identification",

  # health behaviors
  "alcohol_frequency" = "Alcohol Frequency",
  "alcohol_intensity" = "Alcohol Intensity",
  "hlth_disability_binary" = "Disability Status",
  "smoker_binary" = "Smoker",
  "hours_exercise" = "Hours of Exercise",
  "bmi_cat_num" =  "Body Mass Index (WHO)",


  # time use
  "hours_children" = "Hours with Children",
  "hours_commute" = "Hours Commuting",
  "hours_exercise" = "Hours Exercising",
  "hours_housework" = "Hours on Housework",
  "log_hours_children" = "Log Hours with Children",
  "log_hours_commute" = "Log Hours Commuting",
   "who_hours_exercise_num" = "Hours Exercising (WHO)",
  "log_hours_housework" = "Log Hours on Housework",


  # Added (Optional)
  "religion_bigger_denominations" = "Major Religions"
)
here_save(var_labels_baseline, "var_labels_baseline")

# get names
var_labels_outcomes <- list(
  "log_hours_charity" = "Volunteering (weekly, log)",
  "log_charity_donate" = "Charitable Donations (annual, log)",
  "support" = "Social Support",
  "belong" = "Social Belonging"
)

# save for manuscript
here_save(var_labels_outcomes, "var_labels_outcomes")

# save all variable translations
var_labels_measures <- c(var_labels_baseline, var_labels_exposure, var_labels_outcomes)
var_labels_measures

# save for manuscript
here_save(var_labels_measures, "var_labels_measures")

# graph
nzavs_sample_weights <- dat_baseline$sample_weights
hist_sample_weights <- tibble(nzavs_sample_weights = nzavs_sample_weights) |>
  ggplot(aes(x = nzavs_sample_weights)) +
  geom_histogram()

# view
hist_sample_weights

# save
margot_save_png(hist_sample_weights,
                height = 10,
                width = 12,
                base_filename = "hist_sample_weights")

n_participants

# stabalise weights;
t0_sample_weights <- margot_trim_sample_weights(dat_baseline$sample_weights,  upper_quantile = .99)
hist(t0_sample_weights)
here_save(t0_sample_weights, "t0_sample_weights")


# make baseline table -----------------------------------------------------
baseline_table <- margot::margot_make_tables(
  data = dat_baseline,
  vars = baseline_vars,
  by =  time_factor,
  labels = var_labels_baseline,
  table1_opts = list(overall = FALSE, transpose = FALSE),
  format = "markdown"
)
print(baseline_table)
margot::here_save(baseline_table, "baseline_table", push_mods)

# create exposure table by wave
exposure_table <- margot::margot_make_tables(
  data = dat_long_final |> filter(time_factor %in% c(baseline_wave, exposure_waves)),
  vars = exposure_var,
  by = "time_factor",
  labels = var_labels_exposure,
  factor_vars = exposure_var_binary,
  table1_opts = list(overall = FALSE, transpose = FALSE),
  format = "markdown"
)
print(exposure_table)
margot::here_save(exposure_table, "exposure_table", push_mods)

# create outcomes table by wave
outcomes_table <- margot::margot_make_tables(
  data = dat_long_final |> filter(time_factor %in% c(baseline_wave, outcome_wave)),
  vars = outcome_vars,
  by = "time_factor",
  labels = var_labels_outcomes,
  format = "markdown"
)
print(outcomes_table)
margot::here_save(outcomes_table, "outcomes_table", push_mods)

# note: completed data preparation step -------------------------------------
# you're now ready for the next steps:
# 1. creating wide-format dataset for analysis
# 2. applying causal inference methods
# 3. conducting sensitivity analyses

# key decisions summary:
# exposure variable: extraversion
# study waves: baseline (2018), exposure (2019), outcome (2020)
# baseline covariates: demographics, traits, health measures (excluding exposure)
# outcomes: health, psychological, wellbeing, and social variables
# binary cutpoint for exposure: here, 4 on the extraversion scale
# label names for tables


# make timeline -----------------------------------------------------------
nzavs_wave_breaks <- list(
  "time 10" = c(as.Date("2018-06-18"), as.Date("2019-09-30")),
  "time 11" = c(as.Date("2019-10-01"), as.Date("2020-09-30")),
  "time 12" = c(as.Date("2020-10-01"), as.Date("2021-09-30"))
  # "time 13" = c(as.Date("2021-10-01"), as.Date("2022-09-20")),
  # "time 14" = c(as.Date("2022-09-21"), as.Date("2023-10-14")),
  # "time 15" = c(as.Date("2023-10-15"), as.Date("2024-10-25"))
)

# use margot's prepare panel data function.
df_test <- prepare_panel_data(
  dat_long_censored,
  wave_col = "wave",
  tscore_col = "tscore",
  id_col = "id",
  base_date = as.Date("2009-06-30"),
  wave_breaks = nzavs_wave_breaks
)


# make the time line and save it, recalling again that "push_mods" is a path on your computer
margot_plot_response_timeline(df_timeline = df_test$df_timeline,
                              n_total_participants = df_test$n_total_participants,
                              save = TRUE,
                              save_path = here::here(push_mods),
                              width = 12,
                              height = 8,
                              base_filename = "timeline_histogram",
                              title = "Panel Study Timeline",
                              x_label = "Year",
                              y_label = "Count of Responses",
                              color_palette = NULL,
                              save_png = FALSE,
                              use_timestamp = FALSE)

# make the time line and save it, recalling again that "push_mods" is a path on your computer


# scale ranges ------------------------------------------------------------
scale_range_exposure <- c(0,8)
scale_ranges_outcomes <- c(1, 7) # sat nz environment 0-10

individual_plot_exposure <- margot_plot_individual_responses(
  dat_long_censored,
  y_vars = name_exposure,
  id_col = "id",
  waves = c(2018:2019),
  theme = theme_classic(),
  random_draws =56,
  title = NULL,
  y_label = NULL,
  x_label = NULL,
  color_palette = NULL,
  include_timestamp = FALSE,
  save_path = here::here(push_mods),
  width = 16,
  height = 8,
  seed = 123,
  full_response_scale = TRUE,
  scale_range = scale_range_exposure
)
individual_plot_exposure

# individual plot ---------------------------------------------------------
# exposure plot
individual_plot_exposure <- margot_plot_individual_responses(
  dat_long_censored,
  y_vars = name_exposure,
  id_col = "id",
  waves = c(2018,2019), # avoid biases note that 2020, 2021 church is imputed by carrying one forward
  theme = theme_classic(),
  random_draws =64,
  title = NULL,
  y_label = NULL,
  x_label = NULL,
  color_palette = NULL,
  include_timestamp = FALSE,
  save_path = here::here(push_mods),
  width = 16,
  height = 8,
  seed = 123,
  full_response_scale = TRUE,
  scale_range = scale_range_exposure
)

# view
individual_plot_exposure

# check size of object
margot_size( individual_plot_exposure )

