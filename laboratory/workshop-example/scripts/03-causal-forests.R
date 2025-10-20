# Causal Forests and Heterogeneity Analysis
# Script 3: Religion → Cooperation Analysis Using GRF
# This script estimates treatment effects and evaluates heterogeneity

# restart fresh session for clean workspace
rstudioapi::restartSession(clean = TRUE)

# set seed for workshop reproducibility
seed = 2025
set.seed(seed)

# essential library ---------------------------------------------------------
if (!require(margot, quietly = TRUE)) {
  devtools::install_github("go-bayes/margot")
  library(margot)
}

# min version of margot
if (packageVersion("margot") < "1.0.220") {
  stop(
    "please install margot >= 1.0.220 for this workflow\n
       run: devtools::install_github(\"go-bayes/margot\")
"
  )
}

# call library
library("margot")

# check package version
packageVersion(pkg = "margot")


# load libraries ----------------------------------------------------------
# pacman will install missing packages automatically
pacman::p_load(
  tidyverse,
  # data wrangling + plotting
  qs,
  here,# project-relative file paths
  data.table,# fast data manipulation
  fastDummies,# dummy variable creation
  naniar,# missing data handling
  skimr,# summary statistics
  grf,
  ranger,
  doParallel,
  kableExtra,
  ggplot2 ,
  rlang ,
  purrr ,
  patchwork,
  janitor,  # nice labels
  glue,
  cli,
  future,
  crayon,
  glue,
  stringr,
  future,
  furrr
)

# directory path configuration -----------------------------------------------
# workshop data directory (automatically configured)
push_mods <- here::here('/Users/joseph/v-project Dropbox/data/25/grf-church-coop')

# read original data (for plots) ------------------------------------------
original_df <- margot::here_read("df_wide", push_mods)

# import names ------------------------------------------------------------
name_exposure <- margot::here_read("name_exposure")
name_exposure

# make exposure names
t1_name_exposure_binary <- paste0("t1_", name_exposure, "_binary")

# check exposure name
t1_name_exposure_binary

# read outcome vars
outcome_vars <- margot::here_read("outcome_vars")

# read and sort outcome variables -----------------------------------------
# view
t2_outcome_z <- here_read("t2_outcome_z")

# define names for titles -------------------------------------------------
title <- here_read(
 "title"
)
# check
title

# plot title --------------------------------------------------------------
# for manuscript later
title_binary <- margot::here_read("title_binary")

# checks
title_binary


# combine outcomes ---------------------------------------------------------
# check outcome vars and make labels for graphs/tables
label_mapping_all <- here_read("label_mapping_all")

# check
label_mapping_all

cli::cli_h1("created and saved label_mapping for use in graphs/tables ✔")


# load GRF data and prepare inputs ----------------------------------------
df_grf <- margot::here_read('df_grf', push_mods)
E      <- margot::here_read('E', push_mods)

# check exposure binary
stopifnot(all(df_grf[[t1_name_exposure_binary]][!is.na(df_grf[[t1_name_exposure_binary]])] %in% 0:1))
# set exposure and weights

W <- as.vector(df_grf[[t1_name_exposure_binary]]) # note it is the processed weights for attrition "t1"

# old workflow
# weights <- df_grf$t1_adjusted_weights

# new weights workflow, use "combo_weights" -- see revised script 2
weights <- df_grf$combo_weights

hist(weights) # quick check for extreme weights
# select covariates and drop numeric attributes
X <- margot::remove_numeric_attributes(df_grf[E])

# set model defaults for workshop (optimised for demonstration)
grf_defaults <- list(seed = 2025,
                     min.node.size = 20,
                     stabilize.splits = TRUE,
                     num.trees = 1000)



# causal forest model ----------------

# !!!! THIS WILL TAKE TIME  !!!!!
# **----- COMMENT OUT AFTER YOU RUN TO AVOID RUNNING MORE THAN ONCE -----**
models_binary_ate <- margot_causal_forest(
  # <- could be 'margot_causal_forest_parrallel()' if you have a powerful computer
  data = df_grf,
  outcome_vars = t2_outcome_z,
  covariates = X,
  W = W,
  weights = weights,
  grf_defaults = grf_defaults,
  top_n_vars = 15,
  #<- can be modified but will affect run times
  save_models = TRUE,
  save_data = TRUE,
  # compute_conditional_means = TRUE,
  compute_marginal_only = TRUE,
  train_proportion = NULL,  # use all data for ATE
  seed = 2025
)
# # save to directory
here_save_qs(models_binary_ate, "models_binary_ate", push_mods)
# models_binary_ate <- here_read_qs("models_binary_ate", push_mods)


# compute models for heterogeneous effects using sample splitting
models_binary_cate <- margot_causal_forest(
  data = df_grf,
  outcome_vars = t2_outcome_z,
  covariates = X,
  W = W,
  weights = weights,
  grf_defaults = grf_defaults,
  top_n_vars = 15,
  save_models = TRUE,
  save_data = TRUE,
  compute_conditional_means = TRUE,
  train_proportion = 0.5,   # split for CATE
  seed = 2025
)

# save model
margot::here_save_qs(models_binary_cate, "models_binary_cate", push_mods)
cli::cli_h1("causal forest model completed and saved ✔")

# count models by category
# just a check
cat("Number of original models:\n",
    length(models_binary$results),
    "\n")


# make ate plots ----------------------------------------------------------
#   ************* NEW - CORRECTION FOR FAMILY-WISE ERROR **********
# make options -------------------------------------------------------------
# titles
ate_title = glue::glue("ATE Effects of {{nice_name_exposure}} on {{nice_name_outcome}}")
subtitle = ""
filename_prefix = "grf_"

here_save(ate_title, "ate_title")
here_save(filename_prefix, "filename_prefix")

# settings
x_offset = -.25
x_lim_lo = -.25
x_lim_hi = .25


# defaults for ate plots
base_defaults_binary <- list(
  type = "RD",
  title = ate_title,
  e_val_bound_threshold = 1.2,
  colors = c(
    "positive" = "#E69F00",
    "not reliable" = "grey50",
    "negative" = "#56B4E9"
  ),
  x_offset = x_offset,
  # will be set based on type
  x_lim_lo = x_lim_lo,
  # will be set based on type
  x_lim_hi = x_lim_hi,
  text_size = 8,
  linewidth = 0.75,
  estimate_scale = 1,
  base_size = 18,
  point_size = 4,
  title_size = 19,
  subtitle_size = 16,
  legend_text_size = 10,
  legend_title_size = 10,
  include_coefficients = FALSE
)

# health graph options
outcomes_options_all <- margot_plot_create_options(
  title = subtitle,
  base_defaults = base_defaults_binary,
  subtitle = subtitle,
  filename_prefix = filename_prefix
)


# read results back -------------------------------------------------------
models_binary_ate <- margot::here_read_qs("models_binary_ate", push_mods)



# graph tau(x) ------------------------------------------------------------
# multi-tau hats
plots_tau_hats <- margot_plot_tau(models_binary_ate, label_mapping = label_mapping_all)
plots_tau_hats
# str(models_binary, max.level = 1)
# str(models_binary$results, max.level = 1)
margot_save_png(plots_tau_hats,
                height = 10,
                width = 12,
                base_filename = "plots_tau_hats")

# ate graphs --------------------------------------------------------------
# then pass to the results
devtools::load_all("/Users/joseph/GIT/margot/")

ate_results <- margot_plot(
  models_binary_ate$combined_table,
  # <- now pass the corrected results.
  options = outcomes_options_all,
  include_coefficients = FALSE,
  save_output = FALSE,
  order = "evaluebound_asc",
  label_mapping = label_mapping_all,
  original_df = original_df,
  e_val_bound_threshold = 1.1,
  rename_ate = TRUE,
  adjust = "bonferroni",  #<- new
  alpha = 0.05 # <- new
)
ate_results$plot
ate_results$plot


# interpretation
cat(ate_results$interpretation)

# save
here_save_qs(ate_results, "ate_results", push_mods)


# make markdown tables (to be imported into the manuscript)
margot_bind_tables_markdown <- margot_bind_tables(
  ate_results$transformed_table,
  #list(all_models$combined_table),
  sort_E_val_bound = "desc",
  e_val_bound_threshold = 1.2,
  # ← choose threshold
  highlight_color = NULL,
  bold = TRUE,
  rename_cols = TRUE,
  col_renames = list("E-Value" = "E_Value", "E-Value bound" = "E_Val_bound"),
  rename_ate = TRUE,
  threshold_col = "E_Val_bound",
  output_format = "markdown",
  kbl_args = list(
    booktabs = TRUE,
    caption = NULL,
    align = NULL
  )
)

# view markdown table
# save for publication
here_save(margot_bind_tables_markdown, "margot_bind_tables_markdown")

# evaluate models ---------------------------------------------------------
# trim models if extreme propensity scores dominate
# diag_tbl_98 <- margot_inspect_qini(models_binary,
#                                        propensity_bounds = c(0.01, 0.99))

# # FLIPPING OUTCOMES  ------------------------------------------------------
#
# # note that the meaning of a heterogeneity will vary depending on our interests.
# # typically we are interested in whether an exposure improves life, and whether there is variability (aka HTE) in degrees of improvement.
# # in this case we must take negative outcomes and "flip" them -- recalculating the policy trees and qini curves for each
# # for example if the outcome is depression, then by flipping depression we better understand how the exposure *reduces* depression.
# # what if the exposure is harmful? say what if we are interested in the effect of depression on wellbeing? In that case, we might
# # want to "flip" the positive outcomes. That is, we might want to understand for whom a negative exposure is extra harmful.
# # here we imagine that extroversion is generally positive in its effects, and so we "flip" the negative outcomes.
# # if you were interested in a negative exposure, say "neuroticism" then you would probably want to flip the positive outcomes.
# # note there are further questions we might ask. We might consider who responds more 'weakly" to a negative exposure (or perhaps to a positive exposure).
# # Such a question could make sense if we had an exposure that was generally very strong.
# # however, let's stay focussed on evaluating evaluating strong responders. We will flip the negative outcomes if we expect the exposure is positive,
# # and flip the positive outcomes if we expect the exposure to be generally negative.
# # if there is no natural "positive" or negative, then just make sure the valence of the outcomes aligns, so that all are oriented in the same
# # direction if they have a valence.  if unsure, just ask for help!
#
# # flipping models: outcomes we want to minimise given the exposure --------
# # standard negative outcomes/  not used in this example
# # flipping models: outcomes we want to minimise given the exposure --------
# # standard negative outcomes/  not used in this example
#
#
# # +--------------------------+
# # |    MODIFY THIS           |
# # +--------------------------+
#
# # WHICH OUTCOMES -- if any ARE UNDESIREABLE?
# flip_outcomes_standard = c(
#   #"t2_alcohol_frequency_weekly_z",
#   #"t2_alcohol_intensity_z",
#   #"t2_hlth_bmi_z",
#   #"t2_hlth_fatigue_z",
#   "t2_kessler_latent_anxiety_z",
#   #  ← select
#   "t2_kessler_latent_depression_z",
#   #  ← select
#   "t2_rumination_z" #  ← select
#   #"t2_perfectionism_z" # the exposure variable was not investigated
# )
#
# # when exposure is negative and you want to focus on how much worse off
#
# # NOTE IF THE EXPOSURE IS NEGATIVE, FOCUS ON WHICH OUTCOMES, if any, ARE POSITIVE AND FLIP THESE?
# # flip_outcomes<- c( setdiff(t2_outcomes_all, flip_outcomes_standard) )
#
# # our example has the exposure as positive
# flip_outcomes <- flip_outcomes_standard
#
# # save
# here_save(flip_outcomes, "flip_outcomes")
#
# # check
# flip_outcomes
# label_mapping_all
#
#
# # +--------------------------+
# # |   END MODIFY             |
# # +--------------------------+
#
# # get labels
# flipped_names <- margot_get_labels(flip_outcomes, label_mapping_all)
#
# # check
# flipped_names
#
# # save for publication
# here_save(flipped_names, "flipped_names")
#
# cli::cli_h1("flipped outcomes identified and names saved ✔")
#
#
# # flip negatively oriented outcomes --------------------------------------
#
# # +--------------------------+
# # |       DO NOT ALTER       |
# # +--------------------------+
#
#
# # flip models using margot's function
#
# #  *** this will take some time ***
#
# # ** give it time **
# # ** once run/ comment out **
#
# # +--------------------------+
# # |          ALERT           |
# # +--------------------------+
# # !!!! THIS WILL TAKE TIME  !!!!!
# # can be margot_flip_forests_parrallel() if you have sufficient compute, set GB = something less than your system RAM
# models_binary_flipped_all <- margot_flip_forests(models_binary,
#                                                  flip_outcomes = flip_outcomes_standard,
#                                                  recalc_policy = TRUE)
#
# cli::cli_h1("flipped forest models completed ✔")
# # !!!! THIS WILL TAKE TIME  !!!!!
# # save
# here_save_qs(models_binary_flipped_all,
#              "models_binary_flipped_all",
#              push_mods)
#
#
# # +--------------------------+
# # |          ALERT           |
# # +--------------------------+
# # !!!! THIS WILL TAKE TIME  !!!!!
# # read back if needed
# models_binary_flipped_all <- here_read_qs("models_binary_flipped_all", push_mods)
#
#
# # this is a new function requires margot 1.0.48 or higher
# label_mapping_all_flipped <- margot_reversed_labels(label_mapping_all, flip_outcomes)
#
# # view
# label_mapping_all_flipped
#
# # save flipped labels
# here_save(label_mapping_all_flipped, "label_mapping_all_flipped")
#
# # +--------------------------+
# # |        END ALERT         |
# # +--------------------------+
#
#
# # +--------------------------+
# # |       DO NOT ALTER       |
# # +--------------------------+
#

# ──────────────────────────────────────────────────────────────────────────────
# WORKSHOP EXAMPLE: HETEROGENEITY ANALYSIS WORKFLOW
# PURPOSE: Demonstrate complete heterogeneity pipeline for religion → cooperation
#          - Screen outcomes for heterogeneity with omnibus tests
#          - Plot RATE AUTOC & QINI curves for effect prioritisation
#          - Fit policy trees for actionable subgroup identification
#          - Generate interpretable summaries for workshop participants
# WORKSHOP FOCUS: Understanding when and for whom religious service affects cooperation
# DATA: Synthetic NZAVS data (n=20,000) with cooperation outcomes
# ──────────────────────────────────────────────────────────────────────────────

# check package version early
stopifnot(utils::packageVersion("margot") >= "1.0.209")



# step 1: omnibus hetero test ---------------------------------------------

# read results back -------------------------------------------------------
models_binary_cate <- margot::here_read_qs("models_binary_cate", push_mods)
models_binary_cate

model_analysis <- models_binary_cate
# define models and labels
label_mapping = label_mapping_all

# run analysis (check that label mapping)
hte_test_cv <- margot_interpret_heterogeneity(
  models_binary_cate,
  label_mapping =  label_mapping,
  spend_levels = c(.1,.4), #<- default
  #alpha = 0.20,
  adjust = "none", #<- personality is well defined set of outcomes
  parallel = FALSE, #<- TRUE is currently running slower margot v 1.0.201
  include_extended_report = TRUE,
  use_cross_validation = TRUE,
  cv_num_folds = 5,
  seed = 2025
)


# save result
here_save(hte_test_cv, "hte_test_cv")


hte_test_cv <- here_read("hte_test_cv")

# check results
cat(hte_test_cv$extended_report)
print( hte_test_cv$evidence_summary, n = 10)

# view results
hte_test_cv$all_selected_model_ids
hte_test_cv$omnibus_results
hte_test_cv$cv_results$cv_results
hte_test_cv$rate_results$autoc
hte_test_cv$rate_results$qini
hte_test_cv$rate_results$raw_results


# 2  PLOT RATE AUTOC CURVES ---------------------------------------------------
# policy_tree_defaults <- list(
#   point_alpha              = 0.5,
#   title_size               = 12,
#   subtitle_size            = 12,
#   axis_title_size          = 12,
#   legend_title_size        = 12,
#   split_line_color         = "red",
#   split_line_alpha         = 0.8,
#   split_label_color        = "red",
#   split_label_nudge_factor = 0.007
# )
#
# decision_tree_defaults <- list(
#   span_ratio        = 0.2,
#   text_size         = 4,
#   y_padding         = 0.5,
#   edge_label_offset = 0.05,
#   border_size       = 0.01
# )
#
# qini_results_init <- margot_plot_qini_batch(
#   models_analysis,
#   # decision_tree_args = policy_tree_defaults,
#   # policy_tree_args   = policy_tree_defaults,
#   qini_args          =  list(show_ci = "cate"),
#   # model_names        =  $reliable_model_ids,
#   original_df        = original_df,
#   spend_levels = c(.1, .4),
#   label_mapping      = label_mapping_all,
#   # max_depth          = 2L,
#   output_objects     = c("qini_plot", "diff_gain_summaries")
# )
# qini_results_init$model_t2_belong_z$qini_plot

# qini results
# devtools::load_all("/Users/joseph/GIT/margot/")

# use reliable ids
qini_plots <- margot_plot_qini_batch(
  models_binary_cate,
  ci_n_points = 20,
  # baseline_method = "auto",
  # horizontal_line = TRUE,
  treatment_cost = 1,
  ylim = c(-.01, 0.2),
  show_ci = "cate",
  # model_names      = hte_test_cv$all_selected_model_ids,
  label_mapping      = label_mapping_all
)
qini_plots[[2]]
#
# qini_plots_unclear <- margot_plot_qini_batch(
#   models_analysis,
#   ci_n_points = 20,
#   # baseline_method = "auto",
#   # horizontal_line = TRUE,
#   treatment_cost = 1,
#   ylim = c(-.1,.5),
#   # show_ci = "cate",
#   # model_names      = hte_test_cv$unclear_model_names,
#   label_mapping      = label_mapping
# )

# policy trees ------------------------------------------------------------

# 4  POLICY TREES (max depth = 2) -------------------------------------------
devtools::load_all("/Users/joseph/GIT/margot/")
hte_test_cv$all_selected_model_ids

policy_tree_result <- margot_policy_tree_stability(
  models_binary_cate,
  model_names = hte_test_cv$selected_model_ids,
  label_mapping = label_mapping,
  n_iterations = 1000,
  train_proportion = .5,
  tree_method = "fastpolicytree",  # 10x faster!
  metaseed = 2025,
  depth = 2
)


# save
here_save_qs(policy_tree_result, "policy_tree_result", push_mods)

policy_tree_result <- here_read_qs("policy_tree_result", push_mods)
policy_tree_result$results$model_t2_belong_z

policy_tree_interpretation <- margot_interpret_stability_batch(
  policy_tree_result,
  format = "technical",
  label_mapping = label_mapping_all,
  stability_threshold = 0.2
)

devtools::load_all("/Users/joseph/GIT/margot/")
# Then use the enhanced results for interpretation
policy_text <- margot_interpret_policy_batch(
  policy_tree_result,
  # models = policy_tree_result_stability_with_means,
  original_df = original_df,
  output_format = "prose",
  label_mapping = label_mapping,
  max_depth = 2L,
  include_conditional_means = TRUE  # this should now work
)
cat(policy_text)


policy_plots <- margot_policy(
  policy_tree_result,
  original_df = original_df,
  output_objects = c("combined_plot"),
  label_mapping = label_mapping,
  max_depth = 2L,
  seed = 2025
)
policy_plots$model_t2_belong_z
policy_plots$model_t2_log_charity_donate_z
policy_plots$model_t2_support_z


# will be same for all models
vars_correlated <- margot_assess_variable_correlation(models_analysis, model_name = hte_test_cv$selected_model_ids[[1]], label_mapping = label_mapping)

# evaluate clusters
identify <- margot_identify_variable_clusters(vars_correlated, label_mapping = label_mapping)

# not working
diagnostics <- margot_stability_diagnostics(
  policy_tree_result,  # Your bootstrap results
  model_results = models_binary_flipped_all#,     # Original causal forest results
  # model_name =  "model_t2_conscientiousness_z"
)



# tests -------------------------------------------------------------------
# push_mods
# # test --------------------------------------------------------------------
# n <- nrow(X) # n in sample
#
# # define training sample
# toy <- sample(1:n, n / 4) # get half sample
#
# # test data
# toy_data = df_grf[toy, ]
#
# # check
# nrow(toy_data)
#
# # test covariates
# X_toy = X[toy, ]
#
# # check
# str(X_toy)
#
# # test exposure
# W_toy = W[toy]
#
# # test weights
# weights_toy = weights[toy]
#
#
#
# devtools::load_all("/Users/joseph/GIT/margot/")
# # devtools::load_all("/Users/joseph/GIT/margot/")
# t2_outcome_z
# outcome_vars = t2_outcome_z
#
# t2_outcome_z
#
# # # test model
# cf.test <- margot_causal_forest(
#   data = toy_data,
#   outcome_vars = c("t2_log_charity_donate_z","t2_log_hours_charity_z"),
#   flip_outcomes = "t2_log_hours_charity_z",
#   covariates = X_toy,
#   W = W_toy,
#   # qini_split = TRUE,
#   weights = weights_toy,
#   grf_defaults = grf_defaults,
#   save_data = TRUE,
#   compute_marginal_only = FALSE,
#   train_proportion  = 0.5,
#   top_n_vars = 15,
#   save_models = TRUE
# )
# cf.test$combined_table
#
# flip_outcomes = c(
#   "t2_log_hours_charity_z"
# )
#
# # save
#
# # get labels
# flipped_names <- margot_get_labels(flip_outcomes, label_mapping_all)
#
# # check
# flipped_names
#
# # this is a new function requires margot 1.0.48 or higher
# label_mapping_all_flipped <- margot_reversed_labels(label_mapping_all, flip_outcomes)
#
# # view reversed labels
# label_mapping_all_flipped
#
#
# test_table <- margot_plot(
#   cf.test$combined_table,
#   # <- now pass the corrected results.
#   options = outcomes_options_all,
#   include_coefficients = FALSE,
#   save_output = FALSE,
#   order = "evaluebound_asc",
#   label_mapping = label_mapping_all_flipped,
#   original_df = original_df,
#   e_val_bound_threshold = 1.1,
#   rename_ate = TRUE,
#   adjust = "bonferroni",  #<- new
#   alpha = 0.05 # <- new
# )
# test_table$plot
# test_table$interpretation
#
# #
# # # flip model
# # cf.test_flipped <- margot_flip_forests(cf.test,
# #                                        grf_defaults = grf_defaults,
# #                                        flip_outcomes = flip_outcomes)
#
# model_test <- cf.test
# label_mapping <- label_mapping_all
#
# devtools::load_all("/Users/joseph/GIT/margot/")
# # General HTE interpretation
# test_test_hte_test <- margot_interpret_heterogeneity(
#   model_test, label_mapping = label_mapping)
#
#
# #table
# test_test_hte_test$evidence_summary
#
# cat(test_test_hte_test$interpretation)
# cat(test_test_hte_test$extended_report)
# test_test_hte_test$cv_results$cv_results
# test_test_hte_test$rate_results$autoc
# test_test_hte_test$rate_results$qini
# test_test_hte_test$selected_model_ids
# test_test_hte_test$all_selected_model_ids
#
# devtools::load_all("/Users/joseph/GIT/margot/")
#
# policy_tree_result_stability_test <- margot_policy_tree_stability(
#   cf.test_flipped,
#   model_names = het_test_cv$selected_model_ids,
#   # exclude_covariates = c("t0_hlth_bmi_z", "_log"), #<- prone to error
#   # custom_covariates = t0_name_exposure_continuous,  # <-  include baseline exposure, but note it may not be a strong conditional predictor
#   # covariate_mode =  "add",
#   label_mapping = label_mapping_all_flipped,
#   n_iterations = 100,
#   train_proportion = .5,
#   tree_method = "fastpolicytree",  # 10x faster!
#   metaseed = 12345,
#   depth = 2
# )
#
# # save
# here_save_qs(policy_tree_result_stability_test, "policy_tree_result_stability_test", push_mods)
#
# policy_tree_interpretation <- margot_interpret_stability_batch(
#   policy_tree_result_stability_test,
#   format = "technical",
#   label_mapping = label_mapping_all_flipped,
#   stability_threshold = 0.2
# )
#
# #
# cat(policy_tree_interpretation)
#
#
#
# # Then use the enhanced results for interpretation
# policy_text <- margot_interpret_policy_batch(
#   policy_tree_result_stability_test,
#   # models = policy_tree_result_stability_with_means,
#   original_df = original_df,
#   output_format = "prose",
#   label_mapping = label_mapping_all_flipped,
#   max_depth = 2L,
#   include_conditional_means = TRUE  # this should now work
# )
#
# # And for plotting
# policy_plots <- margot_policy(
#   policy_tree_result_stability_test,
#   original_df = original_df,
#   output_objects = c("combined_plot"),
#   label_mapping = label_mapping_all_flipped,
#   max_depth = 2L
# )
#
# policy_plots[[1]]
#
#
# policy_text <- margot_interpret_policy_batch(
#   policy_tree_result_stability_test,
#   # models            = policy_tree_result_stability,
#   model_names = het_test_cv$selected_model_ids,
#   original_df       = original_df,
#   # model_names       = use_ids,
#   output_format     =   "prose",
#   label_mapping     = label_mapping_all_flipped,
#   max_depth         = 2L
# )
# cat(policy_text)
#
# # plots
# policy_plots <- margot_policy(
#   policy_tree_result_stability,
#   original_df       = original_df,
#   output_objects     = c("combined_plot"),
#   label_mapping     = label_mapping_all_flipped,
#   max_depth         = 2L
# )
#
# policy_plots$model_t2_conscientiousness_z
# policy_plots$model_t2_honesty_humility_z
# cat(policy_text)
#
#
#


#
# sd_log <- sd(original_df$t2_log_charity_donate, na.rm=TRUE)
#
#
# delta_log <- 0.14 * sd_log # say the effect was .14
# ratio     <- exp(delta_log)
# pct_change <- (ratio - 1) * 100
# pct_change
# # recover original dollars
# dollars     <- exp(original_df$t2_log_charity_donate) - 1
# mean_dollars <- mean(dollars, na.rm = TRUE)
#
# abs_change  <- mean_dollars * (ratio - 1)
# # print results
# list(
#   delta_log    = delta_log,
#   pct_change   = pct_change,
#   abs_change   = abs_change
# )
#
# # 1. sd on the log(dollars + 1) scale
# sd_log <- sd(original_df$log_ch)
#
# # 2. effect in log‐units
# delta_log <- 0.14 * sd_log
#
# # 3. multiplicative change on dollars+1
# ratio      <- exp(delta_log)
# pct_change <- (ratio - 1) * 100  # percent increase
#
# # 4. absolute change in dollars (approximate, using the sample mean)
# dollars      <- exp(original_df$t2_log_charity_donate) - 1
# mean_dollars <- mean(dollars)
# abs_change   <- mean_dollars * (ratio - 1)
#
# # print results
# list(
#   delta_log    = delta_log,
#   pct_change   = pct_change,
#   abs_change   = abs_change
# )
# And for plotting
