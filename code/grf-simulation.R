#causalworkshop run script
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}

# download package
devtools::install_github("go-bayes/causalworkshop")

# boilerplate: reporting
if (!requireNamespace("boilerplate", quietly = TRUE)) {
  install.packages("boilerplate")
}

# margot -- optional
if (!requireNamespace("margot", quietly = TRUE)) {
  devtools::install_github("go-bayes/margot")
}


# load library
library(causalworkshop)


# opptional
check_workshop_prerequisites()


# Run a quick test
library(causalworkshop)

# Copy all workshop scripts to your working directory
get_workshop_scripts()

# See what scripts are available
list_workshop_scripts()

# Work through the scripts in order:
# 01-baseline-adjustment.R    - Foundation concepts
# 02-causal-forest-analysis.R - Core methodology
# 03-rate-qini-curves.R      - Performance evaluation
# 04-policy-trees.R          - Decision rules
# 05-margot-workflow.R       - Professional analysis
# 06-interpretation.qmd       - Results reporting
```

