# Workshop - Package Installation Script
# Installs causalworkshop and all required dependencies

cat("
╔══════════════════════════════════════════════════════════════╗
║                         Workshop                             ║
║           Causal Inference with Machine Learning             ║
╚══════════════════════════════════════════════════════════════╝

Install workshop dependencies...
")

# Required packages from CRAN
cran_packages <- c(
  "kableExtra",    # Tables
  "boilerplate"    # References // Methods
  "cli",           # Nice Messages
  "devtools",      # Package builds
  "dplyr",         # Data manipulation
  "grf",           # Generalized Random Forests
  "ggplot2",       # Data visualisation
  "here",          # Path management
  "knitr",         # Document generation
  "margot",        # Causal inference workflows. # not on CRAN
  "purrr",         # Functional programming
  "qs",            # Save Read large files
  "rmarkdown",     # R Markdown documents
  "scales",        # Scale functions for ggplot2
  "tibble",        # Modern data frames
  "tidyr",         # Data tidying/ labels
  "tidyverse"      # Complete tidyverse
)

# Check and install CRAN packages
cat("Installing CRAN packages...\n")
missing_cran <- setdiff(cran_packages, rownames(installed.packages()))

if (length(missing_cran) > 0) {
  cat("Installing:", paste(missing_cran, collapse = ", "), "\n")
  install.packages(missing_cran, dependencies = TRUE)
} else {
  cat("All CRAN packages already installed.\n")
}

# Install GitHub packages
cat("Installing GitHub packages...\n")

# Install margot from GitHub (latest version)
if (!requireNamespace("margot", quietly = TRUE) ||
    packageVersion("margot") < "1.0.250") {
  cat("Installing margot package...\n")
  devtools::install_github("go-bayes/margot")
}

# Install causalworkshop from GitHub
cat("Installing causalworkshop package...\n")
devtools::install_github("go-bayes/causalworkshop")

# Verify installation
cat("\n")
cat("Verifying installation...\n")

required_for_workshop <- c("causalworkshop", "margot", "grf", "dplyr", "ggplot2")
installation_status <- sapply(required_for_workshop, function(pkg) {
  if (requireNamespace(pkg, quietly = TRUE)) {
    version <- as.character(packageVersion(pkg))
    cat(sprintf("✓ %s (v%s)\n", pkg, version))
    return(TRUE)
  } else {
    cat(sprintf("✗ %s - FAILED TO INSTALL\n", pkg))
    return(FALSE)
  }
})

if (all(installation_status)) {
  cat("\n")
  cat("🎉 SUCCESS! All workshop packages installed correctly.\n")
  cat("\n")
  cat("You're ready for the6 Workshop!\n")
  cat("To get started, run: library(causalworkshop)\n")
  cat("\n")
} else {
  cat("\n")
  cat("⚠️  Some packages failed to install. Please check the errors above.\n")
  cat("You may need to install missing system dependencies.\n")
  cat("\n")
}

cat("For more information visit: https://github.com/go-bayes/causalworkshop\n")

