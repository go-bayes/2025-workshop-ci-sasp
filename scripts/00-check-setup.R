# Check Workshop Setup
# Run this script to ensure all packages are properly installed

cat("Checking Workshop Setup...\n")
cat("================================\n\n")

# Function to check package
check_package <- function(pkg, github = FALSE) {
  if (requireNamespace(pkg, quietly = TRUE)) {
    ver <- packageVersion(pkg)
    cat(sprintf("✓ %s (v%s) installed\n", pkg, ver))
    return(TRUE)
  } else {
    cat(sprintf("✗ %s not installed\n", pkg))
    if (github) {
      cat(sprintf("  Install with: devtools::install_github('%s')\n", github))
    } else {
      cat(sprintf("  Install with: install.packages('%s')\n", pkg))
    }
    return(FALSE)
  }
}

# Check R version
cat("R Version Check:\n")
r_version <- R.Version()
cat(sprintf("Current R version: %s.%s.%s\n",
            r_version$major,
            r_version$minor,
            r_version$patch))
if (as.numeric(r_version$major) >= 4 && as.numeric(r_version$minor) >= 1) {
  cat("✓ R version is sufficient (>= 4.1.0)\n\n")
} else {
  cat("✗ Please update R to version 4.1.0 or higher\n\n")
}

# Check CRAN packages
cat("CRAN Packages:\n")
cran_pkgs <- c("tidyverse", "grf", "policytree", "here",
               "knitr", "kableExtra", "patchwork", "DiagrammeR",
               "qs", "devtools")

cran_ok <- sapply(cran_pkgs, check_package)

# Check GitHub packages
cat("\nGitHub Packages:\n")
github_pkgs <- list(
  margot = "go-bayes/margot",
  boilerplate = "go-bayes/boilerplate",
  margot.sim = "go-bayes/margot.sim"
)

github_ok <- mapply(check_package,
                   names(github_pkgs),
                   github_pkgs,
                   SIMPLIFY = TRUE)

# Summary
cat("\n================================\n")
cat("Setup Summary:\n")
total_required <- length(cran_pkgs) + length(github_pkgs)
total_installed <- sum(c(cran_ok, github_ok))

if (total_installed == total_required) {
  cat(sprintf("✓ All packages installed (%d/%d)\n",
              total_installed, total_required))
  cat("✓ You're ready for the workshop!\n")
} else {
  cat(sprintf("✗ Missing packages (%d/%d installed)\n",
              total_installed, total_required))
  cat("✗ Please install missing packages before the workshop\n")
}

# Test that scripts exist
cat("\nChecking workshop scripts:\n")
scripts <- c(
  "scripts/01-simulate-religion-baseline-simple.R",
  "scripts/02-baseline-ate-att-atc.R",
  "scripts/03-simulate-religion-heterogeneity-rich.R",
  "scripts/04-advanced-heterogeneity.R"
)

scripts_exist <- file.exists(here::here(scripts))
if (all(scripts_exist)) {
  cat("✓ All workshop scripts found\n")
} else {
  cat("✗ Some scripts missing. Please ensure you're in the correct directory.\n")
}

cat("\nSetup check complete!\n")

