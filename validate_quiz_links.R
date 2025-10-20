#!/usr/bin/env Rscript

# Quiz Link Validation Script
# Validates all quiz PDF links are correctly configured and accessible

library(stringr)

cat("=== QUIZ LINK VALIDATION ===\n\n")

# Check _quarto.yml for quiz links
quarto_yml <- readLines("_quarto.yml", warn = FALSE)
quiz_links <- quarto_yml[grepl("quiz/.*\\.pdf", quarto_yml)]

cat("QUIZ LINKS IN NAVIGATION:\n")
cat("========================\n")

quiz_files <- c()
for (link_line in quiz_links) {
  # Extract href path
  href_match <- str_extract(link_line, 'href:\\s*([^\\s]+\\.pdf)')
  if (!is.na(href_match)) {
    quiz_path <- str_extract(href_match, '[^\\s:]+\\.pdf')
    quiz_files <- c(quiz_files, quiz_path)
    
    # Check if file exists in source
    source_path <- file.path(quiz_path)
    source_exists <- file.exists(source_path)
    
    # Check if file exists in docs output
    docs_path <- file.path("docs", quiz_path)
    docs_exists <- file.exists(docs_path)
    
    # Get file info
    text_match <- str_extract(link_line, 'text:\\s*"([^"]+)"')
    link_text <- if (!is.na(text_match)) str_extract(text_match, '"[^"]+"') else "Unknown"
    
    cat(sprintf("Link: %s\n", str_remove_all(link_text, '"')))
    cat(sprintf("  Path: %s\n", quiz_path))
    cat(sprintf("  Source exists: %s\n", if(source_exists) "✅" else "❌"))
    cat(sprintf("  Docs exists: %s\n", if(docs_exists) "✅" else "❌"))
    
    if (docs_exists) {
      file_info <- file.info(docs_path)
      cat(sprintf("  File size: %s bytes\n", file_info$size))
      cat(sprintf("  Modified: %s\n", file_info$mtime))
    }
    cat("\n")
  }
}

# Check quiz directory contents
cat("QUIZ DIRECTORY CONTENTS:\n")
cat("=======================\n")

if (dir.exists("quiz")) {
  source_files <- list.files("quiz", pattern = "\\.pdf$", full.names = FALSE)
  cat("Source quiz/ directory:\n")
  for (file in source_files) {
    cat(sprintf("  ✅ %s\n", file))
  }
} else {
  cat("❌ Source quiz/ directory not found\n")
}

if (dir.exists("docs/quiz")) {
  docs_files <- list.files("docs/quiz", pattern = "\\.pdf$", full.names = FALSE)
  cat("\nDocs quiz/ directory:\n")
  for (file in docs_files) {
    cat(sprintf("  ✅ %s\n", file))
  }
} else {
  cat("❌ Docs quiz/ directory not found\n")
}

# Check resources configuration
cat("\nRESOURCES CONFIGURATION:\n")
cat("=======================\n")
resources_lines <- quarto_yml[grepl("resources:", quarto_yml, ignore.case = TRUE)]
if (length(resources_lines) > 0) {
  cat("Found resources configuration:\n")
  # Find the resources section
  resource_start <- which(grepl("resources:", quarto_yml, ignore.case = TRUE))
  if (length(resource_start) > 0) {
    for (i in resource_start:(resource_start + 5)) {
      if (i <= length(quarto_yml)) {
        line <- quarto_yml[i]
        if (grepl("^\\s*-", line) || grepl("resources:", line)) {
          cat(sprintf("  %s\n", line))
          if (grepl("quiz", line)) {
            cat("    ✅ Quiz resources configured for copying\n")
          }
        }
      }
    }
  }
} else {
  cat("❌ No resources configuration found\n")
}

cat("\n=== DIAGNOSIS ===\n")
if (length(quiz_files) > 0) {
  all_exist_local <- all(sapply(quiz_files, function(f) file.exists(file.path("docs", f))))
  if (all_exist_local) {
    cat("✅ All quiz files exist locally in docs/quiz/\n")
    cat("💡 LIKELY ISSUE: GitHub Pages deployment problem\n")
    cat("\nPOSSIBLE CAUSES:\n")
    cat("- Git LFS (Large File Storage) not configured for PDFs\n")
    cat("- PDF files not committed to repository\n")
    cat("- GitHub Pages build process not copying quiz files\n")
    cat("- File permissions or case sensitivity issues\n")
    
    cat("\nRECOMMENDED ACTIONS:\n")
    cat("1. Check if PDF files are in the GitHub repository\n")
    cat("2. Verify git status shows quiz files as committed\n")
    cat("3. Check GitHub Actions build logs for errors\n")
    cat("4. Test the links after redeployment\n")
  } else {
    cat("❌ Some quiz files missing from local docs directory\n")
    cat("💡 Run 'quarto render' to regenerate docs\n")
  }
} else {
  cat("❌ No quiz links found in navigation\n")
}