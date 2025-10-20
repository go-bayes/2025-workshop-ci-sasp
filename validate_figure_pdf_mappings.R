#!/usr/bin/env Rscript

# Comprehensive Figure-to-PDF Mapping Validation Script
# Prevents embarrassing duplicate PDF linking issues

library(stringr)

cat("=== FIGURE-TO-PDF MAPPING VALIDATION ===\n\n")

# Define expected figure-to-PDF mappings based on content analysis
expected_mappings <- data.frame(
  figure_id = c(
    "fig-conventions",
    "fig-general", 
    "fig-directedgraph",
    "fig-symbol-key",
    "fig-terminologyconfounders"
  ),
  expected_pdf = c(
    "1a-terminologylocalconventions.pdf",
    "S1-graphical-key.pdf",
    "1b-terminologydirectedgraph.pdf", 
    "S1-graphical-key.pdf",
    "S2-glossary.pdf"
  ),
  content_description = c(
    "Variable naming conventions (X, A, Y, L, U, M, etc.)",
    "Nodes, edges, conditioning conventions, arrow types",
    "Five elementary causal structures", 
    "Graphical conventions and symbol key",
    "Four rules of confounding control"
  ),
  stringsAsFactors = FALSE
)

# Function to extract figure mappings from QMD file
extract_figure_mappings <- function(file_path) {
  content <- readLines(file_path, warn = FALSE)
  
  mappings <- list()
  
  # Find all figure divs
  figure_lines <- which(grepl("^:::\\s*\\{#fig-", content))
  
  for (line_idx in figure_lines) {
    # Extract figure ID
    fig_line <- content[line_idx]
    fig_id <- str_extract(fig_line, "#(fig-[^}]+)")
    fig_id <- str_remove(fig_id, "^#")
    
    # Look for iframe in next few lines
    for (i in 1:5) {
      if ((line_idx + i) <= length(content)) {
        iframe_line <- content[line_idx + i]
        if (grepl("iframe.*src.*pdf", iframe_line)) {
          pdf_path <- str_extract(iframe_line, 'src="([^"]+\\.pdf)"')
          pdf_file <- basename(str_extract(pdf_path, '[^"]+\\.pdf'))
          
          mappings[[fig_id]] <- pdf_file
          break
        }
      }
    }
  }
  
  return(mappings)
}

# Analyze 01-background.qmd
bg_file <- "content/01-background.qmd"
if (file.exists(bg_file)) {
  cat("=== ANALYZING", bg_file, "===\n")
  
  current_mappings <- extract_figure_mappings(bg_file)
  
  # Create summary table
  results <- data.frame(
    figure_id = character(),
    current_pdf = character(),
    expected_pdf = character(),
    status = character(),
    content_description = character(),
    stringsAsFactors = FALSE
  )
  
  for (i in 1:nrow(expected_mappings)) {
    fig_id <- expected_mappings$figure_id[i]
    expected_pdf <- expected_mappings$expected_pdf[i]
    description <- expected_mappings$content_description[i]
    
    current_pdf <- ifelse(fig_id %in% names(current_mappings), 
                         current_mappings[[fig_id]], 
                         "NOT_FOUND")
    
    status <- ifelse(current_pdf == expected_pdf, "✅ CORRECT", 
                    ifelse(current_pdf == "NOT_FOUND", "❌ MISSING",
                          "❌ WRONG"))
    
    results <- rbind(results, data.frame(
      figure_id = fig_id,
      current_pdf = current_pdf,
      expected_pdf = expected_pdf,
      status = status,
      content_description = description,
      stringsAsFactors = FALSE
    ))
  }
  
  # Print results
  cat("\nFIGURE-TO-PDF MAPPING RESULTS:\n")
  cat("=====================================\n")
  for (i in 1:nrow(results)) {
    cat(sprintf("%-25s %s\n", results$figure_id[i], results$status[i]))
    cat(sprintf("  Current:  %s\n", results$current_pdf[i]))
    cat(sprintf("  Expected: %s\n", results$expected_pdf[i]))
    cat(sprintf("  Content:  %s\n", results$content_description[i]))
    cat("\n")
  }
  
  # Check for duplicates
  pdf_counts <- table(unlist(current_mappings))
  duplicates <- names(pdf_counts)[pdf_counts > 1]
  
  cat("=== DUPLICATE PDF USAGE CHECK ===\n")
  if (length(duplicates) > 0) {
    cat("❌ DUPLICATE PDFs FOUND:\n")
    for (dup_pdf in duplicates) {
      figures_using <- names(current_mappings)[unlist(current_mappings) == dup_pdf]
      cat(sprintf("- %s used by: %s\n", dup_pdf, paste(figures_using, collapse = ", ")))
    }
  } else {
    cat("✅ No duplicate PDF usage found\n")
  }
  
  # Check if PDFs exist
  cat("\n=== PDF FILE EXISTENCE CHECK ===\n")
  all_pdfs <- unique(c(results$current_pdf, results$expected_pdf))
  all_pdfs <- all_pdfs[all_pdfs != "NOT_FOUND"]
  
  for (pdf_file in all_pdfs) {
    pdf_path <- file.path("docs/pdfs/bulbulia-hand-outs", pdf_file)
    exists <- file.exists(pdf_path)
    status <- if (exists) "✅" else "❌"
    cat(sprintf("%s %s\n", status, pdf_file))
  }
  
  # Summary
  correct_count <- sum(results$status == "✅ CORRECT")
  total_count <- nrow(results)
  
  cat(sprintf("\n=== SUMMARY ===\n"))
  cat(sprintf("Correct mappings: %d/%d\n", correct_count, total_count))
  cat(sprintf("Duplicate usage issues: %d\n", length(duplicates)))
  
  if (correct_count == total_count && length(duplicates) == 0) {
    cat("\n🎉 ALL FIGURE-TO-PDF MAPPINGS ARE CORRECT! 🎉\n")
  } else {
    cat("\n⚠️  Issues found that need attention\n")
  }
  
} else {
  cat("❌ File not found:", bg_file, "\n")
}

cat("\n=== PREVENTIVE MEASURES ===\n")
cat("✅ This script should be run after any changes to figure references\n")
cat("✅ Consider adding this to your build/CI process\n")
cat("✅ Keep the expected_mappings table updated when adding new figures\n")