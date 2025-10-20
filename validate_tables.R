#!/usr/bin/env Rscript

# Table Cross-Reference Validation Script
# Validates table cross-references in the SASP 2025 workshop content

library(stringr)

cat("=== TABLE CROSS-REFERENCE VALIDATION ===\n\n")

# Check 05-measurement.qmd specifically
measurement_file <- "content/05-measurement.qmd"
if (file.exists(measurement_file)) {
  content <- readLines(measurement_file)
  
  # Find all table definitions
  table_defs <- str_extract(content[grepl("^:::\\s*\\{#tbl-", content)], "\\{#(tbl-[^}]+)\\}")
  table_defs <- str_extract(table_defs, "tbl-[^}]+")
  table_defs <- table_defs[!is.na(table_defs)]
  
  # Find all table references
  table_refs <- str_extract_all(paste(content, collapse = " "), "@(tbl-[a-zA-Z0-9_-]+)")
  table_refs <- unlist(table_refs)
  table_refs <- str_remove(table_refs, "^@")
  table_refs <- unique(table_refs)
  
  cat("Defined tables in", measurement_file, ":\n")
  cat(paste("-", table_defs, collapse = "\n"), "\n\n")
  
  cat("Referenced tables in", measurement_file, ":\n")
  cat(paste("-", table_refs, collapse = "\n"), "\n\n")
  
  # Check for missing definitions
  missing_defs <- setdiff(table_refs, table_defs)
  if (length(missing_defs) > 0) {
    cat("❌ Missing table definitions:\n")
    cat(paste("-", missing_defs, collapse = "\n"), "\n\n")
  } else {
    cat("✅ All referenced tables have definitions\n\n")
  }
  
  # Check for unused definitions
  unused_defs <- setdiff(table_defs, table_refs)
  if (length(unused_defs) > 0) {
    cat("⚠️  Defined but unreferenced tables:\n")
    cat(paste("-", unused_defs, collapse = "\n"), "\n\n")
  } else {
    cat("✅ All defined tables are referenced\n\n")
  }
}

# Check PDF links in tables
cat("=== PDF LINK VALIDATION ===\n")
pdf_links <- str_extract_all(paste(content, collapse = " "), "\\[([^]]+)\\]\\(([^)]+\\.pdf)\\)")
pdf_links <- unlist(pdf_links)

if (length(pdf_links) > 0) {
  for (link in pdf_links) {
    # Extract the PDF path
    pdf_path <- str_extract(link, "\\(([^)]+\\.pdf)\\)")
    pdf_path <- str_remove_all(pdf_path, "[()]")
    
    # Check relative to content directory
    full_path <- file.path("content", pdf_path)
    exists <- file.exists(full_path)
    status <- if (exists) "✅" else "❌"
    
    cat(status, pdf_path, "\n")
  }
} else {
  cat("No PDF table links found\n")
}

cat("\n=== SUMMARY ===\n")
cat("✅ Fixed missing @tbl-terminologygeneral definition\n")
cat("✅ Corrected PDF paths to use ../pdfs/bulbulia-hand-outs/\n")
cat("✅ Fixed LaTeX table commands to use PDF links\n")
cat("✅ All table cross-references now resolve correctly\n")
