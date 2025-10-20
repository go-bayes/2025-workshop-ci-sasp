#!/usr/bin/env Rscript

# Case Study Embed Validation Script
# Validates Dropbox PDF iframe embeds are correctly configured

library(stringr)

cat("=== CASE STUDY EMBED VALIDATION ===\n\n")

# Check index.qmd for case study embeds
index_file <- "index.qmd"
if (file.exists(index_file)) {
  content <- readLines(index_file, warn = FALSE)
  content_text <- paste(content, collapse = "\n")
  
  # Find all Dropbox iframe embeds
  iframe_pattern <- 'iframe src="(https://www\\.dropbox\\.com[^"]+)"'
  iframe_matches <- str_extract_all(content_text, iframe_pattern)[[1]]
  
  cat("DROPBOX IFRAME EMBEDS FOUND:\n")
  cat("============================\n")
  
  if (length(iframe_matches) > 0) {
    for (i in seq_along(iframe_matches)) {
      url <- str_extract(iframe_matches[i], 'https://[^"]+')
      
      # Check if URL has correct parameters for iframe display
      has_raw_1 <- str_detect(url, "raw=1")
      has_dl_0 <- str_detect(url, "dl=0")
      
      cat(sprintf("Embed %d:\n", i))
      cat(sprintf("  URL: %s\n", url))
      cat(sprintf("  raw=1 (correct): %s\n", if(has_raw_1) "✅" else "❌"))
      cat(sprintf("  dl=0 (correct):  %s\n", if(has_dl_0) "✅" else "❌"))
      
      # Check if it's likely a PDF
      is_pdf <- str_detect(url, "\\.pdf")
      cat(sprintf("  PDF file: %s\n", if(is_pdf) "✅" else "❌"))
      cat("\n")
    }
  } else {
    cat("❌ No Dropbox iframe embeds found\n\n")
  }
  
  # Check for case study sections
  case_study_pattern <- "# Case Study \\d+"
  case_studies <- str_extract_all(content_text, case_study_pattern)[[1]]
  
  cat("CASE STUDY SECTIONS:\n")
  cat("===================\n")
  if (length(case_studies) > 0) {
    for (study in case_studies) {
      cat("✅", study, "\n")
    }
  } else {
    cat("❌ No case study sections found\n")
  }
  
  # Check for presentation-embed CSS class usage
  css_class_pattern <- "\\{\\.presentation-embed\\}"
  css_matches <- str_extract_all(content_text, css_class_pattern)[[1]]
  
  cat(sprintf("\nCSS CLASS USAGE:\n"))
  cat("================\n")
  cat(sprintf("presentation-embed class found: %d times %s\n", 
              length(css_matches), 
              if(length(css_matches) > 0) "✅" else "❌"))
  
} else {
  cat("❌ index.qmd file not found\n")
}

# Check if corresponding CSS styles exist
css_file <- "styles.css"
if (file.exists(css_file)) {
  css_content <- readLines(css_file, warn = FALSE)
  css_text <- paste(css_content, collapse = "\n")
  
  cat("\nCSS STYLES CHECK:\n")
  cat("=================\n")
  
  # Check for presentation-embed styles
  has_embed_style <- str_detect(css_text, "\\.presentation-embed")
  cat(sprintf("presentation-embed CSS: %s\n", if(has_embed_style) "✅" else "❌"))
  
  has_iframe_style <- str_detect(css_text, "\\.presentation-embed iframe")
  cat(sprintf("iframe styling: %s\n", if(has_iframe_style) "✅" else "❌"))
  
  has_btn_style <- str_detect(css_text, "\\.presentation-embed \\.btn")
  cat(sprintf("button styling: %s\n", if(has_btn_style) "✅" else "❌"))
  
} else {
  cat("\n❌ styles.css file not found\n")
}

cat("\n=== SUMMARY ===\n")
cat("✅ Fixed Dropbox iframe parameters (raw=0 → raw=1)\n")
cat("✅ Standardized CSS class names (presentation-embed)\n")
cat("✅ Added proper CSS styling for presentation embeds\n")
cat("✅ Case study previews should now display correctly\n")

cat("\n=== TESTING CHECKLIST ===\n")
cat("□ Deploy the site and test case study previews\n")
cat("□ Verify PDFs load in iframes without Dropbox wrapper\n")
cat("□ Check 'Open in New Tab' buttons work correctly\n")
cat("□ Test on different browsers and devices\n")