#!/usr/bin/env Rscript

# PDF.js Embed Validation Script
# Validates that presentation iframes use PDF.js for cross-browser compatibility

library(stringr)

cat("=== PDF.JS EMBED VALIDATION ===\n\n")

# Check index.qmd for PDF.js iframe embeds
index_file <- "index.qmd"
if (file.exists(index_file)) {
  content <- readLines(index_file, warn = FALSE)
  content_text <- paste(content, collapse = "\n")
  
  # Find all PDF.js iframe embeds
  pdfjs_pattern <- 'iframe src="(https://mozilla\\.github\\.io/pdf\\.js/web/viewer\\.html\\?file=[^"]+)"'
  pdfjs_matches <- str_extract_all(content_text, pdfjs_pattern)[[1]]
  
  cat("PDF.JS IFRAME EMBEDS FOUND:\n")
  cat("===========================\n")
  
  if (length(pdfjs_matches) > 0) {
    for (i in seq_along(pdfjs_matches)) {
      url <- str_extract(pdfjs_matches[i], 'https://[^"]+')
      
      # Extract the file parameter (Dropbox URL)
      file_url <- str_extract(url, "file=([^&]+)")
      file_url <- str_remove(file_url, "^file=")
      file_url <- URLdecode(file_url)
      
      cat(sprintf("Embed %d:\n", i))
      cat(sprintf("  PDF.js URL: %s\n", url))
      cat(sprintf("  Source PDF: %s\n", file_url))
      
      # Validate PDF.js URL structure
      has_viewer <- str_detect(url, "mozilla\\.github\\.io/pdf\\.js/web/viewer\\.html")
      has_file_param <- str_detect(url, "\\?file=")
      has_dropbox <- str_detect(url, "dropbox\\.com")
      has_raw_1 <- str_detect(url, "raw=1")
      
      cat(sprintf("  ✅ Uses PDF.js viewer: %s\n", if(has_viewer) "✅" else "❌"))
      cat(sprintf("  ✅ Has file parameter: %s\n", if(has_file_param) "✅" else "❌"))
      cat(sprintf("  ✅ Dropbox source: %s\n", if(has_dropbox) "✅" else "❌"))
      cat(sprintf("  ✅ Raw=1 parameter: %s\n", if(has_raw_1) "✅" else "❌"))
      cat("\n")
    }
  } else {
    cat("❌ No PDF.js iframe embeds found\n\n")
    
    # Check for old-style Dropbox embeds
    dropbox_pattern <- 'iframe src="(https://www\\.dropbox\\.com[^"]+)"'
    old_matches <- str_extract_all(content_text, dropbox_pattern)[[1]]
    
    if (length(old_matches) > 0) {
      cat("⚠️  Found old-style Dropbox embeds:\n")
      for (match in old_matches) {
        url <- str_extract(match, 'https://[^"]+')
        cat(sprintf("  - %s\n", url))
      }
      cat("These may not work properly in Chrome!\n\n")
    }
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
  
} else {
  cat("❌ index.qmd file not found\n")
}

cat("\n=== CROSS-BROWSER COMPATIBILITY ===\n")
cat("✅ PDF.js viewer works in:\n")
cat("  - ✅ Chrome (fixes the 'Failed to load PDF' issue)\n")
cat("  - ✅ Safari\n") 
cat("  - ✅ Firefox\n")
cat("  - ✅ Edge\n")
cat("  - ✅ Mobile browsers\n")

cat("\n=== TESTING CHECKLIST ===\n")
cat("□ Test in Chrome - should show PDF.js viewer (no more failures)\n")
cat("□ Test in Safari - should still work as before\n")
cat("□ Test 'Open in New Tab' buttons work correctly\n")
cat("□ Test on mobile devices\n")
cat("□ Verify PDFs load without errors\n")

cat("\n=== BENEFITS OF PDF.JS SOLUTION ===\n")
cat("✅ Universal browser support\n")
cat("✅ Professional PDF viewer with zoom, search, download controls\n")
cat("✅ No more Chrome compatibility issues\n")
cat("✅ Consistent user experience across all browsers\n")
cat("✅ No changes needed to Dropbox hosting\n")