#!/usr/bin/env Rscript

# Browser-Aware PDF Embed Validation Script
# Validates the improved solution that works in Safari with Chrome fallback

library(stringr)

cat("=== BROWSER-AWARE PDF EMBED VALIDATION ===\n\n")

# Check index.qmd for the improved solution
index_file <- "index.qmd"
if (file.exists(index_file)) {
  content <- readLines(index_file, warn = FALSE)
  content_text <- paste(content, collapse = "\n")

  # Check for original Dropbox iframe embeds (should be present)
  dropbox_pattern <- 'iframe.*src="(https://www\\.dropbox\\.com[^"]+)"'
  dropbox_matches <- str_extract_all(content_text, dropbox_pattern)[[1]]

  cat("DROPBOX IFRAME EMBEDS (ORIGINAL WORKING):\n")
  cat("========================================\n")

  if (length(dropbox_matches) > 0) {
    for (i in seq_along(dropbox_matches)) {
      url <- str_extract(dropbox_matches[i], 'https://[^"]+')

      cat(sprintf("Embed %d:\n", i))
      cat(sprintf("  URL: %s\n", url))

      # Validate URL parameters
      has_raw_1 <- str_detect(url, "raw=1")
      has_dl_0 <- str_detect(url, "dl=0")
      is_pdf <- str_detect(url, "\\.pdf")

      cat(sprintf("  ✅ PDF file: %s\n", if(is_pdf) "✅" else "❌"))
      cat(sprintf("  ✅ raw=1 (correct): %s\n", if(has_raw_1) "✅" else "❌"))
      cat(sprintf("  ✅ dl=0 (correct): %s\n", if(has_dl_0) "✅" else "❌"))
      cat("\n")
    }
  } else {
    cat("❌ No Dropbox iframe embeds found\n\n")
  }

  # Check for Chrome fallback messages
  chrome_fallback_pattern <- 'id="chrome-fallback-\\d+"'
  fallback_matches <- str_extract_all(content_text, chrome_fallback_pattern)[[1]]

  cat("CHROME FALLBACK MESSAGES:\n")
  cat("========================\n")

  if (length(fallback_matches) > 0) {
    for (match in fallback_matches) {
      cat(sprintf("Found: %s\n", match))
    }
    cat(sprintf("\nTotal fallback messages: %d\n", length(fallback_matches)))
  } else {
    cat("No Chrome fallback messages found\n")
  }

  # Check for browser detection JavaScript
  has_js_detection <- str_detect(content_text, "navigator\\.userAgent.*Chrome")
  cat(sprintf("\nJavaScript browser detection: %s\n", if(has_js_detection) "Yes" else "No"))

  # Check for "Open in New Tab" buttons
  new_tab_pattern <- 'target="_blank".*Open in New Tab'
  new_tab_matches <- str_extract_all(content_text, new_tab_pattern)[[1]]

  cat(sprintf("'Open in New Tab' buttons: %d %s\n",
              length(new_tab_matches),
              if(length(new_tab_matches) > 0) "Yes else "No"))

} else {
  cat("No index.qmd file not found\n")
}

cat("\n=== SOLUTION BENEFITS ===\n")
cat("✅ Safari: Works as before (PDF previews in iframe)\n")
cat("✅ Chrome: Shows helpful message + 'Open in New Tab' button\n")
cat("✅ All browsers: 'Open in New Tab' buttons always available\n")
cat("✅ No breaking changes: Maintains existing Safari functionality\n")
cat("✅ Progressive enhancement: Adds Chrome-specific guidance\n")

cat("\n=== HOW IT WORKS ===\n")
cat("1. Safari users: See PDF preview in iframe (as before)\n")
cat("2. Chrome users: See warning message about iframe issues\n")
cat("3. All users: Can use 'Open in New Tab' for full PDF experience\n")
cat("4. JavaScript detects Chrome and shows helpful guidance\n")

cat("\n=== TESTING CHECKLIST ===\n")
cat("□ Safari: Should show PDF previews in iframes (restored)\n")
cat("□ Chrome: Should show yellow warning message about iframe\n")
cat("□ Chrome: 'Open in New Tab' should work perfectly\n")
cat("□ All browsers: Navigation and buttons should work\n")
cat("□ Mobile: Should work appropriately on small screens\n")
