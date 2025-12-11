# Script to render the PowerPoint presentation from presentation.Rmd
# FA-582 Group 7

cat("========================================\n")
cat("Rendering PowerPoint Presentation\n")
cat("========================================\n\n")

# Load required packages
if (!require("rmarkdown", quietly = TRUE)) {
  stop("rmarkdown package is required. Please run setup.R first.")
}

# Set working directory to project root (if not already)
if (basename(getwd()) != "FA582_Project") {
  if (dir.exists("FA582_Project")) {
    setwd("FA582_Project")
  }
}

# Check if presentation.Rmd exists
if (!file.exists("presentation.Rmd")) {
  stop("presentation.Rmd not found in current directory.")
}

cat("Rendering presentation.Rmd to PowerPoint...\n")
cat("This may take a few minutes...\n\n")

# Render the presentation
tryCatch({
  rmarkdown::render(
    input = "presentation.Rmd",
    output_format = "powerpoint_presentation",
    output_file = "presentation.pptx",
    quiet = FALSE
  )
  cat("\n========================================\n")
  cat("SUCCESS! Presentation generated:\n")
  cat("  presentation.pptx\n")
  cat("========================================\n")
}, error = function(e) {
  cat("\n========================================\n")
  cat("ERROR: Failed to render presentation\n")
  cat("Error message:", e$message, "\n")
  cat("========================================\n")
  stop(e)
})



