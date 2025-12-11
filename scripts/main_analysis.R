# Main Analysis Script
# FA-582 Group 7: Cryptocurrency-Commodity Correlation Analysis
# This script orchestrates all analysis scripts in sequence

cat("========================================\n")
cat("Cryptocurrency-Commodity Correlation Analysis\n")
cat("FA-582 Group 7\n")
cat("========================================\n\n")

# Record start time
start_time <- Sys.time()

# Source setup script
cat("Step 1: Loading packages and setting up project...\n")
tryCatch(
  {
    source("setup.R")
    cat("  Setup complete\n\n")
  },
  error = function(e) {
    cat("  Error in setup:", e$message, "\n")
    stop("Setup failed. Please check setup.R")
  }
)

# Step 1: Data Collection
cat("Step 2: Collecting data from Yahoo Finance...\n")
tryCatch(
  {
    source("scripts/01_data_collection.R")
    cat("  Data collection complete\n\n")
  },
  error = function(e) {
    cat("  Error in data collection:", e$message, "\n")
    cat("  Attempting to continue with existing data files...\n\n")
  }
)

# Step 2: Data Preprocessing
cat("Step 3: Preprocessing data...\n")
tryCatch(
  {
    source("scripts/02_data_preprocessing.R")
    cat("  Preprocessing complete\n\n")
  },
  error = function(e) {
    cat("  Error in preprocessing:", e$message, "\n")
    stop("Preprocessing failed. Please check data files.")
  }
)

# Step 3: Exploratory Data Analysis
cat("Step 4: Performing Exploratory Data Analysis...\n")
tryCatch(
  {
    source("scripts/03_eda.R")
    cat("  EDA complete\n\n")
  },
  error = function(e) {
    cat("  Error in EDA:", e$message, "\n")
    stop("EDA failed. Please check preprocessing output.")
  }
)

# Step 4: Initial Analysis
cat("Step 5: Performing Initial Analysis...\n")
tryCatch(
  {
    source("scripts/04_initial_analysis.R")
    cat("  Initial analysis complete\n\n")
  },
  error = function(e) {
    cat("  Error in initial analysis:", e$message, "\n")
    stop("Initial analysis failed.")
  }
)

# Calculate elapsed time
end_time <- Sys.time()
elapsed_time <- difftime(end_time, start_time, units = "secs")

cat("========================================\n")
cat("Analysis Complete!\n")
cat("Total time:", round(elapsed_time, 2), "seconds\n")
cat("========================================\n\n")

cat("Output files generated:\n")
cat("  - Raw data: data/raw/*.csv\n")
cat("  - Processed data: data/processed/*.csv\n")
cat("  - Figures: output/figures/*.png\n")
cat("  - Summary statistics: output/summary_statistics.csv\n")
cat("  - Correlation matrix: output/correlation_matrix.csv\n")
cat("  - Test results: output/adf_test_results.csv\n\n")

cat("Next steps:\n")
cat("  - Render final report: rmarkdown::render('reports/progress_report.Rmd')\n")
