# Setup script for Cryptocurrency-Commodity Correlation Analysis Project
# FA-582 Group 7

# Install required packages if not already installed
required_packages <- c(
  "quantmod",           # Yahoo Finance data retrieval
  "tidyquant",          # Tidy financial workflows
  "PerformanceAnalytics", # Correlation and performance metrics
  "ggplot2",            # Data visualization
  "dplyr",              # Data manipulation
  "tidyr",              # Data tidying
  "zoo",                # Time series data manipulation
  "xts",                # Time series data manipulation
  "knitr",              # Report generation
  "rmarkdown",          # R Markdown rendering
  "lubridate",          # Date handling
  "corrplot",           # Correlation plots
  "gridExtra",          # Arrange multiple plots
  "scales",             # Scale functions for plots
  "tseries"             # Time series analysis (ADF tests)
)

# Install missing packages
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) {
  install.packages(new_packages, repos = "https://cran.rstudio.com/")
}

# Load all required packages
library(quantmod)
library(tidyquant)
library(PerformanceAnalytics)
library(ggplot2)
library(dplyr)
library(tidyr)
library(zoo)
library(xts)
library(knitr)
library(rmarkdown)
library(lubridate)
library(corrplot)
library(gridExtra)
library(scales)
library(tseries)

# Set working directory to project root
if (!dir.exists("data")) {
  dir.create("data", recursive = TRUE)
  dir.create("data/raw", recursive = TRUE)
  dir.create("data/processed", recursive = TRUE)
}

if (!dir.exists("scripts")) {
  dir.create("scripts")
}

if (!dir.exists("output")) {
  dir.create("output", recursive = TRUE)
  dir.create("output/figures", recursive = TRUE)
}

if (!dir.exists("reports")) {
  dir.create("reports")
}

cat("Setup complete! All required packages loaded.\n")
cat("Project directories created.\n")

