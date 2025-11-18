# Data Collection Script
# FA-582 Group 7: Cryptocurrency-Commodity Correlation Analysis
# This script fetches historical data from Yahoo Finance API

# Load required libraries
library(quantmod)
library(dplyr)
library(lubridate)

# Set date range
start_date <- as.Date("2020-01-01")
end_date <- as.Date("2024-09-30")

# Define tickers
tickers <- c(
  "BTC-USD" = "Bitcoin",
  "ETH-USD" = "Ethereum",
  "GC=F" = "Gold",
  "CL=F" = "Oil"
)

cat("Starting data collection from Yahoo Finance...\n")
cat("Date range:", format(start_date), "to", format(end_date), "\n\n")

# Function to fetch data with error handling
fetch_data <- function(ticker, asset_name, start, end) {
  cat("Fetching", asset_name, "(", ticker, ")...\n")
  
  # Try to get data with retries
  max_retries <- 3
  retry_count <- 0
  data <- NULL
  
  while(retry_count < max_retries && is.null(data)) {
    tryCatch({
      # Use getSymbols to fetch data
      data <- getSymbols(
        ticker,
        src = "yahoo",
        from = start,
        to = end,
        auto.assign = FALSE,
        warnings = FALSE
      )
      
      # Convert to data frame
      df <- data.frame(
        Date = index(data),
        Open = as.numeric(Op(data)),
        High = as.numeric(Hi(data)),
        Low = as.numeric(Lo(data)),
        Close = as.numeric(Cl(data)),
        Volume = as.numeric(Vo(data)),
        Adj_Close = as.numeric(Ad(data))
      )
      
      # Remove rows with all NA values
      df <- df[!is.na(df$Adj_Close), ]
      
      cat("  Successfully fetched", nrow(df), "observations\n")
      return(df)
      
    }, error = function(e) {
      retry_count <<- retry_count + 1
      if(retry_count < max_retries) {
        cat("  Error occurred, retrying (", retry_count, "/", max_retries, ")...\n")
        Sys.sleep(2) # Wait 2 seconds before retry
      } else {
        cat("  Failed after", max_retries, "attempts. Error:", e$message, "\n")
        return(NULL)
      }
    })
  }
  
  return(NULL)
}

# Fetch data for each ticker
all_data <- list()

for(i in 1:length(tickers)) {
  ticker <- names(tickers)[i]
  asset_name <- tickers[i]
  
  data <- fetch_data(ticker, asset_name, start_date, end_date)
  
  if(!is.null(data)) {
    all_data[[asset_name]] <- data
    
    # Save raw data to CSV
    filename <- paste0("data/raw/", asset_name, "_raw.csv")
    write.csv(data, file = filename, row.names = FALSE)
    cat("  Saved to", filename, "\n\n")
  } else {
    cat("  Warning: Failed to fetch data for", asset_name, "\n\n")
  }
  
  # Small delay to avoid rate limiting
  Sys.sleep(1)
}

# Summary
cat("\n=== Data Collection Summary ===\n")
for(asset in names(all_data)) {
  cat(asset, ":", nrow(all_data[[asset]]), "observations\n")
}

cat("\nData collection complete!\n")
cat("Raw data files saved in data/raw/\n")

