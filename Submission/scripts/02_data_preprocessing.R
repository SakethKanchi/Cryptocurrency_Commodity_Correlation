# Data Preprocessing Script
# FA-582 Group 7: Cryptocurrency-Commodity Correlation Analysis
# This script cleans data, calculates returns, and creates derived features

# Load required libraries
library(dplyr)
library(tidyr)
library(zoo)
library(xts)
library(lubridate)

cat("Starting data preprocessing...\n\n")

# Function to load raw data
load_raw_data <- function(asset_name) {
  filename <- paste0("data/raw/", asset_name, "_raw.csv")
  if(file.exists(filename)) {
    data <- read.csv(filename, stringsAsFactors = FALSE)
    data$Date <- as.Date(data$Date)
    return(data)
  } else {
    stop(paste("File not found:", filename))
  }
}

# Load all raw data
cat("Loading raw data files...\n")
btc_raw <- load_raw_data("Bitcoin")
eth_raw <- load_raw_data("Ethereum")
gold_raw <- load_raw_data("Gold")
oil_raw <- load_raw_data("Oil")

cat("Bitcoin:", nrow(btc_raw), "observations\n")
cat("Ethereum:", nrow(eth_raw), "observations\n")
cat("Gold:", nrow(gold_raw), "observations\n")
cat("Oil:", nrow(oil_raw), "observations\n\n")

# Function to preprocess data
preprocess_asset <- function(df, asset_name, is_crypto = FALSE) {
  cat("Preprocessing", asset_name, "...\n")
  
  # Use adjusted close price
  df$Price <- df$Adj_Close
  
  # Handle missing values - forward fill
  df$Price <- na.locf(df$Price, na.rm = FALSE)
  
  # Remove any remaining NA rows
  df <- df[!is.na(df$Price), ]
  
  # Remove zero or negative prices (invalid for log transformation)
  invalid_prices <- sum(df$Price <= 0, na.rm = TRUE)
  if(invalid_prices > 0) {
    cat("  Warning: Removing", invalid_prices, "rows with zero or negative prices\n")
    df <- df[df$Price > 0, ]
  }
  
  # Sort by date
  df <- df %>% arrange(Date)
  
  # Calculate log returns: ln(Price_t / Price_t-1)
  # Only calculate if we have valid prices
  if(nrow(df) > 0 && all(df$Price > 0, na.rm = TRUE)) {
    df$Return <- c(NA, diff(log(df$Price)))
  } else {
    stop(paste("Error: No valid prices found for", asset_name))
  }
  
  # Calculate normalized price (base = 100 at first date)
  first_price <- df$Price[1]
  df$Normalized_Price <- (df$Price / first_price) * 100
  
  # Calculate 30-day rolling volatility (standard deviation of returns)
  df$Volatility_30d <- rollapply(df$Return, width = 30, FUN = sd, 
                                   fill = NA, align = "right", na.rm = TRUE)
  
  cat("  Processed", nrow(df), "observations\n")
  cat("  Date range:", format(min(df$Date)), "to", format(max(df$Date)), "\n")
  cat("  Missing values handled\n")
  
  return(df)
}

# Preprocess each asset
btc <- preprocess_asset(btc_raw, "Bitcoin", is_crypto = TRUE)
eth <- preprocess_asset(eth_raw, "Ethereum", is_crypto = TRUE)
gold <- preprocess_asset(gold_raw, "Gold", is_crypto = FALSE)
oil <- preprocess_asset(oil_raw, "Oil", is_crypto = FALSE)

# Align trading calendars
# Cryptocurrencies trade 24/7, commodities trade weekdays only
# We'll merge on common dates
cat("\nAligning trading calendars...\n")

# Create a common date sequence
all_dates <- sort(unique(c(btc$Date, eth$Date, gold$Date, oil$Date)))

# Merge all data on common dates
combined_data <- data.frame(Date = all_dates) %>%
  left_join(btc %>% select(Date, BTC_Price = Price, BTC_Return = Return, 
                           BTC_Normalized = Normalized_Price, BTC_Volatility = Volatility_30d),
            by = "Date") %>%
  left_join(eth %>% select(Date, ETH_Price = Price, ETH_Return = Return,
                           ETH_Normalized = Normalized_Price, ETH_Volatility = Volatility_30d),
            by = "Date") %>%
  left_join(gold %>% select(Date, Gold_Price = Price, Gold_Return = Return,
                            Gold_Normalized = Normalized_Price, Gold_Volatility = Volatility_30d),
            by = "Date") %>%
  left_join(oil %>% select(Date, Oil_Price = Price, Oil_Return = Return,
                           Oil_Normalized = Normalized_Price, Oil_Volatility = Volatility_30d),
            by = "Date")

# Forward fill missing values for commodities (weekends)
combined_data <- combined_data %>%
  arrange(Date) %>%
  mutate(
    Gold_Price = na.locf(Gold_Price, na.rm = FALSE),
    Gold_Return = ifelse(is.na(Gold_Return), 0, Gold_Return),
    Gold_Normalized = na.locf(Gold_Normalized, na.rm = FALSE),
    Gold_Volatility = na.locf(Gold_Volatility, na.rm = FALSE),
    Oil_Price = na.locf(Oil_Price, na.rm = FALSE),
    Oil_Return = ifelse(is.na(Oil_Return), 0, Oil_Return),
    Oil_Normalized = na.locf(Oil_Normalized, na.rm = FALSE),
    Oil_Volatility = na.locf(Oil_Volatility, na.rm = FALSE)
  )

# Remove rows where we don't have at least crypto data
combined_data <- combined_data %>%
  filter(!is.na(BTC_Return) & !is.na(ETH_Return))

cat("Aligned dataset:", nrow(combined_data), "observations\n")
cat("Date range:", format(min(combined_data$Date)), "to", format(max(combined_data$Date)), "\n\n")

# Calculate rolling correlations (90-day window)
cat("Calculating rolling correlations...\n")

# Function to calculate rolling correlation
# This function is used for initial 90-day correlations in preprocessing
calc_rolling_corr <- function(x, y, window = 90) {
  n <- length(x)
  corr <- rep(NA, n)
  
  for(i in window:n) {
    window_x <- x[(i-window+1):i]
    window_y <- y[(i-window+1):i]
    
    # Only calculate if we have enough non-NA values
    valid <- !is.na(window_x) & !is.na(window_y)
    if(sum(valid) >= 30) {  # Require at least 30 valid observations
      corr[i] <- cor(window_x[valid], window_y[valid], use = "complete.obs")
    }
  }
  
  return(corr)
}

# Calculate rolling correlations for all pairs
combined_data$Rolling_Corr_BTC_Gold_90d <- calc_rolling_corr(
  combined_data$BTC_Return, combined_data$Gold_Return, window = 90
)

combined_data$Rolling_Corr_BTC_Oil_90d <- calc_rolling_corr(
  combined_data$BTC_Return, combined_data$Oil_Return, window = 90
)

combined_data$Rolling_Corr_ETH_Gold_90d <- calc_rolling_corr(
  combined_data$ETH_Return, combined_data$Gold_Return, window = 90
)

combined_data$Rolling_Corr_ETH_Oil_90d <- calc_rolling_corr(
  combined_data$ETH_Return, combined_data$Oil_Return, window = 90
)

combined_data$Rolling_Corr_BTC_ETH_90d <- calc_rolling_corr(
  combined_data$BTC_Return, combined_data$ETH_Return, window = 90
)

cat("Rolling correlations calculated\n\n")

# Save processed data
cat("Saving processed data...\n")

# Save individual processed assets
write.csv(btc %>% select(Date, Price, Return, Normalized_Price, Volatility_30d),
          file = "data/processed/Bitcoin_processed.csv", row.names = FALSE)
write.csv(eth %>% select(Date, Price, Return, Normalized_Price, Volatility_30d),
          file = "data/processed/Ethereum_processed.csv", row.names = FALSE)
write.csv(gold %>% select(Date, Price, Return, Normalized_Price, Volatility_30d),
          file = "data/processed/Gold_processed.csv", row.names = FALSE)
write.csv(oil %>% select(Date, Price, Return, Normalized_Price, Volatility_30d),
          file = "data/processed/Oil_processed.csv", row.names = FALSE)

# Save combined dataset
write.csv(combined_data, file = "data/processed/combined_data.csv", row.names = FALSE)

cat("Processed data saved to data/processed/\n")
cat("\n=== Preprocessing Summary ===\n")
cat("Final dataset:", nrow(combined_data), "observations\n")
cat("Features created:\n")
cat("  - Log returns for all assets\n")
cat("  - Normalized prices (base = 100)\n")
cat("  - 30-day rolling volatility\n")
cat("  - 90-day rolling correlations for all pairs\n")
cat("\nPreprocessing complete!\n")

