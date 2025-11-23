# Initial Analysis Script
# FA-582 Group 7: Cryptocurrency-Commodity Correlation Analysis
# This script performs rolling correlation analysis and basic statistical tests

# Load required libraries
library(dplyr)
library(tidyr)
library(zoo)
library(ggplot2)
library(tseries)  # For ADF tests
library(lubridate)

cat("Starting Initial Analysis...\n\n")

# Ensure output directory exists
if (!dir.exists("output")) {
  dir.create("output", recursive = TRUE)
}
if (!dir.exists("output/figures")) {
  dir.create("output/figures", recursive = TRUE)
}

# Load processed data
combined_data <- read.csv("data/processed/combined_data.csv", stringsAsFactors = FALSE)
combined_data$Date <- as.Date(combined_data$Date)

cat("Loaded processed data:", nrow(combined_data), "observations\n\n")

# ============================================================================
# 1. Rolling Correlation Analysis for Multiple Windows
# ============================================================================
cat("Calculating rolling correlations for multiple windows...\n")

# Function to calculate rolling correlation for different windows
# Note: This is similar to calc_rolling_corr in preprocessing but uses adaptive threshold
calc_rolling_corr_window <- function(x, y, window) {
  n <- length(x)
  corr <- rep(NA, n)
  
  for(i in window:n) {
    window_x <- x[(i-window+1):i]
    window_y <- y[(i-window+1):i]
    
    valid <- !is.na(window_x) & !is.na(window_y)
    # Use adaptive threshold: require at least 30 observations or 1/3 of window
    if(sum(valid) >= min(30, window/3)) {
      corr[i] <- cor(window_x[valid], window_y[valid], use = "complete.obs")
    }
  }
  
  return(corr)
}

# Calculate rolling correlations for different windows
windows <- c(30, 60, 90, 180)

for(w in windows) {
  cat("  Calculating", w, "day rolling correlations...\n")
  
  combined_data[[paste0("Rolling_Corr_BTC_Gold_", w, "d")]] <- 
    calc_rolling_corr_window(combined_data$BTC_Return, combined_data$Gold_Return, w)
  
  combined_data[[paste0("Rolling_Corr_BTC_Oil_", w, "d")]] <- 
    calc_rolling_corr_window(combined_data$BTC_Return, combined_data$Oil_Return, w)
  
  combined_data[[paste0("Rolling_Corr_ETH_Gold_", w, "d")]] <- 
    calc_rolling_corr_window(combined_data$ETH_Return, combined_data$Gold_Return, w)
  
  combined_data[[paste0("Rolling_Corr_ETH_Oil_", w, "d")]] <- 
    calc_rolling_corr_window(combined_data$ETH_Return, combined_data$Oil_Return, w)
}

cat("Rolling correlations calculated for all windows\n\n")

# Save updated data
write.csv(combined_data, file = "data/processed/combined_data.csv", row.names = FALSE)

# ============================================================================
# 2. Alternative Correlation Measures (Robustness Checks)
# ============================================================================
cat("Calculating alternative correlation measures (Spearman, Kendall)...\n")

# Spearman correlation (rank-based, robust to outliers)
calc_spearman_corr <- function(x, y, window = 90) {
  n <- length(x)
  corr <- rep(NA, n)
  
  for(i in window:n) {
    window_x <- x[(i-window+1):i]
    window_y <- y[(i-window+1):i]
    
    valid <- !is.na(window_x) & !is.na(window_y)
    if(sum(valid) >= 30) {
      corr[i] <- cor(window_x[valid], window_y[valid], method = "spearman", use = "complete.obs")
    }
  }
  
  return(corr)
}

combined_data$Rolling_Corr_BTC_Gold_Spearman <- 
  calc_spearman_corr(combined_data$BTC_Return, combined_data$Gold_Return, 90)

combined_data$Rolling_Corr_BTC_Oil_Spearman <- 
  calc_spearman_corr(combined_data$BTC_Return, combined_data$Oil_Return, 90)

cat("Alternative correlation measures calculated\n\n")

# ============================================================================
# 3. Statistical Tests
# ============================================================================
cat("Performing statistical tests...\n\n")

# Stationarity Tests (Augmented Dickey-Fuller)
cat("=== Stationarity Tests (Augmented Dickey-Fuller) ===\n")

returns_data <- combined_data %>%
  select(BTC_Return, ETH_Return, Gold_Return, Oil_Return) %>%
  filter(complete.cases(.))

adf_results <- data.frame(
  Asset = character(),
  Test_Statistic = numeric(),
  P_Value = character(),
  Is_Stationary = character(),
  stringsAsFactors = FALSE
)

for(asset in c("BTC_Return", "ETH_Return", "Gold_Return", "Oil_Return")) {
  returns <- returns_data[[asset]]
  returns <- returns[!is.na(returns)]
  
  tryCatch({
    # Suppress warnings about p-value being smaller than printed
    adf_test <- suppressWarnings(adf.test(returns))
    
    # Extract p-value, handling very small p-values
    p_val <- adf_test$p.value
    if(p_val < 0.001) {
      p_val_display <- "< 0.001"
    } else {
      p_val_display <- round(p_val, 4)
    }
    
    adf_results <- rbind(adf_results, data.frame(
      Asset = asset,
      Test_Statistic = round(adf_test$statistic, 4),
      P_Value = ifelse(p_val < 0.001, "< 0.001", round(p_val, 4)),
      Is_Stationary = ifelse(p_val < 0.05, "Yes", "No"),
      stringsAsFactors = FALSE
    ))
    
    cat(asset, ":\n")
    cat("  ADF Statistic:", round(adf_test$statistic, 4), "\n")
    cat("  P-value:", p_val_display, "\n")
    cat("  Stationary:", ifelse(p_val < 0.05, "Yes", "No"), "\n\n")
    
  }, error = function(e) {
    cat("  Error testing", asset, ":", e$message, "\n\n")
  })
}

write.csv(adf_results, file = "output/adf_test_results.csv", row.names = FALSE)

# ============================================================================
# 4. Correlation Summary Statistics
# ============================================================================
cat("=== Rolling Correlation Summary Statistics ===\n")

corr_summary <- combined_data %>%
  summarise(
    BTC_Gold_Mean = mean(Rolling_Corr_BTC_Gold_90d, na.rm = TRUE),
    BTC_Gold_SD = sd(Rolling_Corr_BTC_Gold_90d, na.rm = TRUE),
    BTC_Gold_Min = min(Rolling_Corr_BTC_Gold_90d, na.rm = TRUE),
    BTC_Gold_Max = max(Rolling_Corr_BTC_Gold_90d, na.rm = TRUE),
    
    BTC_Oil_Mean = mean(Rolling_Corr_BTC_Oil_90d, na.rm = TRUE),
    BTC_Oil_SD = sd(Rolling_Corr_BTC_Oil_90d, na.rm = TRUE),
    BTC_Oil_Min = min(Rolling_Corr_BTC_Oil_90d, na.rm = TRUE),
    BTC_Oil_Max = max(Rolling_Corr_BTC_Oil_90d, na.rm = TRUE),
    
    ETH_Gold_Mean = mean(Rolling_Corr_ETH_Gold_90d, na.rm = TRUE),
    ETH_Gold_SD = sd(Rolling_Corr_ETH_Gold_90d, na.rm = TRUE),
    ETH_Gold_Min = min(Rolling_Corr_ETH_Gold_90d, na.rm = TRUE),
    ETH_Gold_Max = max(Rolling_Corr_ETH_Gold_90d, na.rm = TRUE),
    
    ETH_Oil_Mean = mean(Rolling_Corr_ETH_Oil_90d, na.rm = TRUE),
    ETH_Oil_SD = sd(Rolling_Corr_ETH_Oil_90d, na.rm = TRUE),
    ETH_Oil_Min = min(Rolling_Corr_ETH_Oil_90d, na.rm = TRUE),
    ETH_Oil_Max = max(Rolling_Corr_ETH_Oil_90d, na.rm = TRUE)
  )

print(round(corr_summary, 3))
write.csv(corr_summary, file = "output/rolling_correlation_summary.csv", row.names = FALSE)

# ============================================================================
# 5. Preliminary Regime Identification
# ============================================================================
cat("\n=== Preliminary Regime Identification ===\n")

# Simple threshold-based classification
# Define regimes based on correlation levels
combined_data <- combined_data %>%
  mutate(
    BTC_Gold_Regime = case_when(
      Rolling_Corr_BTC_Gold_90d > 0.3 ~ "High Positive",
      Rolling_Corr_BTC_Gold_90d > 0.1 ~ "Moderate Positive",
      Rolling_Corr_BTC_Gold_90d > -0.1 ~ "Low",
      Rolling_Corr_BTC_Gold_90d > -0.3 ~ "Moderate Negative",
      TRUE ~ "High Negative"
    ),
    BTC_Oil_Regime = case_when(
      Rolling_Corr_BTC_Oil_90d > 0.3 ~ "High Positive",
      Rolling_Corr_BTC_Oil_90d > 0.1 ~ "Moderate Positive",
      Rolling_Corr_BTC_Oil_90d > -0.1 ~ "Low",
      Rolling_Corr_BTC_Oil_90d > -0.3 ~ "Moderate Negative",
      TRUE ~ "High Negative"
    )
  )

# Count observations in each regime
regime_counts <- combined_data %>%
  filter(!is.na(BTC_Gold_Regime)) %>%
  count(BTC_Gold_Regime, name = "Count") %>%
  mutate(Percentage = round(Count / sum(Count) * 100, 2))

cat("\nBTC-Gold Correlation Regimes:\n")
print(regime_counts)

# Identify periods of high correlation (potential risk-on periods)
high_corr_periods <- combined_data %>%
  filter(Rolling_Corr_BTC_Gold_90d > 0.3 | Rolling_Corr_BTC_Oil_90d > 0.3) %>%
  select(Date, Rolling_Corr_BTC_Gold_90d, Rolling_Corr_BTC_Oil_90d) %>%
  arrange(Date)

cat("\nPeriods with high positive correlation (>0.3):\n")
cat("Total periods:", nrow(high_corr_periods), "\n")
if(nrow(high_corr_periods) > 0) {
  cat("Date range:", format(min(high_corr_periods$Date)), "to", format(max(high_corr_periods$Date)), "\n")
}

# ============================================================================
# 6. Visualization: Multiple Window Comparison
# ============================================================================
cat("\nCreating visualization for multiple window comparison...\n")

# Create comparison plot for BTC-Gold correlation across windows
corr_comparison <- combined_data %>%
  select(Date, 
         `30-day` = Rolling_Corr_BTC_Gold_30d,
         `60-day` = Rolling_Corr_BTC_Gold_60d,
         `90-day` = Rolling_Corr_BTC_Gold_90d,
         `180-day` = Rolling_Corr_BTC_Gold_180d) %>%
  pivot_longer(cols = -Date, names_to = "Window", values_to = "Correlation") %>%
  filter(!is.na(Correlation))

fig_multi_window <- ggplot(corr_comparison, aes(x = Date, y = Correlation, color = Window)) +
  geom_line(linewidth = 0.7, alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  labs(
    title = "Rolling Correlation: Bitcoin-Gold Across Different Windows",
    subtitle = "Comparison of 30, 60, 90, and 180-day rolling correlations",
    x = "Date",
    y = "Correlation",
    color = "Window Size"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(size = 12, face = "bold")) +
  ylim(-0.5, 0.7)

ggsave("output/figures/Figure_Multi_Window_Correlation.png", fig_multi_window, 
       width = 12, height = 6, dpi = 300)

cat("Visualization saved\n")

# Save updated data with regimes
write.csv(combined_data, file = "data/processed/combined_data.csv", row.names = FALSE)

cat("\n=== Analysis Summary ===\n")
cat("1. Rolling correlations calculated for windows: 30, 60, 90, 180 days\n")
cat("2. Alternative correlation measures (Spearman) calculated\n")
cat("3. Stationarity tests performed (ADF)\n")
cat("4. Preliminary regime identification completed\n")
cat("5. Summary statistics generated\n\n")

cat("Initial analysis complete! Results saved to output/\n")

