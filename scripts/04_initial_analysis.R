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
# 5. Correlation Significance Tests
# ============================================================================
cat("\n=== Correlation Significance Tests ===\n")
cat("Testing H0: correlation = 0 vs Ha: correlation ≠ 0\n\n")

# Function to calculate rolling correlation with p-values
calc_rolling_corr_with_pvalue <- function(x, y, window = 90) {
  n <- length(x)
  corr <- rep(NA, n)
  pval <- rep(NA, n)
  
  for(i in window:n) {
    window_x <- x[(i-window+1):i]
    window_y <- y[(i-window+1):i]
    
    valid <- !is.na(window_x) & !is.na(window_y)
    if(sum(valid) >= 30) {
      tryCatch({
        test_result <- cor.test(window_x[valid], window_y[valid], 
                                alternative = "two.sided", method = "pearson")
        corr[i] <- test_result$estimate
        pval[i] <- test_result$p.value
      }, error = function(e) {
        # If test fails, just calculate correlation
        corr[i] <- cor(window_x[valid], window_y[valid], use = "complete.obs")
      })
    }
  }
  
  return(list(correlation = corr, pvalue = pval))
}

# Calculate rolling correlations with p-values for all pairs
cat("Calculating rolling correlations with significance tests (90-day window)...\n")

pairs <- list(
  list(name = "BTC_Gold", x = combined_data$BTC_Return, y = combined_data$Gold_Return),
  list(name = "BTC_Oil", x = combined_data$BTC_Return, y = combined_data$Oil_Return),
  list(name = "ETH_Gold", x = combined_data$ETH_Return, y = combined_data$Gold_Return),
  list(name = "ETH_Oil", x = combined_data$ETH_Return, y = combined_data$Oil_Return)
)

corr_significance_results <- data.frame(
  Pair = character(),
  Mean_Correlation = numeric(),
  Mean_PValue = numeric(),
  Significant_Periods = numeric(),
  Total_Periods = numeric(),
  Percent_Significant = numeric(),
  stringsAsFactors = FALSE
)

for(pair in pairs) {
  result <- calc_rolling_corr_with_pvalue(pair$x, pair$y, window = 90)
  
  # Store p-values
  combined_data[[paste0("Rolling_Corr_", pair$name, "_90d_PValue")]] <- result$pvalue
  
  # Calculate summary statistics
  valid_pvals <- !is.na(result$pvalue)
  significant <- sum(result$pvalue[valid_pvals] < 0.05, na.rm = TRUE)
  total <- sum(valid_pvals)
  
  corr_significance_results <- rbind(corr_significance_results, data.frame(
    Pair = pair$name,
    Mean_Correlation = mean(result$correlation[valid_pvals], na.rm = TRUE),
    Mean_PValue = mean(result$pvalue[valid_pvals], na.rm = TRUE),
    Significant_Periods = significant,
    Total_Periods = total,
    Percent_Significant = round(significant / total * 100, 2),
    stringsAsFactors = FALSE
  ))
  
  cat(pair$name, ":\n")
  cat("  Mean correlation:", round(mean(result$correlation[valid_pvals], na.rm = TRUE), 4), "\n")
  cat("  Mean p-value:", round(mean(result$pvalue[valid_pvals], na.rm = TRUE), 4), "\n")
  cat("  Significant periods (p < 0.05):", significant, "out of", total, 
      "(", round(significant / total * 100, 2), "%)\n\n")
}

write.csv(corr_significance_results, file = "output/correlation_significance_results.csv", row.names = FALSE)

# ============================================================================
# 6. Regime Identification for All Pairs
# ============================================================================
cat("\n=== Regime Identification for All Pairs ===\n")

# Threshold Justification:
# - High Positive (>0.3): Strong positive relationship, indicates coupling during risk-on periods
#   Based on financial literature, correlations >0.3 are considered moderate to strong
# - Moderate Positive (0.1-0.3): Weak to moderate positive relationship
# - Low (-0.1 to 0.1): Near-zero correlation, independent movement
# - Moderate Negative (-0.3 to -0.1): Weak to moderate negative relationship
# - High Negative (<-0.3): Strong negative relationship, indicates decoupling or hedging
# These thresholds are symmetric around zero and align with standard correlation interpretation
# in finance literature (e.g., Bodie, Kane, Marcus - Investments textbook)

cat("\nThreshold Justification:\n")
cat("- High Positive (>0.3): Strong positive relationship, risk-on coupling\n")
cat("- Moderate Positive (0.1-0.3): Weak to moderate positive relationship\n")
cat("- Low (-0.1 to 0.1): Near-zero correlation, independent movement\n")
cat("- Moderate Negative (-0.3 to -0.1): Weak to moderate negative relationship\n")
cat("- High Negative (<-0.3): Strong negative relationship, decoupling/hedging\n")
cat("Thresholds are symmetric and align with standard finance literature.\n\n")

# Define regime classification function
classify_regime <- function(corr) {
  case_when(
    corr > 0.3 ~ "High Positive",
    corr > 0.1 ~ "Moderate Positive",
    corr > -0.1 ~ "Low",
    corr > -0.3 ~ "Moderate Negative",
    TRUE ~ "High Negative"
  )
}

# Apply regime classification to all pairs
combined_data <- combined_data %>%
  mutate(
    BTC_Gold_Regime = classify_regime(Rolling_Corr_BTC_Gold_90d),
    BTC_Oil_Regime = classify_regime(Rolling_Corr_BTC_Oil_90d),
    ETH_Gold_Regime = classify_regime(Rolling_Corr_ETH_Gold_90d),
    ETH_Oil_Regime = classify_regime(Rolling_Corr_ETH_Oil_90d)
  )

# Create comprehensive regime summary for all pairs
regime_summary <- data.frame(
  Pair = character(),
  Regime = character(),
  Count = numeric(),
  Percentage = numeric(),
  Mean_Correlation = numeric(),
  stringsAsFactors = FALSE
)

pairs_list <- c("BTC_Gold", "BTC_Oil", "ETH_Gold", "ETH_Oil")

for(pair_name in pairs_list) {
  # Get the regime and correlation columns
  regime_col <- paste0(pair_name, "_Regime")
  corr_col <- paste0("Rolling_Corr_", pair_name, "_90d")
  
  # Create temporary dataframe for this pair
  temp_data <- combined_data %>%
    select(Date, Regime = all_of(regime_col), Correlation = all_of(corr_col)) %>%
    filter(!is.na(Regime))
  
  pair_regime <- temp_data %>%
    group_by(Regime) %>%
    summarise(
      Count = n(),
      Mean_Correlation = mean(Correlation, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      Percentage = round(Count / sum(Count) * 100, 2),
      Pair = pair_name
    ) %>%
    select(Pair, Regime, Count, Percentage, Mean_Correlation)
  
  regime_summary <- rbind(regime_summary, pair_regime)
  
  cat("\n", pair_name, "Correlation Regimes:\n")
  print(pair_regime)
}

write.csv(regime_summary, file = "output/regime_summary_all_pairs.csv", row.names = FALSE)

# Identify periods of high correlation (potential risk-on periods) for all pairs
high_corr_periods_all <- combined_data %>%
  filter(Rolling_Corr_BTC_Gold_90d > 0.3 | Rolling_Corr_BTC_Oil_90d > 0.3 |
         Rolling_Corr_ETH_Gold_90d > 0.3 | Rolling_Corr_ETH_Oil_90d > 0.3) %>%
  select(Date, Rolling_Corr_BTC_Gold_90d, Rolling_Corr_BTC_Oil_90d,
         Rolling_Corr_ETH_Gold_90d, Rolling_Corr_ETH_Oil_90d) %>%
  arrange(Date)

cat("\n\nPeriods with high positive correlation (>0.3) for any pair:\n")
cat("Total periods:", nrow(high_corr_periods_all), "\n")
if(nrow(high_corr_periods_all) > 0) {
  cat("Date range:", format(min(high_corr_periods_all$Date)), "to", 
      format(max(high_corr_periods_all$Date)), "\n")
}

# ============================================================================
# 7. Visualization: Multiple Window Comparison
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
cat("4. Correlation significance tests completed (H0: cor=0) with p-values\n")
cat("5. Regime identification completed for all pairs (BTC-Gold, BTC-Oil, ETH-Gold, ETH-Oil)\n")
cat("6. Threshold justification documented\n")
cat("7. Summary statistics generated\n\n")

cat("Initial analysis complete! Results saved to output/\n")

