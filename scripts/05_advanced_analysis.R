# Advanced Analysis Script
# FA-582 Group 7: Additional analyses for final report
# This script performs statistical comparisons, event studies, and robustness checks

library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

cat("Starting Advanced Analysis...\n\n")

# Load processed data
combined_data <- read.csv("data/processed/combined_data.csv", stringsAsFactors = FALSE)
combined_data$Date <- as.Date(combined_data$Date)

# ============================================================================
# 1. BTC vs ETH Statistical Comparison
# ============================================================================
cat("Comparing BTC vs ETH correlation patterns...\n")

# Extract correlation data for comparison
btc_gold_corr <- combined_data$Rolling_Corr_BTC_Gold_90d[!is.na(combined_data$Rolling_Corr_BTC_Gold_90d)]
eth_gold_corr <- combined_data$Rolling_Corr_ETH_Gold_90d[!is.na(combined_data$Rolling_Corr_ETH_Gold_90d)]
btc_oil_corr <- combined_data$Rolling_Corr_BTC_Oil_90d[!is.na(combined_data$Rolling_Corr_BTC_Oil_90d)]
eth_oil_corr <- combined_data$Rolling_Corr_ETH_Oil_90d[!is.na(combined_data$Rolling_Corr_ETH_Oil_90d)]

# Statistical tests
btc_eth_gold_test <- t.test(btc_gold_corr, eth_gold_corr, paired = FALSE)
btc_eth_oil_test <- t.test(btc_oil_corr, eth_oil_corr, paired = FALSE)

# Mann-Whitney U test (non-parametric alternative)
btc_eth_gold_mw <- wilcox.test(btc_gold_corr, eth_gold_corr, paired = FALSE)
btc_eth_oil_mw <- wilcox.test(btc_oil_corr, eth_oil_corr, paired = FALSE)

# Create comparison summary
btc_eth_comparison <- data.frame(
  Pair = c("BTC-Gold vs ETH-Gold", "BTC-Oil vs ETH-Oil"),
  BTC_Mean = c(mean(btc_gold_corr, na.rm = TRUE), mean(btc_oil_corr, na.rm = TRUE)),
  ETH_Mean = c(mean(eth_gold_corr, na.rm = TRUE), mean(eth_oil_corr, na.rm = TRUE)),
  BTC_Std = c(sd(btc_gold_corr, na.rm = TRUE), sd(btc_oil_corr, na.rm = TRUE)),
  ETH_Std = c(sd(eth_gold_corr, na.rm = TRUE), sd(eth_oil_corr, na.rm = TRUE)),
  T_Statistic = c(btc_eth_gold_test$statistic, btc_eth_oil_test$statistic),
  T_PValue = c(btc_eth_gold_test$p.value, btc_eth_oil_test$p.value),
  MW_Statistic = c(btc_eth_gold_mw$statistic, btc_eth_oil_mw$statistic),
  MW_PValue = c(btc_eth_gold_mw$p.value, btc_eth_oil_mw$p.value),
  Significant = c(btc_eth_gold_test$p.value < 0.05, btc_eth_oil_test$p.value < 0.05)
)

write.csv(btc_eth_comparison, file = "output/btc_eth_comparison.csv", row.names = FALSE)
cat("BTC vs ETH comparison saved\n\n")

# ============================================================================
# 2. Spearman vs Pearson Correlation Comparison
# ============================================================================
cat("Comparing Spearman vs Pearson correlations...\n")

# Calculate comparison statistics
spearman_comparison <- data.frame(
  Pair = character(),
  Pearson_Mean = numeric(),
  Spearman_Mean = numeric(),
  Pearson_Std = numeric(),
  Spearman_Std = numeric(),
  Correlation = numeric(),
  stringsAsFactors = FALSE
)

pairs_list <- list(
  list(name = "BTC_Gold", pearson = "Rolling_Corr_BTC_Gold_90d", spearman = "Rolling_Corr_BTC_Gold_Spearman"),
  list(name = "BTC_Oil", pearson = "Rolling_Corr_BTC_Oil_90d", spearman = "Rolling_Corr_BTC_Oil_Spearman")
)

for(p in pairs_list) {
  pearson_vals <- combined_data[[p$pearson]][!is.na(combined_data[[p$pearson]]) & !is.na(combined_data[[p$spearman]])]
  spearman_vals <- combined_data[[p$spearman]][!is.na(combined_data[[p$pearson]]) & !is.na(combined_data[[p$spearman]])]
  
  if(length(pearson_vals) > 0 && length(spearman_vals) > 0) {
    corr_between <- cor(pearson_vals, spearman_vals, use = "complete.obs")
    
    spearman_comparison <- rbind(spearman_comparison, data.frame(
      Pair = p$name,
      Pearson_Mean = mean(pearson_vals, na.rm = TRUE),
      Spearman_Mean = mean(spearman_vals, na.rm = TRUE),
      Pearson_Std = sd(pearson_vals, na.rm = TRUE),
      Spearman_Std = sd(spearman_vals, na.rm = TRUE),
      Correlation = corr_between
    ))
  }
}

write.csv(spearman_comparison, file = "output/spearman_pearson_comparison.csv", row.names = FALSE)
cat("Spearman vs Pearson comparison saved\n\n")

# ============================================================================
# 3. Window Size Sensitivity Analysis
# ============================================================================
cat("Analyzing sensitivity to window size...\n")

windows <- c(30, 60, 90, 180)
window_sensitivity <- data.frame(
  Pair = character(),
  Window = numeric(),
  Mean_Correlation = numeric(),
  Std_Correlation = numeric(),
  Min_Correlation = numeric(),
  Max_Correlation = numeric(),
  Range = numeric(),
  stringsAsFactors = FALSE
)

pairs_for_windows <- c("BTC_Gold", "BTC_Oil", "ETH_Gold", "ETH_Oil")

for(pair in pairs_for_windows) {
  for(w in windows) {
    col_name <- paste0("Rolling_Corr_", pair, "_", w, "d")
    if(col_name %in% names(combined_data)) {
      corr_vals <- combined_data[[col_name]][!is.na(combined_data[[col_name]])]
      if(length(corr_vals) > 0) {
        window_sensitivity <- rbind(window_sensitivity, data.frame(
          Pair = pair,
          Window = w,
          Mean_Correlation = mean(corr_vals, na.rm = TRUE),
          Std_Correlation = sd(corr_vals, na.rm = TRUE),
          Min_Correlation = min(corr_vals, na.rm = TRUE),
          Max_Correlation = max(corr_vals, na.rm = TRUE),
          Range = max(corr_vals, na.rm = TRUE) - min(corr_vals, na.rm = TRUE)
        ))
      }
    }
  }
}

write.csv(window_sensitivity, file = "output/window_sensitivity_analysis.csv", row.names = FALSE)
cat("Window sensitivity analysis saved\n\n")

# ============================================================================
# 4. Event Study Analysis
# ============================================================================
cat("Conducting event study analysis...\n")

# Define events
events <- data.frame(
  Event = c("COVID-19", "Fed_Tightening", "FTX_Collapse", "Banking_Crisis"),
  Date = as.Date(c("2020-03-15", "2022-03-16", "2022-11-11", "2023-03-10")),
  stringsAsFactors = FALSE
)

event_window <- 30  # ±30 days

event_study_results <- data.frame(
  Event = character(),
  Pair = character(),
  Pre_Event_Mean = numeric(),
  Post_Event_Mean = numeric(),
  Pre_Event_Std = numeric(),
  Post_Event_Std = numeric(),
  Change = numeric(),
  T_Statistic = numeric(),
  P_Value = numeric(),
  Significant = logical(),
  stringsAsFactors = FALSE
)

pairs_for_events <- c("BTC_Gold", "BTC_Oil", "ETH_Gold", "ETH_Oil")

for(event_idx in 1:nrow(events)) {
  event_date <- events$Date[event_idx]
  event_name <- events$Event[event_idx]
  
  # Find closest date in data
  date_idx <- which.min(abs(combined_data$Date - event_date))
  if(length(date_idx) == 0) next
  
  for(pair in pairs_for_events) {
    col_name <- paste0("Rolling_Corr_", pair, "_90d")
    if(!col_name %in% names(combined_data)) next
    
    # Pre-event period (30 days before)
    pre_start <- max(1, date_idx - event_window)
    pre_end <- date_idx - 1
    pre_values <- combined_data[[col_name]][pre_start:pre_end]
    pre_values <- pre_values[!is.na(pre_values)]
    
    # Post-event period (30 days after)
    post_start <- date_idx + 1
    post_end <- min(nrow(combined_data), date_idx + event_window)
    post_values <- combined_data[[col_name]][post_start:post_end]
    post_values <- post_values[!is.na(post_values)]
    
    if(length(pre_values) >= 10 && length(post_values) >= 10) {
      # Statistical test
      test_result <- t.test(pre_values, post_values, paired = FALSE)
      
      event_study_results <- rbind(event_study_results, data.frame(
        Event = event_name,
        Pair = pair,
        Pre_Event_Mean = mean(pre_values, na.rm = TRUE),
        Post_Event_Mean = mean(post_values, na.rm = TRUE),
        Pre_Event_Std = sd(pre_values, na.rm = TRUE),
        Post_Event_Std = sd(post_values, na.rm = TRUE),
        Change = mean(post_values, na.rm = TRUE) - mean(pre_values, na.rm = TRUE),
        T_Statistic = test_result$statistic,
        P_Value = test_result$p.value,
        Significant = test_result$p.value < 0.05
      ))
    }
  }
}

write.csv(event_study_results, file = "output/event_study_results.csv", row.names = FALSE)
cat("Event study results saved\n\n")

# ============================================================================
# 5. Regime Transition Analysis
# ============================================================================
cat("Analyzing regime transitions...\n")

# Function to identify regime transitions
identify_regime_transitions <- function(corr_series, thresholds = c(-0.3, -0.1, 0.1, 0.3)) {
  regimes <- case_when(
    corr_series > 0.3 ~ "High Positive",
    corr_series > 0.1 ~ "Moderate Positive",
    corr_series > -0.1 ~ "Low",
    corr_series > -0.3 ~ "Moderate Negative",
    TRUE ~ "High Negative"
  )
  
  transitions <- c()
  for(i in 2:length(regimes)) {
    if(!is.na(regimes[i]) && !is.na(regimes[i-1]) && regimes[i] != regimes[i-1]) {
      transitions <- c(transitions, i)
    }
  }
  
  return(list(regimes = regimes, transitions = transitions))
}

# Analyze transitions for each pair
regime_transitions_summary <- data.frame(
  Pair = character(),
  Total_Transitions = numeric(),
  Avg_Days_Between = numeric(),
  Most_Common_Transition = character(),
  stringsAsFactors = FALSE
)

for(pair in pairs_for_events) {
  col_name <- paste0("Rolling_Corr_", pair, "_90d")
  if(!col_name %in% names(combined_data)) next
  
  corr_series <- combined_data[[col_name]]
  transition_data <- identify_regime_transitions(corr_series)
  
  if(length(transition_data$transitions) > 0) {
    avg_days <- mean(diff(transition_data$transitions), na.rm = TRUE)
    
    # Find most common transition type
    transition_types <- c()
    for(i in transition_data$transitions) {
      if(i > 1 && !is.na(transition_data$regimes[i]) && !is.na(transition_data$regimes[i-1])) {
        transition_types <- c(transition_types, 
                             paste(transition_data$regimes[i-1], "->", transition_data$regimes[i]))
      }
    }
    
    most_common <- names(sort(table(transition_types), decreasing = TRUE))[1]
    if(is.null(most_common)) most_common <- "N/A"
    
    regime_transitions_summary <- rbind(regime_transitions_summary, data.frame(
      Pair = pair,
      Total_Transitions = length(transition_data$transitions),
      Avg_Days_Between = round(avg_days, 1),
      Most_Common_Transition = most_common
    ))
  }
}

write.csv(regime_transitions_summary, file = "output/regime_transitions_summary.csv", row.names = FALSE)
cat("Regime transition analysis saved\n\n")

cat("Advanced analysis complete!\n")
cat("Output files:\n")
cat("  - output/btc_eth_comparison.csv\n")
cat("  - output/spearman_pearson_comparison.csv\n")
cat("  - output/window_sensitivity_analysis.csv\n")
cat("  - output/event_study_results.csv\n")
cat("  - output/regime_transitions_summary.csv\n")

