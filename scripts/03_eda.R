# Exploratory Data Analysis Script
# FA-582 Group 7: Cryptocurrency-Commodity Correlation Analysis
# This script performs EDA and generates visualizations

# Load required libraries
library(dplyr)
library(ggplot2)
library(PerformanceAnalytics)
library(corrplot)
library(gridExtra)
library(scales)
library(lubridate)

cat("Starting Exploratory Data Analysis...\n\n")

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

cat("Loaded processed data:", nrow(combined_data), "observations\n")
cat("Date range:", format(min(combined_data$Date)), "to", format(max(combined_data$Date)), "\n\n")

# ============================================================================
# 1. Summary Statistics
# ============================================================================
cat("Calculating summary statistics...\n")

# Create summary statistics table
returns_data <- combined_data %>%
  select(BTC_Return, ETH_Return, Gold_Return, Oil_Return) %>%
  filter(complete.cases(.))

summary_stats <- data.frame(
  Asset = c("Bitcoin", "Ethereum", "Gold", "Crude Oil"),
  Mean_Return = c(
    mean(returns_data$BTC_Return, na.rm = TRUE) * 100,
    mean(returns_data$ETH_Return, na.rm = TRUE) * 100,
    mean(returns_data$Gold_Return, na.rm = TRUE) * 100,
    mean(returns_data$Oil_Return, na.rm = TRUE) * 100
  ),
  Std_Dev = c(
    sd(returns_data$BTC_Return, na.rm = TRUE) * 100,
    sd(returns_data$ETH_Return, na.rm = TRUE) * 100,
    sd(returns_data$Gold_Return, na.rm = TRUE) * 100,
    sd(returns_data$Oil_Return, na.rm = TRUE) * 100
  ),
  Skewness = c(
    PerformanceAnalytics::skewness(returns_data$BTC_Return, na.rm = TRUE),
    PerformanceAnalytics::skewness(returns_data$ETH_Return, na.rm = TRUE),
    PerformanceAnalytics::skewness(returns_data$Gold_Return, na.rm = TRUE),
    PerformanceAnalytics::skewness(returns_data$Oil_Return, na.rm = TRUE)
  ),
  Kurtosis = c(
    PerformanceAnalytics::kurtosis(returns_data$BTC_Return, na.rm = TRUE),
    PerformanceAnalytics::kurtosis(returns_data$ETH_Return, na.rm = TRUE),
    PerformanceAnalytics::kurtosis(returns_data$Gold_Return, na.rm = TRUE),
    PerformanceAnalytics::kurtosis(returns_data$Oil_Return, na.rm = TRUE)
  )
)

# Round values
summary_stats[, 2:5] <- round(summary_stats[, 2:5], 2)

cat("\n=== Summary Statistics ===\n")
print(summary_stats)

# Save summary statistics
write.csv(summary_stats, file = "output/summary_statistics.csv", row.names = FALSE)

# ============================================================================
# 2. Correlation Matrix
# ============================================================================
cat("\nCalculating correlation matrix...\n")

# Calculate static correlation matrix for full sample
corr_matrix <- cor(returns_data, use = "complete.obs")
colnames(corr_matrix) <- c("Bitcoin", "Ethereum", "Gold", "Oil")
rownames(corr_matrix) <- c("Bitcoin", "Ethereum", "Gold", "Oil")

cat("\n=== Correlation Matrix (Full Sample) ===\n")
print(round(corr_matrix, 2))

# Save correlation matrix
write.csv(corr_matrix, file = "output/correlation_matrix.csv")

# ============================================================================
# 3. Visualizations
# ============================================================================
cat("\nGenerating visualizations...\n")

# Define event dates for marking
event_dates <- data.frame(
  Event = c("COVID-19 Crash", "Fed Tightening", "FTX Collapse", "Banking Crisis"),
  Date = as.Date(c("2020-03-15", "2022-03-16", "2022-11-11", "2023-03-10"))
)

# Figure 1: Time series of normalized prices (base = 100)
cat("  Creating Figure 1: Normalized Price Time Series...\n")

fig1_data <- combined_data %>%
  select(Date, BTC_Normalized, ETH_Normalized, Gold_Normalized, Oil_Normalized) %>%
  pivot_longer(cols = -Date, names_to = "Asset", values_to = "Normalized_Price") %>%
  mutate(Asset = case_when(
    Asset == "BTC_Normalized" ~ "Bitcoin",
    Asset == "ETH_Normalized" ~ "Ethereum",
    Asset == "Gold_Normalized" ~ "Gold",
    Asset == "Oil_Normalized" ~ "Crude Oil"
  ))

fig1 <- ggplot(fig1_data, aes(x = Date, y = Normalized_Price, color = Asset)) +
  geom_line(linewidth = 0.8) +
  labs(
    title = "Figure 1: Normalized Price Time Series (Base = 100)",
    subtitle = "Price evolution of cryptocurrencies and commodities from January 2020 to September 2024",
    x = "Date",
    y = "Normalized Price (Base = 100)",
    color = "Asset"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10),
    legend.position = "bottom"
  ) +
  scale_color_manual(values = c("Bitcoin" = "#F7931A", "Ethereum" = "#627EEA", 
                                "Gold" = "#FFD700", "Crude Oil" = "#000000"))

ggsave("output/figures/Figure1_Normalized_Prices.png", fig1, width = 12, height = 6, dpi = 300)

# Figure 2: Rolling 90-day BTC-Gold correlation with event markers
cat("  Creating Figure 2: Rolling BTC-Gold Correlation...\n")

fig2 <- ggplot(combined_data, aes(x = Date, y = Rolling_Corr_BTC_Gold_90d)) +
  geom_line(color = "#F7931A", linewidth = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(data = event_dates, aes(xintercept = Date), 
             linetype = "dashed", color = "red", alpha = 0.5) +
  geom_text(data = event_dates, aes(x = Date, y = 0.6, label = Event),
            angle = 90, hjust = -0.1, size = 3, color = "red") +
  labs(
    title = "Figure 2: Rolling 90-Day Correlation Between Bitcoin and Gold",
    subtitle = "Time-varying correlation with major market event markers",
    x = "Date",
    y = "Rolling 90-Day Correlation"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold")) +
  ylim(-0.5, 0.7)

ggsave("output/figures/Figure2_BTC_Gold_Correlation.png", fig2, width = 12, height = 6, dpi = 300)

# Figure 3: Rolling 90-day BTC-Oil correlation
cat("  Creating Figure 3: Rolling BTC-Oil Correlation...\n")

fig3 <- ggplot(combined_data, aes(x = Date, y = Rolling_Corr_BTC_Oil_90d)) +
  geom_line(color = "#000000", linewidth = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(data = event_dates, aes(xintercept = Date), 
             linetype = "dashed", color = "red", alpha = 0.5) +
  geom_text(data = event_dates, aes(x = Date, y = 0.6, label = Event),
            angle = 90, hjust = -0.1, size = 3, color = "red") +
  labs(
    title = "Figure 3: Rolling 90-Day Correlation Between Bitcoin and Crude Oil",
    subtitle = "Time-varying correlation with major market event markers",
    x = "Date",
    y = "Rolling 90-Day Correlation"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold")) +
  ylim(-0.5, 0.7)

ggsave("output/figures/Figure3_BTC_Oil_Correlation.png", fig3, width = 12, height = 6, dpi = 300)

# Figure 4: Distribution histograms of daily returns
cat("  Creating Figure 4: Return Distribution Histograms...\n")

returns_long <- combined_data %>%
  select(Date, BTC_Return, ETH_Return, Gold_Return, Oil_Return) %>%
  pivot_longer(cols = -Date, names_to = "Asset", values_to = "Return") %>%
  filter(!is.na(Return)) %>%
  mutate(Asset = case_when(
    Asset == "BTC_Return" ~ "Bitcoin",
    Asset == "ETH_Return" ~ "Ethereum",
    Asset == "Gold_Return" ~ "Gold",
    Asset == "Oil_Return" ~ "Crude Oil"
  ))

fig4 <- ggplot(returns_long, aes(x = Return * 100, fill = Asset)) +
  geom_histogram(alpha = 0.7, bins = 50) +
  facet_wrap(~ Asset, scales = "free") +
  labs(
    title = "Figure 4: Distribution of Daily Returns",
    subtitle = "Histograms showing return distributions for all assets",
    x = "Daily Return (%)",
    y = "Frequency"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "none"
  ) +
  scale_fill_manual(values = c("Bitcoin" = "#F7931A", "Ethereum" = "#627EEA", 
                               "Gold" = "#FFD700", "Crude Oil" = "#000000"))

ggsave("output/figures/Figure4_Return_Distributions.png", fig4, width = 12, height = 8, dpi = 300)

# Figure 5: Heatmap showing correlation evolution over time
cat("  Creating Figure 5: Correlation Evolution Heatmap...\n")

# Calculate rolling correlations for different periods (quarterly)
combined_data$Year <- year(combined_data$Date)
combined_data$Quarter <- quarter(combined_data$Date)

quarterly_corr <- combined_data %>%
  filter(!is.na(Rolling_Corr_BTC_Gold_90d)) %>%
  group_by(Year, Quarter) %>%
  summarise(
    BTC_Gold = mean(Rolling_Corr_BTC_Gold_90d, na.rm = TRUE),
    BTC_Oil = mean(Rolling_Corr_BTC_Oil_90d, na.rm = TRUE),
    ETH_Gold = mean(Rolling_Corr_ETH_Gold_90d, na.rm = TRUE),
    ETH_Oil = mean(Rolling_Corr_ETH_Oil_90d, na.rm = TRUE),
    BTC_ETH = mean(Rolling_Corr_BTC_ETH_90d, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Period = paste0(Year, "-Q", Quarter)) %>%
  select(Period, BTC_Gold, BTC_Oil, ETH_Gold, ETH_Oil, BTC_ETH) %>%
  pivot_longer(cols = -Period, names_to = "Pair", values_to = "Correlation")

fig5 <- ggplot(quarterly_corr, aes(x = Period, y = Pair, fill = Correlation)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", 
                       midpoint = 0, limits = c(-0.5, 0.7)) +
  labs(
    title = "Figure 5: Evolution of Correlations Over Time",
    subtitle = "Quarterly average rolling correlations between asset pairs",
    x = "Time Period",
    y = "Asset Pair",
    fill = "Correlation"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave("output/figures/Figure5_Correlation_Heatmap.png", fig5, width = 12, height = 6, dpi = 300)

cat("\nAll visualizations saved to output/figures/\n")

# ============================================================================
# 4. Key Relationships Analysis
# ============================================================================
cat("\n=== Key Relationships Between Features ===\n")
cat("\n1. Crypto-Crypto Correlation:\n")
cat("   Bitcoin-Ethereum correlation:", round(corr_matrix[1,2], 2), "\n")
cat("   Strong positive correlation suggests similar market drivers\n\n")

cat("2. Crypto-Commodity Correlations:\n")
cat("   BTC-Gold:", round(corr_matrix[1,3], 2), "\n")
cat("   ETH-Gold:", round(corr_matrix[2,3], 2), "\n")
cat("   BTC-Oil:", round(corr_matrix[1,4], 2), "\n")
cat("   ETH-Oil:", round(corr_matrix[2,4], 2), "\n")
cat("   Low correlations suggest potential diversification benefits\n\n")

cat("3. Traditional Commodity Relationship:\n")
cat("   Gold-Oil correlation:", round(corr_matrix[3,4], 2), "\n")
cat("   Modest positive correlation consistent with inflation sensitivity\n\n")

cat("4. Volatility Hierarchy:\n")
cat("   Cryptocurrencies show significantly higher volatility than commodities\n")
cat("   This suggests higher risk-return profiles for digital assets\n\n")

cat("5. Time-Varying Patterns:\n")
cat("   Rolling correlations show dynamic relationships\n")
cat("   BTC-Gold correlation ranges from", 
    round(min(combined_data$Rolling_Corr_BTC_Gold_90d, na.rm = TRUE), 2),
    "to", round(max(combined_data$Rolling_Corr_BTC_Gold_90d, na.rm = TRUE), 2), "\n")
cat("   Suggests regime-dependent behavior\n\n")

cat("EDA complete! All outputs saved.\n")

