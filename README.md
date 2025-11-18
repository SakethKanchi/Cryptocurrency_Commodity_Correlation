# Cryptocurrency-Commodity Correlation Analysis

**Course:** FA-582  
**Semester:** Fall 2025  
**Group:** Group 7  
**Members:** Jhanvi Damwani, Saketh Kanchi, Ajith Shetty

## Project Overview

This project explores whether cryptocurrency-commodity correlations can act as market regime indicators. We analyze the dynamic relationships between cryptocurrencies (Bitcoin and Ethereum) and commodities (Gold and Crude Oil) from January 2020 to September 2024.

## Project Structure

```
FA582_Project/
├── data/
│   ├── raw/           # Raw data files from Yahoo Finance
│   └── processed/     # Cleaned and processed datasets
├── scripts/           # R analysis scripts
│   ├── 01_data_collection.R
│   ├── 02_data_preprocessing.R
│   ├── 03_eda.R
│   ├── 04_initial_analysis.R
│   └── main_analysis.R
├── output/
│   └── figures/       # Generated visualizations
├── reports/           # Progress report and final report
├── setup.R            # Package installation and setup
└── README.md          # This file
```

## Setup Instructions

1. **Install R** (version 4.0 or higher recommended)

2. **Run the setup script** to install required packages:
   ```r
   source("setup.R")
   ```

3. **Execute the main analysis script**:
   ```r
   source("scripts/main_analysis.R")
   ```

   This will:
   - Collect data from Yahoo Finance
   - Preprocess the data
   - Perform exploratory data analysis
   - Generate all visualizations
   - Create the progress report

## Data Sources

- **Yahoo Finance API**: Historical price data for cryptocurrencies and commodities
- **Tickers**:
  - BTC-USD (Bitcoin)
  - ETH-USD (Ethereum)
  - GC=F (Gold futures)
  - CL=F (Crude oil futures)
- **Time Period**: January 1, 2020 to September 30, 2024

## Key Features

- Data collection from Yahoo Finance API
- Data preprocessing and feature engineering
- Exploratory data analysis with summary statistics
- Rolling correlation analysis
- Statistical testing
- Comprehensive visualizations

## Output Files

- Raw data: `data/raw/*.csv`
- Processed data: `data/processed/*.csv`
- Figures: `output/figures/*.png`
- Progress Report: `reports/progress_report.pdf`

## Notes

- Ensure internet connection for data collection
- Data collection may take a few minutes
- All scripts are designed to be run sequentially via `main_analysis.R`

