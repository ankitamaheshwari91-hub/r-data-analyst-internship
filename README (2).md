# R Data Analyst Internship: Mandi Price Analysis

Virtual R Data Analyst Internship (YuvaIntern), by **Ankita Maheshwary**

**Dataset:** *Current Daily Price of Various Commodities from Various Markets (Mandi)*, Open Government Data Platform India ([data.gov.in](https://data.gov.in/resource/current-daily-price-various-commodities-various-markets-mandi)). 9,516 records, 10 variables, arrival date 19 Oct 2026.

**Tools:** R 4.6.1, RStudio, dplyr, ggplot2, readxl

## Week 1: Data Cleaning & Preliminary Analysis

| File | Description |
|---|---|
| `week1_data_cleaning.R` | Full R script: inspection, cleaning, outliers, normalization, encoding, EDA, charts |
| `mandi_prices_clean.csv` | Cleaned dataset (9,223 rows × 26 columns) |
| `01_boxplot_outliers.png` | Modal price by commodity, with outliers highlighted |
| `02_histogram_price.png` | Distribution of modal price (log scale) |
| `03_top_states.png` | Top 10 states by number of records |
| `04_scatter_min_modal.png` | Min price vs modal price |

### Cleaning steps
1. Fixed column names (`Min_x0020_Price` → `Min_Price`) and the date type
2. Duplicates: none found
3. Invalid prices: 26 records below Rs 100/quintal (Rs 0.01 entry errors and per-bunch pricing in Pune markets) were set to NA and imputed with the commodity median. 2 unrecoverable rows were removed.
4. Logical check (Min ≤ Modal ≤ Max): 0 violations
5. Outliers (IQR within each commodity): 291 (3.06%) flagged and excluded from the analysis copy
6. Min-max scaling and log transform of modal price
7. Label encoding (State) and one-hot encoding (Grade)

### Key findings
- Prices are right-skewed. The log transform makes them nearly symmetric.
- Min, Max and Modal prices are almost perfectly correlated (r ≈ 0.99).
- 67% of records come from Tamil Nadu (sampling bias).
- Most reported commodities: Tomato, Brinjal, Potato.

### How to run
Place `Data_Internship.xlsx` (the downloaded data) in the working directory, then run `week1_data_cleaning.R` in RStudio.
