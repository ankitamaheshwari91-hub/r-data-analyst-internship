# ==============================================================
#  Week 1: Data Cleaning & Preliminary Analysis (EDA) in R
#  Dataset : Current Daily Price of Various Commodities from
#            Various Markets (Mandi) - data.gov.in (Agmarknet)
#            Arrival date: 19-Oct-2026 | 9,516 rows x 10 columns
#  Author  : Ankita Maheshwary
# ==============================================================

# ---------- STEP 0: Packages ----------
# install.packages(c("dplyr", "ggplot2", "readxl"))   # first time only
library(dplyr)
library(ggplot2)
library(readxl)

# ---------- STEP 1: Load data ----------
Data_Internship <- read_excel("Data_Internship.xlsx")
df <- as.data.frame(Data_Internship)          # working copy; original stays safe
dir.create("outputs", showWarnings = FALSE)

# ---------- STEP 2: First look ----------
dim(df)        # 9516 rows, 10 columns
str(df)        # 6 categorical (chr), 3 numeric (num), 1 date (POSIXct)

# ---------- STEP 3: Clean column names & fix types ----------
names(df) <- gsub("_x0020_", "_", names(df))  # "Min_x0020_Price" -> "Min_Price"
df$Arrival_Date <- as.Date(df$Arrival_Date)   # date-time -> date
str(df)

# ---------- STEP 4: Summary statistics & duplicates ----------
summary(df[, c("Min_Price", "Max_Price", "Modal_Price")])   # Min = 0.01 -> suspicious
sum(duplicated(df))                                         # 0 duplicates
df <- df[!duplicated(df), ]
nrow(df)

# ---------- STEP 5: Missing values & invalid prices ----------
colSums(is.na(df))                                          # no blank cells
# Prices below Rs 100/quintal are invalid: data-entry errors (0.01)
# or unit inconsistency (leafy vegetables priced per bunch/kg in Pune markets)
df[df$Modal_Price < 100, c("Commodity", "Market", "Min_Price", "Max_Price", "Modal_Price")]

df$Price_Flag <- df$Modal_Price < 100                       # keep a record of changed rows
sum(df$Price_Flag)                                          # 26 rows

df$Min_Price[df$Price_Flag]   <- NA
df$Max_Price[df$Price_Flag]   <- NA
df$Modal_Price[df$Price_Flag] <- NA
colSums(is.na(df))

# Impute with the median price of the same commodity (median is robust to skew)
df <- df %>%
  group_by(Commodity) %>%
  mutate(across(c(Min_Price, Max_Price, Modal_Price),
                ~ ifelse(is.na(.x), median(.x, na.rm = TRUE), .x))) %>%
  ungroup() %>%
  as.data.frame()
colSums(is.na(df))                                          # 2 left (Rajgir: no valid record)

df[is.na(df$Modal_Price), c("Commodity", "Market")]
df <- df[!is.na(df$Modal_Price), ]
nrow(df)                                                    # 9514

# ---------- STEP 6: Logical consistency check ----------
# Rule: Min_Price <= Modal_Price <= Max_Price
wrong <- df$Min_Price > df$Modal_Price | df$Modal_Price > df$Max_Price
sum(wrong)                                                  # 0
df <- df[!wrong, ]

# ---------- STEP 7: Outlier detection (IQR within each commodity) ----------
df <- df %>%
  group_by(Commodity) %>%
  mutate(Q1 = quantile(Modal_Price, 0.25),
         Q3 = quantile(Modal_Price, 0.75),
         IQR_val = Q3 - Q1,
         Outlier = Modal_Price < (Q1 - 1.5 * IQR_val) |
                   Modal_Price > (Q3 + 1.5 * IQR_val)) %>%
  ungroup() %>%
  as.data.frame()

sum(df$Outlier)                                             # 291
round(mean(df$Outlier) * 100, 2)                            # 3.06 %
head(df[df$Outlier, c("Commodity", "Market", "Modal_Price", "Q1", "Q3")], 10)

top8 <- names(sort(table(df$Commodity), decreasing = TRUE))[1:8]
p_box <- ggplot(df[df$Commodity %in% top8, ], aes(x = Commodity, y = Modal_Price)) +
  geom_boxplot(fill = "#9ecae1", outlier.colour = "red") +
  coord_flip() +
  labs(title = "Modal Price by Commodity (red dots = outliers)",
       y = "Modal Price (Rs per quintal)", x = "")
print(p_box)
ggsave("outputs/01_boxplot_outliers.png", p_box, width = 8, height = 5)

# Outliers are flagged, not deleted, in df. Analysis copy excludes them.
df_clean <- df[!df$Outlier, ]
df_clean <- df_clean[, !(names(df_clean) %in% c("Q1", "Q3", "IQR_val", "Outlier"))]
nrow(df_clean)                                              # 9223

# ---------- STEP 8: Normalization ----------
min_max <- function(x) (x - min(x)) / (max(x) - min(x))
df_clean$Modal_Price_scaled <- min_max(df_clean$Modal_Price)   # 0 to 1
df_clean$Modal_Price_log    <- log(df_clean$Modal_Price)       # reduces skew
summary(df_clean[, c("Modal_Price", "Modal_Price_scaled", "Modal_Price_log")])

# ---------- STEP 9: Categorical encoding ----------
df_clean$State     <- as.factor(df_clean$State)
df_clean$Commodity <- as.factor(df_clean$Commodity)
df_clean$Grade     <- as.factor(df_clean$Grade)

df_clean$State_code <- as.numeric(df_clean$State)              # label encoding
head(unique(df_clean[, c("State", "State_code")]), 8)

table(df_clean$Grade)
grade_dummies <- model.matrix(~ Grade - 1, data = df_clean)    # one-hot encoding
head(grade_dummies)
df_clean <- cbind(df_clean, grade_dummies)

# ---------- STEP 10: Exploratory analysis ----------
sd(df_clean$Modal_Price)
head(sort(table(df_clean$State), decreasing = TRUE), 10)

commodity_summary <- df_clean %>%
  group_by(Commodity) %>%
  summarise(Records = n(),
            Avg_Price = round(mean(Modal_Price)),
            Median_Price = median(Modal_Price)) %>%
  arrange(desc(Records))
head(commodity_summary, 10)

round(cor(df_clean[, c("Min_Price", "Max_Price", "Modal_Price")]), 3)   # ~0.99

# ---------- STEP 11: Visualizations ----------
p_hist <- ggplot(df_clean, aes(x = Modal_Price)) +
  geom_histogram(bins = 40, fill = "#3182bd", colour = "white") +
  scale_x_log10() +
  labs(title = "Distribution of Modal Price (log scale)",
       x = "Modal Price (Rs per quintal, log scale)", y = "Number of records")
print(p_hist)
ggsave("outputs/02_histogram_price.png", p_hist, width = 8, height = 5)

top_states <- as.data.frame(head(sort(table(df_clean$State), decreasing = TRUE), 10))
names(top_states) <- c("State", "Records")
p_state <- ggplot(top_states, aes(x = reorder(State, Records), y = Records)) +
  geom_col(fill = "#31a354") +
  coord_flip() +
  labs(title = "Top 10 States by Number of Mandi Records", x = "", y = "Records")
print(p_state)
ggsave("outputs/03_top_states.png", p_state, width = 8, height = 5)

p_scatter <- ggplot(df_clean, aes(x = Min_Price, y = Modal_Price)) +
  geom_point(alpha = 0.3, colour = "#756bb1") +
  scale_x_log10() + scale_y_log10() +
  labs(title = "Min Price vs Modal Price", x = "Min Price (log)", y = "Modal Price (log)")
print(p_scatter)
ggsave("outputs/04_scatter_min_modal.png", p_scatter, width = 8, height = 5)

# ---------- STEP 12: Save cleaned data ----------
write.csv(df_clean, "outputs/mandi_prices_clean.csv", row.names = FALSE)
