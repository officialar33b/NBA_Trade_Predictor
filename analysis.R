library('tidyverse')
library('GGally')

# Loading the dataframes.
df <- read.csv('./train.csv')%>% mutate(CHANGED_FRANCHISE = factor(CHANGED_FRANCHISE))

head(df, 10)

# Selecting desired columns.
# cols_list <- c('RANK', 'GP', 'MIN', 'FGM', 'FG_PCT', 'FG3M', 'FG3A', 'FG3_PCT', 'FTM', 'FTA', 'FT_PCT', 'OREB', 'DREB', 'REB', 'AST', 'STL', 'BLK', 'TOV', 'PF', 'PTS', 'EFF', 'AST_TOV', 'STL_TOV', 'CHANGED_FRANCHISE', 'SEASON_ID')


## Data Analysis
### 1. Trends.
# Basic Statistical overview:

cat("\n=== Trends Analysis ===\n")

# Basic statistical overview
cat("\nBasic Statistics:\n")
print(summary(df))

# Correlation matrix
cat("\nCorrelation Matrix:\n")
cor_matrix <- df %>% 
  select(where(is.numeric)) %>% 
  cor(use = "complete.obs")
print(cor_matrix)

# Covariance analysis
cat("\nCovariance Analysis:\n")
cov_matrix <- df %>% 
  select(PTS, REB, AST, FG_PCT) %>% 
  cov(use = "complete.obs")
print(cov_matrix)

### 2. Visualizatiions.

cat("\n\n=== Visualizations ===\n")
# Pair plot
pair_plot <- df %>% 
  select(PTS, REB, AST, FG_PCT, CHANGED_FRANCHISE) %>% 
  ggpairs(aes(color = CHANGED_FRANCHISE, alpha = 0.5)) +
  theme_bw() +
  ggtitle("Performance Metrics Colored by Franchise Change Status")
print(pair_plot)

# Boxplot comparison
box_plot <- ggplot(df, aes(x = CHANGED_FRANCHISE, y = EFF, fill = CHANGED_FRANCHISE)) +
  geom_boxplot() +
  scale_fill_manual(values = c("skyblue", "salmon")) +
  labs(title = "EFF by Franchise Change Status",
       x = "Changed Franchise (0=No, 1=Yes)",
       y = "EFF") +
  theme_minimal()
print(box_plot)

# Density plot
density_plot <- ggplot(df, aes(x = EFF, fill = CHANGED_FRANCHISE)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("skyblue", "salmon")) +
  labs(title = "EFF Distribution by Franchise Change Status",
       x = "EFF",
       y = "Density") +
  theme_minimal()
print(density_plot)

### 3. Understanding Outliers.
cat("\n\n=== Outlier Analysis ===\n")

numeric_cols <- c("PTS", "REB", "AST", "FG_PCT", "BLK", "EFF")
z_scores <- df %>% 
  select(all_of(numeric_cols)) %>% 
  scale() %>% 
  abs()

outliers <- z_scores > 3
df_outliers <- df %>% 
  filter(rowSums(outliers) > 0) %>% 
  select(PLAYER, all_of(numeric_cols))

cat("Potential Outliers (Z-score > 3):\n")
print(df_outliers)

# IQR method for points
pts_iqr <- df %>% 
  summarise(
    Q1 = quantile(PTS, 0.25, na.rm = TRUE),
    Q3 = quantile(PTS, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1
  )

pts_outliers <- df %>% 
  filter(PTS < (pts_iqr$Q1 - 1.5 * pts_iqr$IQR) | 
           PTS > (pts_iqr$Q3 + 1.5 * pts_iqr$IQR))

cat("\nPoints Outliers (IQR Method):", nrow(pts_outliers), "players\n")

# Outlier visualization
outlier_plot <- ggplot(df, aes(x = PTS, y = REB)) +
  geom_point(aes(color = CHANGED_FRANCHISE, shape = rowSums(outliers) > 0), size = 3) +
  scale_color_manual(values = c("skyblue", "salmon")) +
  scale_shape_manual(values = c(16, 8)) +
  labs(title = "Outlier Detection in Points-Rebounds Relationship",
       x = "Points",
       y = "Rebounds") +
  theme_minimal()
print(outlier_plot)

# Convert to factor.
train_df$CHANGED_FRANCHISE <- as.factor(train_df$CHANGED_FRANCHISE)
train_df$SEASON_ID <- as.factor(train_df$SEASON_ID)

# Check for missing values.
sum(is.na(train_df))

logit_mod <- glm(CHANGED_FRANCHISE ~ RANK + GP + MIN + FGM + FG_PCT + FG3M + FG3A + FG3_PCT + FTM + FTA + FT_PCT+ +OREB + DREB + REB + AST + STL + BLK + TOV + PF + PTS + EFF + AST_TOV + STL_TOV + CHANGED_FRANCHISE + SEASON_ID, data = df, family = binomial())
summary(logit_mod)

### Diagnostics:
mod_res <- residuals(logit_mod)
mod_fit <- fitted(logit_mod)

plot(mod_fit, mod_res, xlab="Fitted", ylab="Residual")
abline(h=0)


qqnorm(mod_res)
qqline(mod_res, col='salmon')
# Calculate 95% confidence intervals for coefficients
conf_int <- confint(logit_mod)
conf_int