library('tidyverse')

# Loading the dataframes.
train_df <- read.csv('./train.csv')

head(train_df, 10)

train_copy <- train_df

# Selecting desired columns.
cols_list <- c('RANK', 'TEAM_ID', 'GP', 'MIN', 'FGM', 'FG_PCT', 'FG3M', 'FG3A', 'FG3_PCT', 'FTM', 'FTA', 'FT_PCT', 'OREB', 'DREB', 'REB', 'AST', 'STL', 'BLK', 'TOV', 'PF', 'PTS', 'EFF', 'AST_TOV', 'STL_TOV', 'CHANGED_FRANCHISE', 'SEASON_ID')
train_df <- train_df[, cols_list]

head(train_df, 10)

# Convert to factor.
train_df$CHANGED_FRANCHISE <- as.factor(train_df$CHANGED_FRANCHISE)
train_df$SEASON_ID <- as.factor(train_df$SEASON_ID)

# Check for missing values.
sum(is.na(train_df))

logit_mod <- glm(CHANGED_FRANCHISE ~ ., data = train_df, family = binomial())
summary(logit_mod)

# Calculate 95% confidence intervals for coefficients
conf_int <- confint(logit_mod)
conf_int