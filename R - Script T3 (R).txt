# Load necessary libraries
library(Metrics)
library(ggplot2)

# Importing dataset
df <- read.csv("C:/Users/Chinmai Rayidi/OneDrive/Desktop/RPBDS T3/sgemm_product.csv")

# Display the column names
print(colnames(df))

# Feature engineering
# List the columns to calculate the mean
columns <- c("Run1..ms.", "Run2..ms.", "Run3..ms.", "Run4..ms.")

# Calculate the mean runtime and add it as a new column named 'target'
df$target <- rowMeans(df[columns], na.rm = TRUE)

# Drop the original runtime columns
df <- df[ , !(names(df) %in% columns)]

# Display the updated dataframe
print(head(df))

# Define the features and target variable
x <- df[ , !(names(df) %in% "target")]
y <- df$target

# Split the data into training and testing sets
set.seed(42)
train_index <- sample(seq_len(nrow(df)), size = 0.8 * nrow(df))
x_train <- x[train_index, ]
x_test <- x[-train_index, ]
y_train <- y[train_index]
y_test <- y[-train_index]

# Fit the linear regression model
lm_model <- lm(y_train ~ ., data = data.frame(x_train, y_train))

# Make predictions
lm_pred <- predict(lm_model, x_test)

# Calculate evaluation metrics
lm_mae <- mae(y_test, lm_pred)
lm_mse <- mse(y_test, lm_pred)
lm_rmse <- rmse(y_test, lm_pred)

# Function to calculate R-squared
rsq <- function(actual, predicted) {
  ss_res <- sum((actual - predicted) ^ 2)
  ss_tot <- sum((actual - mean(actual)) ^ 2)
  return(1 - (ss_res / ss_tot))
}
lm_r2 <- rsq(y_test, lm_pred)

# Compile results
results <- data.frame(
  Model = "Linear Regression",
  MAE = lm_mae,
  MSE = lm_mse,
  RMSE = lm_rmse,
  R2 = lm_r2
)

# Print results
print(results)

# Plot Actual vs Predicted values
plot_data <- data.frame(Actual = y_test, Predicted = lm_pred)

ggplot(plot_data, aes(x = Actual, y = Predicted)) +
  geom_point(color = "blue") +
  geom_abline(intercept = 0, slope = 1, color = "red") +  # Line for perfect prediction
  labs(title = "Actual vs. Predicted Values",
       x = "Actual Values",
       y = "Predicted Values") +
  theme_minimal() +
  coord_fixed()  # Equal scaling on both axes for better visualization
