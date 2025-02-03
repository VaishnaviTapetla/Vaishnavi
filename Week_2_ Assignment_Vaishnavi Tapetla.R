# Load required libraries
library(MASS)
library(parallel)

set.seed(123)
n_samples <- 100
bootstrap_samples <- lapply(1:n_samples, function(i) {
  Boston[sample(1:nrow(Boston), replace = TRUE), ]
})

# Function to fit GLM and extract AIC as model fit statistic
fit_glm_and_extract_stat <- function(data) {
  model <- glm(medv ~ ., data = data)
  AIC(model)
}

# Serial Execution
serial_fit_stats <- sapply(bootstrap_samples, fit_glm_and_extract_stat)

# Aggregate Serial Results
serial_mean <- mean(serial_fit_stats)
serial_iqr <- IQR(serial_fit_stats)

# Plot Serial Results
hist(serial_fit_stats, main = "Model Fit Statistics (Serial)", xlab = "AIC", col = "skyblue")

# Parallel Execution
cl <- makeCluster(detectCores() - 1)
parallel_fit_stats <- parSapply(cl, bootstrap_samples, fit_glm_and_extract_stat)
stopCluster(cl)

# Aggregate Parallel Results
parallel_mean <- mean(parallel_fit_stats)
parallel_iqr <- IQR(parallel_fit_stats)

# Plot Parallel Results
hist(parallel_fit_stats, main = "Model Fit Statistics (Parallel)", xlab = "AIC", col = "lightgreen")

# Display Mean and IQR
cat("Serial Execution - Mean:", serial_mean, "IQR:", serial_iqr, "\n")
cat("Parallel Execution - Mean:", parallel_mean, "IQR:", parallel_iqr, "\n")
