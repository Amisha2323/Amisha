# Load required libraries
library(MASS)
library(parallel)
library(ggplot2)

#Set seed
set.seed(123)

#100 bootstrap samples
n.boots <- 100

# Function to fit GLM and return R-squared
fit.glm.rsq <- function(data) {
  model <- glm(medv ~ ., data = data)
  null.dev <- model$null.deviance
  res.dev <- model$deviance
  rsq <- 1 - (res.dev/null.dev)
  return(rsq)
}

#indices for bootstrap samples for serial and parallel
n <- nrow(Boston)
boot.indices <- replicate(n.boots, sample(1:n, n, replace = TRUE))

# Serial execution
serial.start <- Sys.time()
serial.results <- numeric(n.boots)

for(i in 1:n.boots) {
  boot.data <- Boston[boot.indices[,i], ]
  serial.results[i] <- fit.glm.rsq(boot.data)
}

serial.time <- difftime(Sys.time(), serial.start, units = "secs")

# Parallel execution
n.cores <- detectCores() - 1
cl <- makeCluster(n.cores)

# Export required data and functions to the cluster
clusterExport(cl, c("Boston", "boot.indices", "fit.glm.rsq"))

parallel.start <- Sys.time()
parallel.results <- parLapply(cl, 1:n.boots, function(i) {
  boot.data <- Boston[boot.indices[,i], ]
  fit.glm.rsq(boot.data)
})
parallel.results <- unlist(parallel.results)
parallel.time <- difftime(Sys.time(), serial.start, units = "secs")

stopCluster(cl)

# Calculate summary statistics
get.summary <- function(results) {
  list(
    mean = mean(results),
    q1 = quantile(results, 0.25),
    q3 = quantile(results, 0.75),
    iqr = IQR(results)
  )
}

serial.summary <- get.summary(serial.results)
parallel.summary <- get.summary(parallel.results)

results.df <- data.frame(
  R.squared = c(serial.results, parallel.results),
  Method = rep(c("Serial", "Parallel"), each = n.boots)
)

ggplot(results.df, aes(x = R.squared, fill = Method)) +
  geom_density(alpha = 0.5) +
  theme_minimal() +
  labs(title = "Distribution of R-squared Values from Bootstrap Samples",
       x = "R-squared",
       y = "Density") +
  scale_fill_brewer(palette = "Set2")

# Print summary statistics and timing
cat("\nSerial Execution Summary:\n")
cat("Time taken:", serial.time, "seconds\n")
cat("Mean R-squared:", serial.summary$mean, "\n")
cat("IQR:", serial.summary$iqr, "\n")
cat("Q1:", serial.summary$q1, "\n")
cat("Q3:", serial.summary$q3, "\n")

cat("\nParallel Execution Summary:\n")
cat("Time taken:", parallel.time, "seconds\n")
cat("Mean R-squared:", parallel.summary$mean, "\n")
cat("IQR:", parallel.summary$iqr, "\n")
cat("Q1:", parallel.summary$q1, "\n")
cat("Q3:", parallel.summary$q3, "\n")