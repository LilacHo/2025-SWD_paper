# R Version 4.4.2
# Table 3

library(tidyverse)

DD <- read.csv("DD 2021-2023.csv",na.strings = c("","NA"),header = TRUE)
str(DD)


# Define the function
logistic_model <- function(x, a, b) {
  1/(1+exp(a+b*x))
}

# takes your fitted logistic model and a target y value
# returns: the estimated x corresponding to y_target and the 95% confidence interval
estimate_x_at_y <- function(fit, y_target, conf_level = 0.95) {
  # Extract coefficients
  coefs <- coef(fit)
  a_hat <- coefs["a"]
  b_hat <- coefs["b"]
  
  # Log-odds for target y
  log_odds <- log((1 - y_target) / y_target)
  
  # Estimate x corresponding to y_target
  x_hat <- (log_odds - a_hat) / b_hat
  
  # Variance-covariance matrix
  vcov_mat <- vcov(fit)
  
  # Partial derivatives
  d_x_da <- -1 / b_hat
  d_x_db <- -(log_odds - a_hat) / (b_hat^2)
  
  # Variance via delta method
  var_x <- (d_x_da^2) * vcov_mat["a", "a"] +
    (d_x_db^2) * vcov_mat["b", "b"] +
    2 * d_x_da * d_x_db * vcov_mat["a", "b"]
  
  # Standard error
  se_x <- sqrt(var_x)
  
  # Confidence interval
  z <- qnorm(1 - (1 - conf_level) / 2)
  ci_lower <- x_hat - z * se_x
  ci_upper <- x_hat + z * se_x
  
  # Return as list
  return(list(
    x_estimate = x_hat,
    standard_error = se_x,
    conf_level = conf_level,
    conf_interval = c(lower = ci_lower, upper = ci_upper)
  ))
}


# CDD_PRISM #####################
df <- data.frame(
  x = DD$CDD_PRISM,
  y = DD$proportion
)

# Fit model using non-linear least sqaures
fit <- nls(y ~ logistic_model(x, a, b),
            data = df,
            start = list(a=0.001, b=0.001), 
            algorithm = "port"
)

summary(fit)

# Predicted values
y_pred <- predict(fit)

# Actual values
y_obs <- df$y

# R-sqaured
SS_res <- sum((y_obs - y_pred)^2)
SS_tot <- sum((y_obs - mean(y_obs))^2)
R2 <- 1 - (SS_res / SS_tot)

print(R2) # 0.8048875

# Calculate MSE
n <- length(y_obs)
MSE <- SS_res / n
print(MSE) # 0.02272203


# 5%
result <- estimate_x_at_y(fit, 0.05)

# Print results
cat("Estimate of x at y = 0.05:", result$x_estimate, "\n")
cat("Standard Error:", result$standard_error, "\n")
cat(paste0(result$conf_level*100, "% Confidence Interval: [", 
           result$conf_interval["lower.a"], ", ", result$conf_interval["upper.a"], "]\n"))

# 25%
result <- estimate_x_at_y(fit, 0.25)

# Print results
cat("Estimate of x at y = 0.25:", result$x_estimate, "\n")
cat("Standard Error:", result$standard_error, "\n")
cat(paste0(result$conf_level*100, "% Confidence Interval: [", 
           result$conf_interval["lower.a"], ", ", result$conf_interval["upper.a"], "]\n"))


# 50%
result <- estimate_x_at_y(fit, 0.5)

# Print results
cat("Estimate of x at y = 0.5:", result$x_estimate, "\n")
cat("Standard Error:", result$standard_error, "\n")
cat(paste0(result$conf_level*100, "% Confidence Interval: [", 
           result$conf_interval["lower.a"], ", ", result$conf_interval["upper.a"], "]\n"))




## NJ 2021 ########################
NJ2021 <- DD %>%
  filter(Year == 2021 & Location == "NJ")

df_NJ2021 <- data.frame(
  x = NJ2021$CDD_PRISM,
  y = NJ2021$proportion
)

# Fit model using non-linear least sqaures
fit_NJ2021 <- nls(y ~ logistic_model(x, a, b),
                  data = df_NJ2021,
                  start = list(a=0.001, b=0.001), 
                  algorithm = "port"
)

summary(fit_NJ2021)

# Predicted values
y_pred <- predict(fit_NJ2021)

# Actual values
y_obs <- df_NJ2021$y

# R-sqaured
SS_res <- sum((y_obs - y_pred)^2)
SS_tot <- sum((y_obs - mean(y_obs))^2)
R2 <- 1 - (SS_res / SS_tot)

print(R2) # 0.9587515

# Calculate MSE
n <- length(y_obs)
MSE <- SS_res / n
print(MSE) # 0.004666965


# 5%
result <- estimate_x_at_y(fit_NJ2021, 0.05)

# Print results
cat("Estimate of x at y = 0.05:", result$x_estimate, "\n")
cat("Standard Error:", result$standard_error, "\n")
cat(paste0(result$conf_level*100, "% Confidence Interval: [", 
           result$conf_interval["lower.a"], ", ", result$conf_interval["upper.a"], "]\n"))

# 25%
result <- estimate_x_at_y(fit_NJ2021, 0.25)

# Print results
cat("Estimate of x at y = 0.25:", result$x_estimate, "\n")
cat("Standard Error:", result$standard_error, "\n")
cat(paste0(result$conf_level*100, "% Confidence Interval: [", 
           result$conf_interval["lower.a"], ", ", result$conf_interval["upper.a"], "]\n"))


# 50%
result <- estimate_x_at_y(fit_NJ2021, 0.5)

# Print results
cat("Estimate of x at y = 0.5:", result$x_estimate, "\n")
cat("Standard Error:", result$standard_error, "\n")
cat(paste0(result$conf_level*100, "% Confidence Interval: [", 
           result$conf_interval["lower.a"], ", ", result$conf_interval["upper.a"], "]\n"))



## WL2021 ############3
WL2021 <- DD %>%
  filter(Year == 2021 & Location == "WL")

df_WL2021 <- data.frame(
  x = WL2021$CDD_PRISM,
  y = WL2021$proportion
)

# Fit model using non-linear least sqaures
fit_WL2021 <- nls(y ~ logistic_model(x, a, b),
                  data = df_WL2021,
                  start = list(a=0.001, b=0.001), 
                  algorithm = "port"
)

summary(fit_WL2021)

# Predicted values
y_pred <- predict(fit_WL2021)

# Actual values
y_obs <- df_WL2021$y

# R-sqaured
SS_res <- sum((y_obs - y_pred)^2)
SS_tot <- sum((y_obs - mean(y_obs))^2)
R2 <- 1 - (SS_res / SS_tot)

print(R2) # 0.9913517

# Calculate MSE
n <- length(y_obs)
MSE <- SS_res / n
print(MSE) # 0.0009487057



# 5%
result <- estimate_x_at_y(fit_WL2021, 0.05)

# Print results
cat("Estimate of x at y = 0.05:", result$x_estimate, "\n")
cat("Standard Error:", result$standard_error, "\n")
cat(paste0(result$conf_level*100, "% Confidence Interval: [", 
           result$conf_interval["lower.a"], ", ", result$conf_interval["upper.a"], "]\n"))

# 25%
result <- estimate_x_at_y(fit_WL2021, 0.25)

# Print results
cat("Estimate of x at y = 0.25:", result$x_estimate, "\n")
cat("Standard Error:", result$standard_error, "\n")
cat(paste0(result$conf_level*100, "% Confidence Interval: [", 
           result$conf_interval["lower.a"], ", ", result$conf_interval["upper.a"], "]\n"))


# 50%
result <- estimate_x_at_y(fit_WL2021, 0.5)

# Print results
cat("Estimate of x at y = 0.5:", result$x_estimate, "\n")
cat("Standard Error:", result$standard_error, "\n")
cat(paste0(result$conf_level*100, "% Confidence Interval: [", 
           result$conf_interval["lower.a"], ", ", result$conf_interval["upper.a"], "]\n"))



# NJ 2022#############
NJ2022 <- DD %>%
  filter(Year == 2022 & Location == "NJ")

df_NJ2022 <- data.frame(
  x = NJ2022$CDD_PRISM,
  y = NJ2022$proportion
)

# Fit model using non-linear least sqaures
fit_NJ2022 <- nls(y ~ logistic_model(x, a, b),
                  data = df_NJ2022,
                  start = list(a=0.001, b=0.001), 
                  algorithm = "port"
)

summary(fit_NJ2022)

# Predicted values
y_pred <- predict(fit_NJ2022)

# Actual values
y_obs <- df_NJ2022$y

# R-sqaured
SS_res <- sum((y_obs - y_pred)^2)
SS_tot <- sum((y_obs - mean(y_obs))^2)
R2 <- 1 - (SS_res / SS_tot)

print(R2) # 0.9745727

# Calculate MSE
n <- length(y_obs)
MSE <- SS_res / n
print(MSE) # 0.002661693


# 5%
result <- estimate_x_at_y(fit_NJ2022, 0.05)

# Print results
cat("Estimate of x at y = 0.05:", result$x_estimate, "\n")
cat("Standard Error:", result$standard_error, "\n")
cat(paste0(result$conf_level*100, "% Confidence Interval: [", 
           result$conf_interval["lower.a"], ", ", result$conf_interval["upper.a"], "]\n"))

# 25%
result <- estimate_x_at_y(fit_NJ2022, 0.25)

# Print results
cat("Estimate of x at y = 0.25:", result$x_estimate, "\n")
cat("Standard Error:", result$standard_error, "\n")
cat(paste0(result$conf_level*100, "% Confidence Interval: [", 
           result$conf_interval["lower.a"], ", ", result$conf_interval["upper.a"], "]\n"))


# 50%
result <- estimate_x_at_y(fit_NJ2022, 0.5)

# Print results
cat("Estimate of x at y = 0.5:", result$x_estimate, "\n")
cat("Standard Error:", result$standard_error, "\n")
cat(paste0(result$conf_level*100, "% Confidence Interval: [", 
           result$conf_interval["lower.a"], ", ", result$conf_interval["upper.a"], "]\n"))



# WL 2022################
WL2022 <- DD %>%
  filter(Year == 2022 & Location == "WL")

df_WL2022 <- data.frame(
  x = WL2022$CDD_PRISM,
  y = WL2022$proportion
)

# Fit model using non-linear least sqaures
fit_WL2022 <- nls(y ~ logistic_model(x, a, b),
                  data = df_WL2022,
                  start = list(a=0.001, b=0.001), 
                  algorithm = "port"
)

summary(fit_WL2022)

# Predicted values
y_pred <- predict(fit_WL2022)

# Actual values
y_obs <- df_WL2022$y

# R-sqaured
SS_res <- sum((y_obs - y_pred)^2)
SS_tot <- sum((y_obs - mean(y_obs))^2)
R2 <- 1 - (SS_res / SS_tot)

print(R2) # 0.9852281

# Calculate MSE
n <- length(y_obs)
MSE <- SS_res / n
print(MSE) # 0.001712372


# 5%
result <- estimate_x_at_y(fit_WL2022, 0.05)

# Print results
cat("Estimate of x at y = 0.05:", result$x_estimate, "\n")
cat("Standard Error:", result$standard_error, "\n")
cat(paste0(result$conf_level*100, "% Confidence Interval: [", 
           result$conf_interval["lower.a"], ", ", result$conf_interval["upper.a"], "]\n"))

# 25%
result <- estimate_x_at_y(fit_WL2022, 0.25)

# Print results
cat("Estimate of x at y = 0.25:", result$x_estimate, "\n")
cat("Standard Error:", result$standard_error, "\n")
cat(paste0(result$conf_level*100, "% Confidence Interval: [", 
           result$conf_interval["lower.a"], ", ", result$conf_interval["upper.a"], "]\n"))


# 50%
result <- estimate_x_at_y(fit_WL2022, 0.5)

# Print results
cat("Estimate of x at y = 0.5:", result$x_estimate, "\n")
cat("Standard Error:", result$standard_error, "\n")
cat(paste0(result$conf_level*100, "% Confidence Interval: [", 
           result$conf_interval["lower.a"], ", ", result$conf_interval["upper.a"], "]\n"))



# NJ 2023###################
NJ2023 <- DD %>%
  filter(Year == 2023 & Location == "NJ")

df_NJ2023 <- data.frame(
  x = NJ2023$CDD_PRISM,
  y = NJ2023$proportion
)

# Fit model using non-linear least sqaures
fit_NJ2023 <- nls(y ~ logistic_model(x, a, b),
                  data = df_NJ2023,
                  start = list(a=0.001, b=0.001), 
                  algorithm = "port"
)

summary(fit_NJ2023)

# Predicted values
y_pred <- predict(fit_NJ2023)

# Actual values
y_obs <- df_NJ2023$y

# R-sqaured
SS_res <- sum((y_obs - y_pred)^2)
SS_tot <- sum((y_obs - mean(y_obs))^2)
R2 <- 1 - (SS_res / SS_tot)

print(R2) # 0.9827557

# Calculate MSE
n <- length(y_obs)
MSE <- SS_res / n
print(MSE) # 0.002103184


# 5%
result <- estimate_x_at_y(fit_NJ2023, 0.05)

# Print results
cat("Estimate of x at y = 0.05:", result$x_estimate, "\n")
cat("Standard Error:", result$standard_error, "\n")
cat(paste0(result$conf_level*100, "% Confidence Interval: [", 
           result$conf_interval["lower.a"], ", ", result$conf_interval["upper.a"], "]\n"))

# 25%
result <- estimate_x_at_y(fit_NJ2023, 0.25)

# Print results
cat("Estimate of x at y = 0.25:", result$x_estimate, "\n")
cat("Standard Error:", result$standard_error, "\n")
cat(paste0(result$conf_level*100, "% Confidence Interval: [", 
           result$conf_interval["lower.a"], ", ", result$conf_interval["upper.a"], "]\n"))


# 50%
result <- estimate_x_at_y(fit_NJ2023, 0.5)

# Print results
cat("Estimate of x at y = 0.5:", result$x_estimate, "\n")
cat("Standard Error:", result$standard_error, "\n")
cat(paste0(result$conf_level*100, "% Confidence Interval: [", 
           result$conf_interval["lower.a"], ", ", result$conf_interval["upper.a"], "]\n"))



# WL2023##############
WL2023 <- DD %>%
  filter(Year == 2023 & Location == "WL")

df_WL2023 <- data.frame(
  x = WL2023$CDD_PRISM,
  y = WL2023$proportion
)

# Fit model using non-linear least sqaures
fit_WL2023 <- nls(y ~ logistic_model(x, a, b),
                  data = df_WL2023,
                  start = list(a=0.001, b=0.001), 
                  algorithm = "port"
)

summary(fit_WL2023)

# Predicted values
y_pred <- predict(fit_WL2023)

# Actual values
y_obs <- df_WL2023$y

# R-sqaured
SS_res <- sum((y_obs - y_pred)^2)
SS_tot <- sum((y_obs - mean(y_obs))^2)
R2 <- 1 - (SS_res / SS_tot)

print(R2) # 0.9916682

# Calculate MSE
n <- length(y_obs)
MSE <- SS_res / n
print(MSE) # 0.001043896


# 5%
result <- estimate_x_at_y(fit_WL2023, 0.05)

# Print results
cat("Estimate of x at y = 0.05:", result$x_estimate, "\n")
cat("Standard Error:", result$standard_error, "\n")
cat(paste0(result$conf_level*100, "% Confidence Interval: [", 
           result$conf_interval["lower.a"], ", ", result$conf_interval["upper.a"], "]\n"))

# 25%
result <- estimate_x_at_y(fit_WL2023, 0.25)

# Print results
cat("Estimate of x at y = 0.25:", result$x_estimate, "\n")
cat("Standard Error:", result$standard_error, "\n")
cat(paste0(result$conf_level*100, "% Confidence Interval: [", 
           result$conf_interval["lower.a"], ", ", result$conf_interval["upper.a"], "]\n"))


# 50%
result <- estimate_x_at_y(fit_WL2023, 0.5)

# Print results
cat("Estimate of x at y = 0.5:", result$x_estimate, "\n")
cat("Standard Error:", result$standard_error, "\n")
cat(paste0(result$conf_level*100, "% Confidence Interval: [", 
           result$conf_interval["lower.a"], ", ", result$conf_interval["upper.a"], "]\n"))




# NJ###################
NJ <- DD %>%
  filter(Location == "NJ")

df_NJ <- data.frame(
  x = NJ$CDD_PRISM,
  y = NJ$proportion
)

# Fit model using non-linear least sqaures
fit_NJ <- nls(y ~ logistic_model(x, a, b),
              data = df_NJ,
              start = list(a=0.001, b=0.001), 
              algorithm = "port"
)

summary(fit_NJ)

# Predicted values
y_pred <- predict(fit_NJ)

# Actual values
y_obs <- df_NJ$y

# R-sqaured
SS_res <- sum((y_obs - y_pred)^2)
SS_tot <- sum((y_obs - mean(y_obs))^2)
R2 <- 1 - (SS_res / SS_tot)

print(R2) # 0.9111015

# Calculate MSE
n <- length(y_obs)
MSE <- SS_res / n
print(MSE) # 0.01028304


# 5%
result <- estimate_x_at_y(fit_NJ, 0.05)

# Print results
cat("Estimate of x at y = 0.05:", result$x_estimate, "\n")
cat("Standard Error:", result$standard_error, "\n")
cat(paste0(result$conf_level*100, "% Confidence Interval: [", 
           result$conf_interval["lower.a"], ", ", result$conf_interval["upper.a"], "]\n"))

# 25%
result <- estimate_x_at_y(fit_NJ, 0.25)

# Print results
cat("Estimate of x at y = 0.25:", result$x_estimate, "\n")
cat("Standard Error:", result$standard_error, "\n")
cat(paste0(result$conf_level*100, "% Confidence Interval: [", 
           result$conf_interval["lower.a"], ", ", result$conf_interval["upper.a"], "]\n"))


# 50%
result <- estimate_x_at_y(fit_NJ, 0.5)

# Print results
cat("Estimate of x at y = 0.5:", result$x_estimate, "\n")
cat("Standard Error:", result$standard_error, "\n")
cat(paste0(result$conf_level*100, "% Confidence Interval: [", 
           result$conf_interval["lower.a"], ", ", result$conf_interval["upper.a"], "]\n"))





# WL###################
WL <- DD %>%
  filter(Location == "WL")

df_WL <- data.frame(
  x = WL$CDD_PRISM,
  y = WL$proportion
)

# Fit model using non-linear least sqaures
fit_WL <- nls(y ~ logistic_model(x, a, b),
              data = df_WL,
              start = list(a=0.001, b=0.001), 
              algorithm = "port"
)

summary(fit_WL)

# Predicted values
y_pred <- predict(fit_WL)

# Actual values
y_obs <- df_WL$y

# R-sqaured
SS_res <- sum((y_obs - y_pred)^2)
SS_tot <- sum((y_obs - mean(y_obs))^2)
R2 <- 1 - (SS_res / SS_tot)

print(R2) # 0.9491874

# Calculate MSE
n <- length(y_obs)
MSE <- SS_res / n
print(MSE) # 0.005893854

# 5%
result <- estimate_x_at_y(fit_WL, 0.05)

# Print results
cat("Estimate of x at y = 0.05:", result$x_estimate, "\n")
cat("Standard Error:", result$standard_error, "\n")
cat(paste0(result$conf_level*100, "% Confidence Interval: [", 
           result$conf_interval["lower.a"], ", ", result$conf_interval["upper.a"], "]\n"))

# 25%
result <- estimate_x_at_y(fit_WL, 0.25)

# Print results
cat("Estimate of x at y = 0.25:", result$x_estimate, "\n")
cat("Standard Error:", result$standard_error, "\n")
cat(paste0(result$conf_level*100, "% Confidence Interval: [", 
           result$conf_interval["lower.a"], ", ", result$conf_interval["upper.a"], "]\n"))


# 50%
result <- estimate_x_at_y(fit_WL, 0.5)

# Print results
cat("Estimate of x at y = 0.5:", result$x_estimate, "\n")
cat("Standard Error:", result$standard_error, "\n")
cat(paste0(result$conf_level*100, "% Confidence Interval: [", 
           result$conf_interval["lower.a"], ", ", result$conf_interval["upper.a"], "]\n"))

