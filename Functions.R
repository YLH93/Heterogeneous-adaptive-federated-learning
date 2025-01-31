em_logistic <- function(x, y_obs, max_iter = 1000, tol = 1e-6) {
  # Initial guesses
  beta_0 <- 0.0
  beta_1 <- 0.0
  
  for (iter in 1:max_iter) {
    # E-Step: Compute expected values for missing y
    probs <- 1 / (1 + exp(-(beta_0 + beta_1 * x)))
    y_imputed <- ifelse(is.na(y_obs), probs, y_obs)
    
    # M-Step: Fit logistic regression using glm
    model <- glm(y_imputed ~ x, family = binomial(link = "logit"))
    beta_0_new <- coef(model)[1]
    beta_1_new <- coef(model)[2]
    
    # Check convergence
    if (abs(beta_0 - beta_0_new) < tol && abs(beta_1 - beta_1_new) < tol) {
      break
    }
    
    # Update parameters
    beta_0 <- beta_0_new
    beta_1 <- beta_1_new
  }
  
  list(beta_0 = beta_0, beta_1 = beta_1, iterations = iter)
}


em_poisson_with_intervals <- function(x, y_obs, max_iter = 1000, tol = 1e-6, alpha = 0.05) {
  # Initial guesses
  beta_0 <- 0.0
  beta_1 <- 0.0
  n <- length(y_obs)
  
  for (iter in 1:max_iter) {
    # E-Step: Impute missing values
    lambda <- exp(beta_0 + beta_1 * x)
    y_imputed <- ifelse(is.na(y_obs), lambda, y_obs)
    
    # M-Step: Update beta_0 and beta_1 using weighted Poisson regression
    log_lambda <- log(y_imputed)
    model <- lm(log_lambda ~ x)
    beta_0_new <- coef(model)[1]
    beta_1_new <- coef(model)[2]
    
    # Check convergence
    if (abs(beta_0 - beta_0_new) < tol && abs(beta_1 - beta_1_new) < tol) {
      break
    }
    
    # Update parameters
    beta_0 <- beta_0_new
    beta_1 <- beta_1_new
  }
  
  # Compute the Fisher information matrix
  lambda <- exp(beta_0 + beta_1 * x)
  W <- diag(lambda)  # Weight matrix
  X <- cbind(1, x)   # Design matrix
  fisher_info <- t(X) %*% W %*% X
  
  # Standard errors
  fisher_info_inv <- solve(fisher_info)
  se_beta <- sqrt(diag(fisher_info_inv))
  
  # Confidence intervals
  z <- qnorm(1 - alpha / 2)
  beta_0_ci <- c(beta_0 - z * se_beta[1], beta_0 + z * se_beta[1])
  beta_1_ci <- c(beta_1 - z * se_beta[2], beta_1 + z * se_beta[2])
  
  list(
    beta_0 = beta_0,
    beta_1 = beta_1,
    beta_0_ci = beta_0_ci,
    beta_1_ci = beta_1_ci,
    iterations = iter
  )
}