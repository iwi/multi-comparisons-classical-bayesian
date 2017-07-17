library("arm")


simulator <- function(sigma, tau, N) {
  # Check inputs
  cat("sigma = ", sigma, ", tau = ", tau, ", N = ", N, "\n", sep="")
  
  # Simulate data
  theta <- rnorm(N, 0, tau)
  y <- theta + rnorm(N, 0, sigma)
  
  sim_data <- data.frame(
    theta = theta,
    y = y
  )
  return(sim_data)
}

calculate_classical_outputs <- function(sigma, sim_data) {
  # List and count cases where y is outside two sigmas
  significant = abs(sim_data$y) > 2 * sigma
  classical <- data.frame(
    num_of_exclusions = sum(significant),
    percentage_of_exclusions = as.numeric(fround(100 * mean(significant), 2)),
    mean_absolute_value_for_estimates = as.numeric(fround(mean(abs(sim_data$y)[significant]), 2)),
    mean_absolute_value_for_true_parameters = as.numeric(fround(mean(abs(sim_data$theta)[significant]), 2)),
    percentage_of_wrong_sign = as.numeric(fround(100 * mean((sign(sim_data$theta) != sign(sim_data$y))[significant]), 1))
  )
  
  return(classical)
}
  
show_classical_outputs <- function(classical) {
  cat(classical$num_of_exclusions,
      " (", classical$percentage_of_exclusions,
      "%) of the 95% classical intervals exclude 0\n",
      sep="")
  cat("Mean absolute value of these classical estimates is",
      classical$mean_absolute_value_for_estimates, "\n")
  cat("Mean absolute value of the corresponding true parameters is",
      classical$mean_absolute_value_for_true_parameters, "\n")
  cat(classical$percentage_of_wrong_sign,
      "% of these are the wrong sign (Type S error)\n",
      sep="")
}
  

calculate_bayesian_outputs <- function(sigma, tau, sim_data) {
  theta_hat <- sim_data$y * (1 / sigma^2) / (1 / sigma^2 + 1 / tau^2)
  theta_se <- sqrt(1 / (1 / sigma^2 + 1 / tau^2))
  significant <- abs(theta_hat) > 2 * theta_se
  
  bayesian <- data.frame(
    num_posterior_interval_exclusions = sum(significant),
    percentage_exclusions = as.numeric(fround(100 * mean(significant), 2)),
    mean_estimates = as.numeric(fround(mean(abs(theta_hat)[significant]), 2)),
    mean_absolute_value_true_parameters = as.numeric(fround(mean(abs(sim_data$theta)[significant]), 2)),
    percentage_of_wrong_sign = as.numeric(fround(100 * mean((sign(sim_data$theta) != sign(theta_hat))[significant]), 2))
  )
  
  return(bayesian)
}

show_bayesian_outputs <- function(bayesian){
  cat(bayesian$num_posterior_interval_exclusions,
      " (", bayesian$percentage_exclusions, "%) of the 95% posterior intervals exclude 0\n",
      sep="")
  cat("Mean absolute value of these Bayes estimates is",
      bayesian$mean_estimates, "\n")
  cat("Mean absolute value of the corresponding true parameters is",
      bayesian$mean_absolute_value_true_parameters, "\n")
  cat(bayesian$percentage_of_wrong_sign,
      "% of these are the wrong sign (Type S error)\n", sep="")
}



# Run the example

sigma <- 1
tau <- .5
N <- 1e6
sim_data <- simulator(sigma, tau, N)
classical <- calculate_classical_outputs(sigma, sim_data) 
bayesian <- calculate_bayesian_outputs(sigma, tau, sim_data) 
show_classical_outputs(classical)
show_bayesian_outputs(bayesian)
