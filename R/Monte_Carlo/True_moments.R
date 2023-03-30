# Structure of moments
#
# How does changing the input variables change the value of moments and entropy
#
# Distributions - normal, lognormal, poisson, student t, pareto, gamma, laplace, cauchy, exponential, levy.


install.packages("plot3D")

library(moments)
library(Pareto)
library(rmutil)
library(plot3D)
library(misc3d)
library(plotly)
library(ggplot2)
library(somebm)

normal_vector <- vector(length = 100)
normal_vector <- seq(-2, 2, by = 0.1)

positive_vector <- vector(length = 100)
positive_vector <- seq(0.05, 2, by = 0.05)

# Normal - https://en.wikipedia.org/wiki/Normal_distribution

  sigma <- 5
  mew <- 40
  
  hist(rnorm(n = 100000, mew, sigma), freq = F)
  curve(dnorm(x, mew, sigma), add = T, col = "red")
  
  # Mean
  # mean = mew
  
  # Variance 
  # variance = sigma^2
  
  norm_var <- (positive_vector^2)
  plot(x = positive_vector, y = norm_var, type = 'l', col = "red", xlab = "Sigma", ylab = "Variance", main = "Normal: variance")
  
  # Skewness
  # Skewness = 0
  
  # Ex. Kurtosis
  # Ex. Kurtosis = 0
  
  # Entropy
  # entropy = 0.5(log(2*pi*e*sigma^2))
  
  norm_ent <- 0.5*(log(2*pi*exp(1)*positive_vector^2))
  plot(x = positive_vector, y = norm_ent, type = 'l', col = "red", xlab = "Sigma", ylab = "Entropy", main = "Entropy: Normal")
  
# Log-normal - https://en.wikipedia.org/wiki/Log-normal_distribution
  
  sdlog <- 0.5
  meanlog <- 4
  
  hist(rlnorm(n = 100000, meanlog, sdlog), freq = F)
  curve(dlnorm(x, meanlog, sdlog), add = T, col = "red")
  
  # Mean
  # mean = exp(meanlog + 0.5*sdlog^2)
  
  # Note that meanlog and mew are interchangeable!
  
  # find_est_var is a function to numerically identify a variable, given variable 1 and output
  # input_var = variable that is already determined (sd)
  # est_var = starting estimate of estimated variable
  # output = output that is alredy determined
  # inc = increments tested
  # acc = accuracy that you want the estimated variable to have
  
  
  log_mean <- function(input_var, est_var, output, inc, acc){
  
    error <- 1
    
    while (abs(error) > acc) {
      
      est_var <- est_var + inc
      
      error <- output - exp(est_var + 0.5*input_var^2) # Replace this function
      
    }  
    
    est_var
  
  }
  
  # Checking functionality
    
  exp(-1.7 + 0.5*0^2)
  log_mean(input_var = 1, est_var = -10, output = 5, acc = 0.01, inc = 0.0001)
    
  # Creating chart
  
  log_sd <- positive_vector
  output_mean <- seq(0.5, 2, 0.5)
  
  log_mew <- matrix(nrow = length(log_sd), ncol = length(output_mean))
  
  for (i in 1:length(output_mean)) {
  
    for (j in 1:length(log_sd)) {
      
      log_mew[j, i] <- log_mean(log_sd[j], est_var = -10, output = output_mean[i], acc = 0.01, inc = 0.0001)
    
    }
  }
  
  log_mew <- as.data.frame(log_mew)
  
  log_mew
  nn <- length(output_mean)
  
  matplot(log_mew, type = "l", lty = 1, pch = 1, xlab = "SD", ylab = "mew", main = "Mean vs. sd and mew")
  legend("topright", legend = output_mean, fill = seq_len(nn), col = seq_len(nn), cex = 0.7)
  
  # Variance
  # variance = [ exp(sdlog^2) - 1 ] * exp(2 * meanlog + sdlog^2)
  
  # find_est_var is a function to numerically identify a variable, given variable 1 and output
  # input_var = variable that is already determined (sd)
  # est_var = estimate of estimated variable
  # output = output that is alredy determined
  # inc = increments tested
  # acc = accuracy that you want the estimated variable to have
  
  log_mean_var <- function(input_var, est_var, output, inc, acc){
    
    error <- 1
    
    while (abs(error) > acc) {
      
      est_var <- est_var + inc
      
      error <- output - ((exp(input_var^2) - 1) * exp(2 * est_var + input_var^2)) # Replace this function
      
    }  
    
    est_var
    
  }
  
  # Checking functionality
  
  (exp(0.05^2) - 1) * exp(0 * 2 + 0.05^2)
  log_mean_var(input_var = 1, est_var = 0, output = 538, acc = 0.01, inc = 0.00001)
  
  # Creating chart
  
  log_sd <- positive_vector
  output_var <- seq(0.5, 4, 0.5)
  
  log_mew <- matrix(nrow = length(log_sd), ncol = length(output_var))
  
  for (i in 1:length(output_var)) {
    
    for (j in 1:length(log_sd)) {
      
      log_mew[j, i] <- log_mean_var(log_sd[j], est_var = -10, output = output_var[i], acc = 0.01, inc = 0.00001)
      
    }
  }
  
  log_mew <- as.data.frame(log_mew)
  log_mew
  
  nn <- length(output_var)
  
  matplot(log_mew, type = "l", lty = 1, pch = 1, xlab = "SD", ylab = "mew", main = "Var vs. sd and mew", col = seq_len(nn))
  legend("topright", legend = output_var, fill = seq_len(nn), col = seq_len(nn), cex = 0.7)
  
  # Skewness
  # skewness = [ exp(sdlog^2) + 2 ] * sqrt(exp(sdlog^2) - 1)
  
  lognorm_skw <- ( exp(log_sd^2) + 2 ) * sqrt(exp(log_sd^2) - 1)
  plot(x = log_sd, y = lognorm_skw, type = 'l', col = "red", xlab = "Sigma", ylab = "Skewness", main = "Lognormal: skewness")
  
  # Kurtosis
  # kurtosis = exp(4 * sdlog^2) + 2 * exp(3 * sdlog^2) + 3 * exp(2 * sdlog^2) - 6
  
  lognorm_krt <- exp(4 * log_sd^2) + 2 * exp(3 * log_sd^2) + 3 * exp(2 * log_sd^2) - 6
  plot(x = log_sd, y = lognorm_krt, type = 'l', col = "red", xlab = "Sigma", ylab = "Kurtosis", main = "Lognormal: kurtosis")
  
  # Entropy
  # entropy = logbase2 (sdlog * exp(meanlog + 1/2) * sqrt(2 * pi))
  
  # find_est_var is a function to numerically identify a variable, given variable 1 and output
  # input_var = variable that is already determined (sd)
  # est_var = estimate of estimated variable
  # output = output that is alredy determined
  # inc = increments tested
  # acc = accuracy that you want the estimated variable to have
  
  log_mean_ent <- function(input_var, est_var, output, inc, acc){
    
    error <- 1
    
    while (abs(error) > acc) {
      
      est_var <- est_var + inc
      
      error <- output - log(input_var * exp(est_var + 1/2) * sqrt(2 * pi), base = 2) # Replace this function
      
    }  
    
    est_var
    
  }
  
  # Checking functionality
  
  log(1 * exp(-10 + 1/2) * sqrt(2 * pi), base = 2)
  log_mean_ent(input_var = 1, est_var = 0, output = 3.49, acc = 0.01, inc = 0.00001)
  
  # Creating chart
  
  log_sd <- positive_vector
  output_var <- seq(0.5, 4, 0.5)
  
  log_mew <- matrix(nrow = length(log_sd), ncol = length(output_var))
  
  for (i in 1:length(output_var)) {
    
    for (j in 1:length(log_sd)) {
      
      log_mew[j, i] <- log_mean_ent(log_sd[j], est_var = -10, output = output_var[i], acc = 0.01, inc = 0.00001) # Replace this function name
      
    }
  }
  
  log_mew <- as.data.frame(log_mew)
  log_mew
  
  nn <- length(output_var)
  
  matplot(log_mew, type = "l", lty = 1, pch = 1, xlab = "SD", ylab = "mew", main = "Entropy vs. sd and mew", col = seq_len(nn))
  legend("topright", legend = output_var, fill = seq_len(nn), col = seq_len(nn), cex = 0.7)
  
  