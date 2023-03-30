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
plot(x = positive_vector, y = norm_ent, type = 'l', col = "red", xlab = "Sigma", ylab = "Entropy", main = "Normal: entropy")
