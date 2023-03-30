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

# Poisson - https://en.wikipedia.org/wiki/Poisson_distribution

k <- 5 # Support
lambda <- 5 # Number of instances

hist(rpois(lambda = lambda, n = 100000), freq = F)

# No line because it's discrete

# Mean
# mean = lambda

# Variance
# variance = lambda

# Skewness
# skewness = lambda^-1/2

pois_skw <- positive_vector^(-1/2)
plot(x = positive_vector, y = pois_skw, type = 'l', col = "red", xlab = "Lambda", ylab = "Skewness", main = "Poisson: skewness")

# Kurtosis
# kurtosis = lambda^-1

pois_krt <- positive_vector^(-1)
plot(x = positive_vector, y = pois_krt, type = 'l', col = "red", xlab = "Lambda", ylab = "Kurtosis", main = "Poisson: kurtosis")

# Entropy
# There was no clear answer in terms of entropy so it has been ommitted