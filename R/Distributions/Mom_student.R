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

student_var_vector <- vector(length = 100)
student_var_vector <- seq(2.05, 4, by = 0.05)

student_kur_vector <- vector(length = 100)
student_kur_vector <- seq(4.05, 6, by = 0.05)

# Student t - https://en.wikipedia.org/wiki/Student%27s_t-distribution
# Distribution of local sample mean relative to true mean / sample sd - used as conf. interval for mean

v <- 5 # v is defined as degrees of freedom n - 1 where n is number of obs from the normal dist

hist(rt(n = 100000, df = v), freq = F)
curve(dt(x, df = v), add = T, col = "red")

# Mean
# mean = 0 for v > 1, otherwise undefined

# Variance
# variance = v / (v - 2) for v > 2, inf for 1 < v <= 2, otherwise undefined

student_var <- (student_var_vector / (student_var_vector - 2))
plot(x = student_var_vector, y = student_var, type = 'l', col = "red", xlab = "v", ylab = "Variance", main = "Student t: variance")

# Skewness
# skew = 0 for v > 3, otherwise undefined

# Kurtosis
# kurt = 6 / (v - 4) for v > 4, inf for 2 < v <= 4, otherwise undefined

student_kur <- 6 / (student_kur_vector - 4)
plot(x = student_kur_vector, y = student_kur, type = 'l', col = "red", xlab = "v", ylab = "Kurtosis", main = "Student t: kurtosis")

# Entropy
# entropy = ((v + 1)/2) * (digamma((1 + v) / 2) - digamma (v / 2)) + ln (sqrt(v) * beta( v / 2, 1 / 2))

student_ent <- ((positive_vector + 1)/2) * (digamma((positive_vector + v) / 2) - digamma(positive_vector / 2)) + log(sqrt(positive_vector) * beta( v / 2, 1 / 2))
plot(x = positive_vector, y = student_ent, type = 'l', col = "red", xlab = "v", ylab = "Entropy", main = "Student t: entropy")


