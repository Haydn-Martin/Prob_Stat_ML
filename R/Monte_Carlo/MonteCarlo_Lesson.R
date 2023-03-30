# https://rpubs.com/Koba/Monte-Carlo-Basic-Example


return <- rnorm(1:100, mean = 9, sd = 12)

hist(return)

sample(return, size = 4) # We see that is generates new returns for each excecution of the code

x <- matrix(ncol = 4, nrow = 10000)
i <- 1

for (year in 1:4) {
  for (i in 1:10000) {
    x[i, year] = sample(return, 1)
  }
}

head(x)
apply(x, 2, mean)


# https://www.youtube.com/watch?v=T_igE6bb6hU


norm.simulated <- rnorm(n = 100, mean = 5, sd = 2)
plot(norm.simulated)
hist(norm.simulated)
hist(norm.simulated, freq = F)
curve(dnorm(x, mean = 5, sd = 2), from = 0, to = 10, add = T, col = "red")

norm.sim.all.1 <- replicate(n = 4, norm.simulated)

apply(norm.sim.all.1, MARGIN = 2, FUN = mean)
apply(norm.sim.all.1, MARGIN = 2, FUN = sd)

# This doesn't work becasue using the SAME sample each time - must be different

norm.sim.all.2 <- replicate(n = 1000, rnorm(n = 100, mean = 5, sd = 2))

apply(norm.sim.all.2, MARGIN = 2, FUN = mean)
apply(norm.sim.all.2, MARGIN = 2, FUN = sd)

# Look at standard deviation of mean - roughly equal to the standard error!

sd(apply(norm.sim.all.2, MARGIN = 2, FUN = mean))
2/sqrt(100)

# Can also use for loop for N trials

N <- 100000

sim_means <- rep(NA, N) # Creating vector

for (i in 1:N) {
  sim_data <- rnorm(n = 100, mean = 5, sd = 2) 
  sim_means[i] <- mean(sim_data)
  rm(sim_data) # Removes sim_data at end of loop
  
} 

hist(sim_means, freq = F)
sd(sim_means)


# https://www.youtube.com/watch?v=xuUMz8exU8Q


hist(runif(n = 1000, min = 0, max = 1), freq = F)
hist(rnorm(n = 1000, mean = 5, sd = 5), freq = F)

random.normal.100.rep <- replicate(n = 4, rnorm(n = 1000, mean = 5, sd = 5))

head(random.normal.100.rep)

apply(X = random.normal.100.rep, MARGIN = 2, FUN = mean)
summary(random.normal.100.rep)


# https://www.youtube.com/watch?v=D3-OgfrFwj8


install.packages("MonteCarlo")
library(MonteCarlo)

set.seed(9) # Seed ensures the exact same random number sequence is produced

MonteCarlo(func = rnorm, nrep = 1000, param_list = )
?MonteCarlo
?set.seed

# Sack - too complicated!
