# Shape of PDF

# Distributions - normal, lognormal, poisson, student t, pareto, gamma, laplace, cauchy, exponential, levy.

install.packages("moments")
install.packages("Pareto")
install.packages("rmutil")

library(moments)
library(Pareto)
library(rmutil)
library(graphics)

# Normal

  sd <- 10
  mean <- 40
  
  par(family = "serif")
  hist(rnorm(n = 100000, mean, sd), freq = F, main = "Normal Distribution", ylab = "f(x)", xlab = "x")
  abline(v = 40, col="#cc0000", lwd=2, lty=1)
  curve(dnorm(x, mean, sd), add = T, col = "#0c9300")
  par(family = "")
  
  par(family = "serif")
  x <- seq(from = 0,to = 80, length.out = 100)
  plot(x, dnorm(x, mean, sd), type = 'l', col = "#0c9300", main = "Normal Distribution", ylab = "f(x)", xlab = "x")
  abline(v = 40, col="#cc0000", lwd=2, lty=1)
  par(family = "")
  
# Log-normal

  
  sdlog <- 1.2
  meanlog <- log(40) - sdlog^2/2
  
  hist(rlnorm(n = 100000, meanlog, sdlog), freq = F)
  abline(v = 1000, col="#cc0000", lwd=2, lty=1)
  curve(dlnorm(x, meanlog, sdlog), add = T, col = "red")
  
  par(family = "serif")
  x <- seq(from = 0, to = 80, length.out = 100)
  plot(x, dlnorm(x, meanlog, sdlog), type = 'l', col = "#0c9300", main = "Lognormal Distribution", ylab = "f(x)", xlab = "x")
  abline(v = 40, col="#cc0000", lwd=2, lty=1)
  par(family = "")
  
# Poisson
  
  lambda <- 0.5 # How normal the dist. it

  hist(rpois(n = 10000, lambda), freq = F) 
  # Note: no curve becasue it's discrete
  
# Student t
  
  df <- 10 # Extent at which dist. is fat-tailed (lower = more fat-tailed)
  delta <- 100 # Where the distribution is centred
  
  hist(rt(n = 100000, df = df, ncp = delta), freq = F)
  curve(dt(x, df = df, ncp = delta), add = T, col = "red")
  
  
# Pareto
  scale <- 10 # Values of tails
  alpha <- 20 # How fat tails are
  
  install.packages("sysfonts")
  
  par(family = "serif")
  hist(rpareto(n = 100000, m = scale, s = alpha), freq = F, main = "Pareto Distribution", xlab = "x", ylab = "f(x)", )
  curve(dpareto(x, m = scale, s = alpha), add = T, col = "#0c9300")
  par(family = "")
  
  
# Gamma
  
  alphaG <- 5 # How skewed the dist. is
  betaG <- 5 # Inverse of where the dist. is centred (doesn't change shape)
  
  hist(rgamma(n = 100000, shape = alphaG, rate = betaG), freq = F, main = "Gamma Distribution", xlab = "X")
  curve(dgamma(x, shape = alphaG, rate = betaG), add = T, col = "red")
  
  
# Laplace
  
  meanlap <- 0 # Where the dist. is centred
  scalelap <- 1 # Changes the density values (shape stays the same)
  
  hist(rlaplace(n = 100000, m = meanlap, s = scalelap), freq = F)
  curve(dlaplace(x, m = meanlap, s = scalelap), add = T, col = "red")
  
# Cauchy
  
  locationca <- 0 # Where is the dist. centred
  scaleca <- 1 # Proportional to width of pdf and inversely proportional to density
  
  hist(rcauchy(n = 100000, location = locationca, scale = scaleca), freq = F)
  curve(dcauchy(x, location = locationca, scale = scaleca), add = T, col = "red")
  
# Exponential
  
  lambdaex <- 1/1000 # Lower value = more prob. weight in tail (fatter tail)
  
  hist(rexp(n = 100000, rate = lambdaex), freq = F)
  abline(v = 1000, col="#cc0000", lwd=2, lty=1)
  curve(dexp(x, rate = lambdaex), add = T, col = "red")
  
# Levy
  
  mewlev <- 10000000 # Minimum value that the dist. starts at
  dispertion <- 0.00000005 #How fat-tailed the dist. is
  
  hist(rlevy(n = 100000, m = mewlev, s = dispertion), freq = F)
  curve(dlevy(x, m = mewlev, s = dispertion), add = T, col = "red")
  
  