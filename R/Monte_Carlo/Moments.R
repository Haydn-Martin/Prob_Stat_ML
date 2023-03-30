# Distributions - normal, lognormal, poisson, student t, pareto, gamma, laplace, cauchy, exponential, levy.

library(moments)
library(Pareto)
library(rmutil)

# Different values of moments for repeated experiments

moment_dist <- function(reps, trials, dist, mom, var1, var2, var3, var4, var5){
  
  M <- rep(NA, reps)
  
  if(dist == "rnorm"){ # Normal Distribution
    
    if(mom == "mean"){
      for (i in 1:reps) {
        M[i] <- mean(rnorm(trials, mean = var1, sd = var2))
      }
    } else
      if(mom == "sd"){
        for (i in 1:reps) {
          M[i] <- sd(rnorm(trials, mean = var1, sd = var2))
        }
      } else
        if(mom == "skewness"){
          for (i in 1:reps) {
            M[i] <- skewness(rnorm(trials, mean = var1, sd = var2))
          }
        } else
          if(mom == "kurtosis"){
            for (i in 1:reps) {
              M[i] <- kurtosis(rnorm(trials, mean = var1, sd = var2))
            }
          }
  } else
    if(dist == "rlnorm"){ # Log-noral distribution
      
      if(mom == "mean"){
        for (i in 1:reps) {
          M[i] <- mean(rlnorm(trials, meanlog = var1, sdlog = var2))
        }
      } else
        if(mom == "sd"){
          for (i in 1:reps) {
            M[i] <- sd(rlnorm(trials, meanlog = var1, sdlog = var2))
          }
        } else
          if(mom == "skewness"){
            for (i in 1:reps) {
              M[i] <- skewness(rlnorm(trials, meanlog = var1, sdlog = var2))
            }
          } else
            if(mom == "kurtosis"){
              for (i in 1:reps) {
                M[i] <- kurtosis(rlnorm(trials, meanlog = var1, sdlog = var2))
              }
            }
    } else
      if(dist == "rpois"){ # Poisson distribution
        
        if(mom == "mean"){
          for (i in 1:reps) {
            M[i] <- mean(rpois(trials, lambda = var1))
          }
        } else
          if(mom == "sd"){
            for (i in 1:reps) {
              M[i] <- sd(rpois(trials, lambda = var1))
            }
          } else
            if(mom == "skewness"){
              for (i in 1:reps) {
                M[i] <- skewness(rpois(trials, lambda = var1))
              }
            } else
              if(mom == "kurtosis"){
                for (i in 1:reps) {
                  M[i] <- kurtosis(rpois(trials, lambda = var1))
                }
              }
          
      } else
        if(dist == "rt"){ # Student t
          
          if(mom == "mean"){
            for (i in 1:reps) {
              M[i] <- mean(rt(trials, df = var1, ncp = var2))
            }
          } else
            if(mom == "sd"){
              for (i in 1:reps) {
                M[i] <- sd(rt(trials, df = var1, ncp = var2))
              }
            } else
              if(mom == "skewness"){
                for (i in 1:reps) {
                  M[i] <- skewness(rt(trials, df = var1, ncp = var2))
                }
              } else
                if(mom == "kurtosis"){
                  for (i in 1:reps) {
                    M[i] <- kurtosis(rt(trials, df = var1, ncp = var2))
                  }
                }
          
        } else
          if(dist == "rpareto"){ # Pareto
            
            if(mom == "mean"){
              for (i in 1:reps) {
                M[i] <- mean(rpareto(trials, m = var1, s = var2))
              }
            } else
              if(mom == "sd"){
                for (i in 1:reps) {
                  M[i] <- sd(rpareto(trials, m = var1, s = var2))
                }
              } else
                if(mom == "skewness"){
                  for (i in 1:reps) {
                    M[i] <- skewness(rpareto(trials, m = var1, s = var2))
                  }
                } else
                  if(mom == "kurtosis"){
                    for (i in 1:reps) {
                      M[i] <- kurtosis(rpareto(trials, m = var1, s = var2))
                    }
                  }
            
          } else
            if(dist == "rgamma"){ # Gamma
              
              if(mom == "mean"){
                for (i in 1:reps) {
                  M[i] <- mean(rgamma(trials, shape = var1, rate = var2))
                }
              } else
                if(mom == "sd"){
                  for (i in 1:reps) {
                    M[i] <- sd(rgamma(trials, shape = var1, rate = var2))
                  }
                } else
                  if(mom == "skewness"){
                    for (i in 1:reps) {
                      M[i] <- skewness(rgamma(trials, shape = var1, rate = var2))
                    }
                  } else
                    if(mom == "kurtosis"){
                      for (i in 1:reps) {
                        M[i] <- kurtosis(rgamma(trials, shape = var1, rate = var2))
                      }
                    }
              
            } else
              if(dist == "rlaplace"){ # Laplace
                
                if(mom == "mean"){
                  for (i in 1:reps) {
                    M[i] <- mean(rlaplace(trials, m = var1, s = var2))
                  }
                } else
                  if(mom == "sd"){
                    for (i in 1:reps) {
                      M[i] <- sd(rlaplace(trials, m = var1, s = var2))
                    }
                  } else
                    if(mom == "skewness"){
                      for (i in 1:reps) {
                        M[i] <- skewness(rlaplace(trials, m = var1, s = var2))
                      }
                    } else
                      if(mom == "kurtosis"){
                        for (i in 1:reps) {
                          M[i] <- kurtosis(rlaplace(trials, m = var1, s = var2))
                        }
                      }
                
              } else
                if(dist == "rcauchy"){ # Cauchy
                  
                  if(mom == "mean"){
                    for (i in 1:reps) {
                      M[i] <- mean(rcauchy(trials, location = var1, scale = var2))
                    }
                  } else
                    if(mom == "sd"){
                      for (i in 1:reps) {
                        M[i] <- sd(rcauchy(trials, location = var1, scale = var2))
                      }
                    } else
                      if(mom == "skewness"){
                        for (i in 1:reps) {
                          M[i] <- skewness(rcauchy(trials, location = var1, scale = var2))
                        }
                      } else
                        if(mom == "kurtosis"){
                          for (i in 1:reps) {
                            M[i] <- kurtosis(rcauchy(trials, location = var1, scale = var2))
                          }
                        }
                  
                } else
                  if(dist == "rexp"){ # Exponential
                    
                    if(mom == "mean"){
                      for (i in 1:reps) {
                        M[i] <- mean(rexp(trials, rate = var1))
                      }
                    } else
                      if(mom == "sd"){
                        for (i in 1:reps) {
                          M[i] <- sd(rexp(trials, rate = var1))
                        }
                      } else
                        if(mom == "skewness"){
                          for (i in 1:reps) {
                            M[i] <- skewness(rexp(trials, rate = var1))
                          }
                        } else
                          if(mom == "kurtosis"){
                            for (i in 1:reps) {
                              M[i] <- kurtosis(rexp(trials, rate = var1))
                            }
                          }
                    
                  } else
                    if(dist == "rlevy"){ # Levy
                      
                      if(mom == "mean"){
                        for (i in 1:reps) {
                          M[i] <- mean(rlevy(trials, m = var1, s = var2))
                        }
                      } else
                        if(mom == "sd"){
                          for (i in 1:reps) {
                            M[i] <- sd(rlevy(trials, m = var1, s = var2))
                          }
                        } else
                          if(mom == "skewness"){
                            for (i in 1:reps) {
                              M[i] <- skewness(rlevy(trials, m = var1, s = var2))
                            }
                          } else
                            if(mom == "kurtosis"){
                              for (i in 1:reps) {
                                M[i] <- kurtosis(rlevy(trials, m = var1, s = var2))
                              }
                            }
                      
                    }

        
        hist(M, freq = F)
        M
        
      }
  
  
  
  moment_dist(reps = 1000, trials = 100, dist = "rcauchy", mom = "mean", var1 = 0.5, var2 = 1)
  
  
  
  # Key for moment_dist
  #
  # Overview
  #
  # The function gives the histogram of a number of experiments of a specfifc moment of a distribution with a set number of trials
  # E.g. it gives the histogram of the means of 1000 experiments of a normal distribution of set mean and variance a number of outputs
  # It's useful for observing the variability in moments of different distributions for different trials
  #
  # Variables
  #
  # reps = repetitions of the experiment, trials = number of outcomes for each experiment
  # dist = type of dsitribution (use name as used in r), mom = moment measured (as used in r)
  # var = variables values (varies depending on distribution)
  #
  # Variable inputs for distributions
  # 
  # Normal: var1 = mean, var2 = sd
  # Log-normal: var1 = meanlog, var2 = sdlog
  # Poisson: var1 = lambda
  # Student t: var1 = df, var2 = delta
  # Pareto: var1 = scale, var2 = alpha
  # Gamma: var1 = alphaG, var2 = betaG
  # Laplace: var1 = meanlap, var2 = scalelap
  # Cauchy: var1 = locationca, var2 = scaleca
  # Exponential: var1 = lamdaex
  # Levy: var1 = mewlev, var2 = dispertion
  #
  #
  
  
  
  
  