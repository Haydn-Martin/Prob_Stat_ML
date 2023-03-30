# Different models - see different paths

# Geometric BM (classic model)
# dSt = mu * S(t) * dt + sigma * S(t) * dWt

  install.packages("somebm")
  library(somebm)
  
  # Formulating the model
  
  x0 <- 1 # Start value
  mu <- 0.025 # Expected return
  sigma <- 0.1 # Sigma value (SD of stock)
  t0 <- 0 # Starting time
  t <- 100 # End time
  n <- 100 # Number of periods observed
  
  gbm(x0 = x0, mu = mu, sigma = sigma, t0 = t0, t = t, n = n)
  
  # Plotting 1 path
  
  plot(gbm(x0 = x0, mu = mu, sigma = sigma, t0 = t0, t = t, n = n))
  
  # Plotting multiple paths
  
  nsim <- 100 # Number of paths
  BS <- matrix(nrow = t - t0 + 1, ncol = nsim)
  i <- t0
  
  while (i < nsim + 1) { 
    
    BS[, i] = gbm(x0 = x0, mu = mu, sigma = sigma, t0 = t0, t = t, n = n)
    i <- i + 1
    
  }
  
  # Check for 1 path
  
  plot(t0:t, BS[, 1], xlab = "time", ylab = "price", type = "l")
  
  # Plotting multiple
  matplot(BS, type = "l", lty = 1, pch = 1, col = 1)
  
  
# Merton Jump-Diffusion Model
# https://rdrr.io/github/R-Finance/Meucci/man/SimulateJumpDiffusionMerton.html
# dS(t + 1) = mu * S(t) * dt + sigma * S(t) * dWt + S(t) * k * dqt
# (where qt is a compound poisson proccess and k is the magnitude of the random jump)
# ln(1 + k) ~ N(lambda, delta^2)
# Explanation found here: http://lnu.diva-portal.org/smash/get/diva2:1257256/FULLTEXT01.pdf
  
  install.packages("remotes")
  library(remotes)

  # Formulating the model
  
  SimulateJumpDiffusionMerton = function( m, s, l, a, D, ts, J )
  {
    L = length(ts);
    T = ts[ L ];
    
    # simulate number of jumps; 
    N = rpois( J, l * T );
    
    Jumps = matrix( 0, J, L );
    for( j in 1 : J )
    {
      # simulate jump arrival time
      t = T * rnorm(N[ j ]);
      t = sort(t);
      
      # simulate jump size
      S = a + D * rnorm(N[ j ]);
      
      # put things together
      CumS = cumsum(S);
      Jumps_ts = matrix( 0, 1, L);
      for( n in 1 : L )
      {
        Events = sum( t <= ts[ n ]);
        if( Events )
        {
          Jumps_ts[ n ] = CumS[ Events ];
        }
      }
      
      Jumps[ j, ] = Jumps_ts;
    }
    
    D_Diff = matrix( NaN, J, L );
    for( l in 1 : L )
    {
      Dt = ts[ l ];
      if( l > 1 )
      {
        Dt = ts[ l ] - ts[ l - 1 ];
      }
      
      D_Diff[ , l ] = m * Dt + s * sqrt(Dt) * rnorm(J); 
    }
    
    X = cbind( matrix(0, J, 1), apply(D_Diff, 2, cumsum) + Jumps );
    
    return( X );
  }
  
  
  # Specifying the model
  
  # m = drift (mu)
  # s = sigma (sd)
  # l = k (size of random jump)
  # a = drift of log-jump (lambda)
  # D = sd of log-jump (delta)
  # ts = time steps
  # J = number of simulations
  
  maxt <- 102 # Number of simulations
  JD <- SimulateJumpDiffusionMerton(m = 0.025, s = 0.1, l = 1, a = 0, D = 1.5, 1:maxt, 20)[, 2:maxt]
  
  
  JD <- JD/JD[, 1]
  
  # Testing for one path
  
  plot(1:101, JD[20, ], xlab = "time", ylab = "price", type = "l")
  
  # Plotting for mutliple paths
  
  matplot(t(JD/JD[, 1]), type = "l", lty = 1, pch = 1, col = 1, xlab = "t", ylab = "Cumulative Return")
  

# Mandlebrot fractal model
# https://web.williams.edu/Mathematics/sjmiller/public_html/341Fa09/econ/Mandelbroit_VariationCertainSpeculativePrices.pdf
# Use the Stable Pareto distribution
# https://en.wikipedia.org/wiki/Stable_distribution
# Note: this is only for one tail - use to assess losses and/or gain expected
  
  install.packages("ParetoPosStable")
  library(ParetoPosStable)

  # Specifying the model
  
  n <- 1000 # Number of periods observed
  scalePS <- 1 # lam is scale parameter c
  mewPS <- 0.1 # Sc is location parameter mew
  alphaPS <- 1.7 # v is tability parameter alpha
  

hist(rPPS(n, lam = scalePS, sc = mewPS, v = alphaPS), freq = F, xlab = "X", main = "Pareto Positive Stable")
curve(dPPS(x, lam = scalePS, sc = mewPS, v = alphaPS), add = T, col = "red")

  # We see as we approach alpha = 1, we get very fat tails with huuuuge spikes
  # Mandlebrot thought alpha for cotton was around 1.7