install.packages("quantmod")
install.packages("pracma", repos="http://R-Forge.R-project.org")
library(pracma)
library(quantmod)
library(pacman)
library(PerformanceAnalytics)
library(xts)
library(base)

# Importing data

FTSED <- na.omit(data.frame(getSymbols.yahoo("^FTSE", auto.assign = F)))

FTSEM <- read.csv("~/Documents/Personal/R/Investing Project/FTSE MONTHLY.csv", row.names = 1, header = T)

# Changing column names

colnames(FTSED) <- c("Open", "High", "Low", "Close",  "Volume", "Adjusted")
colnames(FTSEM) <- c("Open", "High", "Low", "Close",  "Adjusted", "Volume")

# Adjusting Adj. price

FTSEDP <- Ad(FTSED)
FTSEMP <- FTSEM[, 1, drop = F]

FTSEMP

# Checking price

plot(FTSEDP, type = 'l')
plot(FTSEMP, type = 'l')

# Getting arithmetic returns

FTSEDP_RET <- dailyReturn(Ad(as.xts(FTSED)), type = 'arithmetic')

  # Denote n the number of time periods:
  n <- nrow(FTSEMP)
  
  FTSEMP_RET <- ((FTSEMP[2:n, 1] - FTSEMP[1:(n-1), 1])/FTSEMP[1:(n-1), 1])
  
  head(FTSEMP_RET)
  
  # Making a vector
  
  FTSEMP_RET <- t(t(FTSEMP_RET))
  
  # Adding date
  
  FTSEMP_RETD <- c(0, FTSEMP_RET)
  head(FTSEMP_RETD)
  FTSEMP_RETD <- cbind(FTSEM[,0], FTSEMP_RETD)
  
  
  # Checking plots
  
  plot(FTSEDP_RET)
  plot(FTSEMP_RET, type = 'l')

  # One-off investment
  
  cumprod(1+FTSEMP_RET)
  plot(cumprod(1+FTSEMP_RET), type = 'l')
  100000*cumprod(1+FTSEMP_RET)
  plot(100000*cumprod(1+FTSEMP_RET), type = 'l')
  length(100000*cumprod(1+FTSEMP_RET))  
  LUMP_SUM_END <- 100000*cumprod(1+FTSEMP_RET)[434]
  
  # Constant investment (100000/434 = 230)
  
  n <- 1
  i <- 0
  while (n<435) { i = (i+230) * (1+FTSEMP_RET[n])
                  n = n+1
  }

  CONST_END <- i

  
  # Fixed return one-off investment
  
  fo <- 0.025
  FIXED1_END <- 100000*(nthroot(fo+1,12)^434)
  FIXED1_END
  
  
  # Fixed return monthly investment
  
  fc <- 0.025
  f <- nthroot(fc+1,12)-1
  f
  n <- 1
  j <- 0
  while (n<435) { j = (j+230) * (1+f)
  n = n+1
  }
  
  j
  FIXED2_END <- j
  
  # Bar chart comparison
  
  barplot(c(LUMP_SUM_END, CONST_END, FIXED2_END, FIXED1_END), names.arg = c("LUMP", "CONST", "FIXC", "FIXL"))
  