library(pracma)
library(quantmod)
library(pacman)
library(PerformanceAnalytics)
library(xts)
library(base)

# Importing FTSE100 daily

ftse <- na.omit(data.frame(getSymbols.yahoo("^FTSE", auto.assign = F)))

# Naming data

colnames(ftse) <- c("Open", "High", "Low", "Close",  "Volume", "Adjusted")

# Getting arithmetic returns

ftr <- dailyReturn(Ad(as.xts(ftse)), type = 'arithmetic')

# Statistical analysis

  # Whole data

  mean(ftr)
  median(ftr)
  StdDev(ftr)
  hist(ftr)
  
  results <- matrix(c(mean(ftr), median(ftr), StdDev(ftr)))
  rownames(results) <- c("ME", "MD", "ST")
  colnames(results) <- ("ALL")
  results
  
  # 4 Sections
  
  ftr1 <- ftr[2:((length(ftr)/4)+1)]
  ftr2 <- ftr[((length(ftr)/4)+2):((length(ftr)/2)+1)]
  ftr3 <- ftr[((length(ftr)/2)+2):((3*(length(ftr)/4))+1)]
  ftr4 <- ftr[((3*(length(ftr)/4))+1):3339]
  
  results1 <- matrix(c(mean(ftr1), median(ftr1), StdDev(ftr1)))
  results2 <- matrix(c(mean(ftr2), median(ftr2), StdDev(ftr2)))
  results3 <- matrix(c(mean(ftr3), median(ftr3), StdDev(ftr3)))
  results4 <- matrix(c(mean(ftr4), median(ftr4), StdDev(ftr4)))
  
  #NOW MAKE INTO 1 TABLE!!!
  
  # Financial analysis (cumulitive returns, return on investment, etc.)

