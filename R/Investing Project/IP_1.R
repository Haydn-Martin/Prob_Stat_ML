install.packages("quantmod")
library(quantmod)
install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
library(pacman)

# Importing CSV after alterred in xl and saved



FTSEM <- read.csv("~/Desktop/FTSEM.csv", row.names = 1, stringsAsFactors = F)

head(FTSEM)


# Renaming columns

colnames(FTSEM) <- c("Open", "High", "Low", "Close",  "Adjusted", "Volume")
head(FTSEM)

class(Ad(FTSEM))

# Isolating adjusted price

FP <- Ad(FTSEM)
head(FP)
class(FP)
length(FP)



as.numeric(FP) #this is not working



# Getting arithmetic returns

dailyReturn(Ad(as.xts(NFP)), type = 'arithmetic')

