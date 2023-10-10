library(pracma)
library(quantmod)
library(pacman)
library(PerformanceAnalytics)
library(xts)
library(base)
library(plyr)
library(dplyr)
library(ggplot2)
library(zoo)
install.packages("xlsx")
library(xlsx)
install.packages("writexl")
library(writexl)

# Importing data

alldataraw <- read.csv("~/Documents/Personal/R/Investing Project/Real_Return_Raw.csv", stringsAsFactors = F)

# Manipulating data

  # Isolating country names
  
  alldata <- t(alldataraw)
  countnames <- alldata[1,]
  
  # Removing country names and date names
  
  datawcn <- alldata[2:59,]
  datawcn
  row.names(datawcn) <- c()
  
  # Reversing order
  
  datawcn <- apply(datawcn,2,rev)
  
  # Turning data numeric
  
  datawcn <- apply(datawcn, 2, as.numeric)
  aret <- datawcn/100
  
  # Cumilitive return path
  
    # Need to separate by country, can't be stored in same matrix becasue won't be the same size
    
      # Need to remove columns which are all NAs
      
      colnames(aret) <- countnames
      aretdata <- aret[ ,is.na(aret[58,]) == "FALSE"]
      
      # Split into vectors
      
      allaret <- split(aretdata, col(aretdata))
    
      names(allaret) <- countnames[is.na(aret[58,]) == "FALSE"]
      
      # Testing removing NAs
      
      test <- na.trim(allaret[["Australia"]], sides = "left")
      test
      
      # Removing NAs for all countries, getting cumulitive returns, adding names and dates
        # Might to change naming of rows and columns based on graph appearance
    
      n <- 1
      countrydata <- vector("list", 85)
      cumcountrydata <- vector("list", 85)
      while (n<86) {  countrydata [[n]] <- as.data.frame(na.trim(allaret[[n]], sides = "left"))
                      names(countrydata)[[n]] <- names(allaret)[n]
                      cumcountrydata [[n]] <- cumprod(1+countrydata [[n]])
                      names(cumcountrydata)[[n]] <- names(allaret)[n]
                      
                      # For naming rows with dates
                      
                      i <- 1
                      datev <- vector()
                      while (i < 1+length(t(as.data.frame(na.trim(allaret[[n]], sides = "left"))))) { 
                      datev [[i]] <- 2017-length(t(as.data.frame(na.trim(allaret[[n]], sides = "left"))))+i
                      i <- i+1
                        
                      }
                      
                      row.names(countrydata[[n]]) <- datev
                      row.names(cumcountrydata[[n]]) <- datev
                      
                      n <- n+1
                      
                      }
  
  # Plotting 1 country
      
      # Cumulative Returns
      
      name <- "United States" # Enter name of country here
      plot(unlist(cumcountrydata[[name]]), x = row.names(cumcountrydata[[name]]), type = 'l', ylab = "Cumulative Returns", xlab = "Year", main = name)
      
      # Log Returns
      
      nameL <- "United States" # Enter name of country here
      plot(unlist(log(cumcountrydata[[nameL]])), x = row.names(cumcountrydata[[nameL]]), type = 'l', ylab = "Log Cumulative Returns", xlab = "Year", main = nameL)
  
      # Value Returns
      
      nameSW <- "United States" # Enter name of country here
      SW <- 100000 # Enter starting wealth
      plot(unlist(SW*cumcountrydata[[nameSW]]), x = row.names(cumcountrydata[[nameSW]]), type = 'l', ylab = "Portfolio Value", xlab = "Year", main = nameSW)
      
  # Plotting countries with the same time frame
  
      # Determining number of columns
      
      an <- 1
      ldf <- 0
      while (an < 59) { if(length(unlist(cumcountrydata[[an]])) == 57) 
        
      { ldf <- ldf+1} 
        
      an <- an+1
      
      }
  
      # Creating matrix

      
      a <- 1
      b <- 1
      i <- 1
      index <- vector() # Use index to name series
      alltimedf <- matrix(nrow = 57, ncol = ldf)
      while (a < 59) { if(length(unlist(cumcountrydata[[a]])) == 57) 
  
      { alltimedf[,b] <- unlist(cumcountrydata[[a]])
      index[i] <- a
        
      i <- i+1
      b <- b+1
      } 
      
                      
      a <- a+1
      }
      
   
  # Setting row and column names
  
  alltimedf <- as.data.frame(alltimedf)
  
  alltimedf <- cbind(row.names(cumcountrydata[["Canada"]]), alltimedf)  
  
  colnames(alltimedf) <- c("Date", names(cumcountrydata)[[(index[1])]], names(cumcountrydata)[[(index[2])]], names(cumcountrydata)[[(index[3])]])
  
  # Changing to Numeric/Date
  
  alltimedf[,2] <- as.numeric((alltimedf[,2]))
  alltimedf[,3] <- as.numeric((alltimedf[,3]))
  alltimedf[,4] <- as.numeric((alltimedf[,4]))
  alltimedf[,1] <- as.Date(alltimedf[,1], "%Y")

  # Plotting
  
    # Testing for 1
  
  ggplot(alltimedf, aes(x = Date, y = Australia)) + geom_line()
    
    # Plotting all countries
  
  ggplot(alltimedf, aes(x = Date)) +
    geom_line(aes(y = Australia, color = "Australia")) +
    geom_line(aes(y = Canada, color = "Canada")) +
    geom_line(aes(y = Japan, color = "Japan")) +
    ggtitle("All Data Comparison") +
    ylab("Cumilative Growth") +
    labs(color = "Country")
  
# Analyse data - # observations, mean, st. dev, buggest loss, biggest gain, skewness, kurtosis
  
  
  n <- 1
  lvec <- vector(length = 85)
  meanvec <- vector(length = 85)
  sdvec <- vector(length = 85)
  maxvec <- vector(length = 85)
  minvec <- vector(length = 85)
  skewnessvec <- vector(length = 85)
  kurtosisvec <- vector(length = 85)
  
  while (n < 86){
    
    lvec[n] <- length((countrydata[[n]])[,1])
    meanvec[n] <- mean((countrydata[[n]])[,1])
    sdvec[n] <- sd((countrydata[[n]])[,1])
    maxvec[n] <- max((countrydata[[n]])[,1])  
    minvec[n] <- min((countrydata[[n]])[,1])
    skewnessvec[n] <- skewness((countrydata[[n]])[,1])
    kurtosisvec[n] <- kurtosis((countrydata[[n]])[,1])
    
    n <- n+1
    
  }
  

  
  statsdf <- as.data.frame(cbind(lvec, meanvec, sdvec, maxvec, minvec, skewnessvec, kurtosisvec))  
  statsdf <- t(statsdf)   
  colnames(statsdf) <- names(countrydata)
  rownames(statsdf) <- c("Observations", "Mean", "Standard Deviation", "Maximum", "Minumum", "Skewness", "Kurtosis")   
  
  statsdf
  
  statsdf <- as.data.frame(statsdf)
  write_xlsx(statsdf, "~/Documents/Personal/R/Investing Project/statsdfINF.xlsx")
  

  