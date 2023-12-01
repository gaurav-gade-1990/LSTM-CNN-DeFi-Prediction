install.packages("xts")
install.packages("quantmod")
install.packages("dplyr")
install.packages("zoo")
install.packages("haven")
install.packages("MOTE")

library(dplyr)
library(xts)
library(quantmod)
library(zoo)
library(haven)
library(MOTE)
#Data preparation


getwd()

setwd("C:/Users/nagar/Documents/ANLY 699 final project/Defi Datasets")

#Data preparation - normalization and scaling

#Comparing Capital Gains of Multiple Securities Over Time                  

#========DATA PREP==========================


#Import data for DeFi Tokens and BTC - BTC, CARDANO, CHAINLINK, POLKADOT, SOLANA

#Import data for each of the four securities - BTC
data.BTC <- read.csv("BTC-USD.csv",header=TRUE)
date <- as.Date(data.BTC$Date,format="%Y-%m-%d")  
data.BTC <- cbind(date, data.BTC[,-1])
data.BTC <- data.BTC[order(data.BTC$date),]
data.BTC <- xts(data.BTC[,2:7],order.by=data.BTC[,1])
names(data.BTC) <- paste(c("BTC.Open","BTC.High","BTC.Low","BTC.Close","BTC.Adjusted","BTC.Volume"))
data.BTC[c(1:3,nrow(data.BTC)),]

#Import data for each of the four securities - Cardano
data.ADA <- read.csv("ADA-USD.csv",header=TRUE)
date <- as.Date(data.ADA$Date,format="%Y-%m-%d")  
data.ADA <- cbind(date, data.ADA[,-1])
data.ADA <- data.ADA[order(data.ADA$date),]
data.ADA <- xts(data.ADA[,2:7],order.by=data.ADA[,1])
names(data.ADA) <- paste(c("ADA.Open","ADA.High","ADA.Low","ADA.Close","ADA.Adjusted","ADA.Volume"))
data.ADA[c(1:3,nrow(data.ADA)),]

#Import data for each of the four securities - ChainLINK (LINK)
data.LINK <- read.csv("LINK-USD.csv",header=TRUE)
date <- as.Date(data.LINK$Date,format="%Y-%m-%d")  
data.LINK <- cbind(date, data.LINK[,-1])
data.LINK <- data.LINK[order(data.LINK$date),]
data.LINK <- xts(data.LINK[,2:7],order.by=data.LINK[,1])
names(data.LINK) <- paste(c("LINK.Open","LINK.High","LINK.Low","LINK.Close","LINK.Adjusted","LINK.Volume"))
data.LINK[c(1:3,nrow(data.LINK)),]

#Import data for each of the four securities - POLKADOT (DOT)
data.DOT <- read.csv("DOT1-USD.csv",header=TRUE)
date <- as.Date(data.DOT$Date,format="%Y-%m-%d")  
data.DOT <- cbind(date, data.DOT[,-1])
data.DOT <- data.DOT[order(data.DOT$date),]
data.DOT <- xts(data.DOT[,2:7],order.by=data.DOT[,1])
names(data.DOT) <- paste(c("DOT.Open","DOT.High","DOT.Low","DOT.Close","DOT.Adjusted","DOT.Volume"))
data.DOT[c(1:3,nrow(data.DOT)),]

#Import data for each of the four securities - SOLANA (SOL)
data.SOL <- read.csv("SOL1-USD.csv",header=TRUE)
date <- as.Date(data.SOL$Date,format="%Y-%m-%d")  
data.SOL <- cbind(date, data.SOL[,-1])
data.SOL <- data.SOL[order(data.SOL$date),]
data.SOL <- xts(data.SOL[,2:7],order.by=data.SOL[,1])
names(data.SOL) <- paste(c("SOL.Open","SOL.High","SOL.Low","SOL.Close","SOL.Adjusted","SOL.Volume"))
data.SOL[c(1:3,nrow(data.SOL)),]


data.ADA <- subset(data.ADA,
                   index(data.ADA)>"2021-03-31")

data.BTC <- subset(data.BTC,
                   index(data.BTC)>"2021-03-31")

data.DOT <- subset(data.DOT,
                   index(data.DOT)>"2021-03-31")

data.LINK <- subset(data.LINK,
                    index(data.LINK)>"2021-03-31")

data.SOL <- subset(data.SOL,
                   index(data.SOL)>"2021-03-31")



#Combine Data Into one Data Object
Close.Prices <- cbind(data.BTC$BTC.Close, data.ADA$ADA.Close,data.LINK$LINK.Close,data.DOT$DOT.Close,data.SOL$SOL.Close)
Close.Prices[c(1:3,nrow(Close.Prices)),]



#Convert Data into a data.frame

multi.df <- cbind(index(Close.Prices),data.frame(Close.Prices))
names(multi.df) <- paste(c("date","BTC","ADA","LINK","DOT","SOL"))
rownames(multi.df) <- seq(1,nrow(multi.df),1)
multi.df[c(1:3,nrow(multi.df)),]


#Calculate Normalized Values for Each Security
#Create an index for each security with values that equal the price of the 	security on each day divided by the security's price on December 31, 	2010.

multi.df$BTC <- as.numeric(multi.df$BTC)
multi.df$ADA <- as.numeric(multi.df$ADA)
multi.df$LINK <- as.numeric(multi.df$LINK)
multi.df$DOT <- as.numeric(multi.df$DOT)
multi.df$SOL <- as.numeric (multi.df$SOL)

multi.df[c(1:3,nrow(multi.df)),]


multi.df$BTC.idx <- multi.df$BTC/multi.df$BTC[1]
multi.df$ADA.idx <- multi.df$ADA/multi.df$ADA[1]
multi.df$LINK.idx <- multi.df$LINK/multi.df$LINK[1]
multi.df$DOT.idx <- multi.df$DOT/multi.df$DOT[1]
multi.df$SOL.idx <- multi.df$SOL/multi.df$SOL[1]

options(digits=5)   # this option remains in place until end of session
multi.df[c(1:3,nrow(multi.df)),]    

#Plot the Capital Appreciation of Each Security
#For the x variable use date, for y use Qualcomm Index


multi <- data.BTC[,5]
head(multi)
multi <- merge(multi,data.ADA[,5])
multi <- merge(multi,data.LINK[,5])
multi <- merge(multi,data.DOT[,5])
multi <- merge(multi,data.SOL[,5])

multi <
  
  multi[c(1:3,nrow(multi)),]

#============PLOTS======================

plot(x=multi.df$date,
     y=multi.df$BTC.idx,
     type="l",
     xlab="Date",
     ylab="Value of Investment ($)",
     col="orange",
     lty=1,
     lwd=1,
     ylim = c(-1,10),
     main="ROI (x times the Value of $1 Investment in
                         BTC, ADA, LINK, DOT and SOL")

#Adding the lines for the other 5 securities


lines(x=multi.df$date,
      y=multi.df$ADA.idx,
      col="blue",
      lty=1,
      lwd=1)

lines(x=multi.df$date,
      y=multi.df$LINK.idx,
      col="green",
      lty=1,
      lwd=1)


lines(x=multi.df$date,
      y=multi.df$DOT.idx,
      col="red",
      lty=1,
      lwd=1)

lines(x=multi.df$date,
      y=multi.df$SOL.idx,
      col="black",
      lty=1,
      lwd=1)
#Adding a line to denote the starting investment value of $1

abline(h=1,lty=2,col="black")

#Adding a legend
legend("topleft",
       c("BTC","ADA","LINK","DOT","SOL"),
       col=c("orange","blue","green","red","black"),
       lty=c(1,1,1,1,1),
       lwd=c(1,1,1,1,1))

#The plot cuts off the values of some securities.
y.range <- range(multi.df[,6:9])  # find minimum/maximum of all 6 securities
y.range


#------------------------------------------------------
#Calculating individual security returns - HW2 Question 1

#In this dataset, we need the data only for the year 2021 April onwards


data.BTC <- subset(data.BTC,
                   index(data.BTC)>"2021-03-31")

data.ADA <- subset(data.ADA,
                   index(data.ADA)>"2021-03-31")

data.LINK <- subset(data.LINK,
                    index(data.LINK)>"2021-03-31")
data.SOL <- subset(data.SOL,
                   index(data.SOL)>"2021-03-31")
data.DOT <- subset(data.DOT,
                   index(data.DOT)>"2021-03-31")

# Calculate EW portfolio values for 1Q 2019

#Subset to include only closing price


BTC.prc.ret <- data.BTC[,4]
BTC.prc.ret[c(1:3,nrow(BTC.prc.ret)),]

ADA.prc.ret <- data.ADA[,4]
ADA.prc.ret[c(1:3,nrow(ADA.prc.ret)),]

LINK.prc.ret <- data.LINK[,4]
LINK.prc.ret[c(1:3,nrow(LINK.prc.ret)),]

SOL.prc.ret <- data.SOL[,4]
SOL.prc.ret[c(1:3,nrow(SOL.prc.ret)),]

DOT.prc.ret <- data.DOT[,4]
DOT.prc.ret[c(1:3,nrow(DOT.prc.ret)),]


#Calculate  BTC, ADA, LINK, DOT and SOL Price Return (using Delt command)

library(quantmod)


BTC.prc.ret$BTC.prc.ret <-  Delt(as.numeric(BTC.prc.ret$BTC.Close))
BTC.prc.ret[c(1:3,nrow(BTC.prc.ret)),]

ADA.prc.ret$ADA.prc.ret <-  Delt(as.numeric(ADA.prc.ret$ADA.Close))
ADA.prc.ret[c(1:3,nrow(ADA.prc.ret)),]

LINK.prc.ret$LINK.prc.ret <-  Delt(as.numeric(LINK.prc.ret$LINK.Close))
LINK.prc.ret[c(1:3,nrow(LINK.prc.ret)),]

SOL.prc.ret$SOL.prc.ret <-  Delt(as.numeric(SOL.prc.ret$SOL.Close))
SOL.prc.ret[c(1:3,nrow(SOL.prc.ret)),]

DOT.prc.ret$DOT.prc.ret <-  Delt(as.numeric(DOT.prc.ret$DOT.Close))
DOT.prc.ret[c(1:3,nrow(DOT.prc.ret)),]



#Clean up the data object (remove the NA and
#[Stock].close - only need returns column)
options(digits = 5)

BTC.prc.ret<-BTC.prc.ret[-1,2]
BTC.prc.ret[c(1:3,nrow(BTC.prc.ret)),]

ADA.prc.ret<-ADA.prc.ret[-1,2]
ADA.prc.ret[c(1:3,nrow(ADA.prc.ret)),]

LINK.prc.ret<-LINK.prc.ret[-1,2]
LINK.prc.ret[c(1:3,nrow(LINK.prc.ret)),]

SOL.prc.ret<-SOL.prc.ret[-1,2]
SOL.prc.ret[c(1:3,nrow(SOL.prc.ret)),]

DOT.prc.ret<-DOT.prc.ret[-1,2]
DOT.prc.ret[c(1:3,nrow(DOT.prc.ret)),]


#Calculate Daily Total Return
#Import adjusted closing price data


BTC.ret <- data.BTC[,5]
BTC.ret[c(1:3,nrow(BTC.ret)),]

ADA.ret <- data.ADA[,5]
ADA.ret[c(1:3,nrow(ADA.ret)),]

LINK.ret <- data.LINK[,5]
LINK.ret[c(1:3,nrow(LINK.ret)),]

SOL.ret <- data.SOL[,5]
SOL.ret[c(1:3,nrow(SOL.ret)),]

DOT.ret <- data.DOT[,5]
DOT.ret[c(1:3,nrow(DOT.ret)),]

#Calculate Total Return (should be at least as
#large as the price return)




BTC.ret$BTC.tot.ret=Delt(as.numeric(BTC.ret$BTC.Adjusted))
BTC.ret[c(1:3,nrow(BTC.ret)),]

ADA.ret$ADA.tot.ret=Delt(as.numeric(ADA.ret$ADA.Adjusted))
ADA.ret[c(1:3,nrow(ADA.ret)),]

LINK.ret$LINK.tot.ret=Delt(as.numeric(LINK.ret$LINK.Adjusted))
LINK.ret[c(1:3,nrow(LINK.ret)),]

SOL.ret$SOL.tot.ret=Delt(as.numeric(SOL.ret$SOL.Adjusted))
SOL.ret[c(1:3,nrow(SOL.ret)),]

DOT.ret$DOT.tot.ret=Delt(as.numeric(DOT.ret$DOT.Adjusted))
DOT.ret[c(1:3,nrow(DOT.ret)),]


#Clean up the data
options(digits = 3)

options(digits = 3)
BTC.tot.ret <- BTC.ret[-1,2]
BTC.tot.ret[c(1:3,nrow(BTC.tot.ret)),]

options(digits = 3)
ADA.tot.ret <- ADA.ret[-1,2]
ADA.tot.ret[c(1:3,nrow(ADA.tot.ret)),]

options(digits = 3)
LINK.tot.ret <- LINK.ret[-1,2]
LINK.tot.ret[c(1:3,nrow(LINK.tot.ret)),]

options(digits = 3)
SOL.tot.ret <- SOL.ret[-1,2]
SOL.tot.ret[c(1:3,nrow(SOL.tot.ret)),]

options(digits = 3)
DOT.tot.ret <- DOT.ret[-1,2]
DOT.tot.ret[c(1:3,nrow(DOT.tot.ret)),]


#Logarithmic Total Returns


#Logarithmic returns will be used for cumulative returns
# calculations

# Import adjusted closing price data

#BTC

options(digits = 5) #to include decimals
BTC.ret <- data.BTC[,5]
BTC.ret[c(1:3,nrow(BTC.ret)),]

#Logarithmic Total Returns

BTC.log.ret <- data.BTC[,5]
BTC.log.ret[c(1:3,nrow(BTC.log.ret)),]
BTC.log.ret$BTC.log.ret <- diff(log(as.numeric(BTC.log.ret)))
BTC.log.ret[c(1:3,nrow(BTC.log.ret)),]

#Logarithmic Total Returns

#Clean up the data
options(digits=3)
BTC.log.ret <- BTC.log.ret[,2]
BTC.log.ret[c(1:3,nrow(BTC.log.ret)),]

#ADA
options(digits = 5) #to include decimals
ADA.ret <- data.ADA[,5]
ADA.ret[c(1:3,nrow(ADA.ret)),]

#Logarithmic Total Returns
ADA.log.ret <- data.ADA[,5]
ADA.log.ret[c(1:3,nrow(ADA.log.ret)),]
ADA.log.ret$ADA.log.ret <- diff(log(as.numeric(ADA.log.ret)))
ADA.log.ret[c(1:3,nrow(ADA.log.ret)),]

#Logarithmic Total Returns
#Clean up the data

options(digits=3)
ADA.log.ret <- ADA.log.ret[,2]
ADA.log.ret[c(1:3,nrow(ADA.log.ret)),]


#----

#LINK

options(digits = 5) #to include decimals
LINK.ret <- data.LINK[,5]
LINK.ret[c(1:3,nrow(LINK.ret)),]

#Logarithmic Total Returns

LINK.log.ret <- data.LINK[,5]
LINK.log.ret[c(1:3,nrow(LINK.log.ret)),]
LINK.log.ret$LINK.log.ret <- diff(log(as.numeric(LINK.log.ret)))
LINK.log.ret[c(1:3,nrow(LINK.log.ret)),]

#Logarithmic Total Returns

#Clean up the data
options(digits=3)
LINK.log.ret <- LINK.log.ret[,2]
LINK.log.ret[c(1:3,nrow(LINK.log.ret)),]

#----

#SOL

options(digits = 5) #to include decimals
LINK.ret <- data.LINK[,5]
LINK.ret[c(1:3,nrow(LINK.ret)),]

#Logarithmic Total Returns

LINK.log.ret <- data.LINK[,5]
LINK.log.ret[c(1:3,nrow(LINK.log.ret)),]
LINK.log.ret$LINK.log.ret <- diff(log(as.numeric(LINK.log.ret)))
LINK.log.ret[c(1:3,nrow(LINK.log.ret)),]

#Logarithmic Total Returns

#Clean up the data
options(digits=3)
LINK.log.ret[c(1:3,nrow(LINK.log.ret)),]


#----

#DOT

options(digits = 5) #to include decimals
DOT.ret <- data.DOT[,5]
DOT.ret[c(1:3,nrow(DOT.ret)),]

#Logarithmic Total Returns

DOT.log.ret <- data.DOT[,5]
DOT.log.ret[c(1:3,nrow(DOT.log.ret)),]
DOT.log.ret$DOT.log.ret <- diff(log(as.numeric(DOT.log.ret)))
DOT.log.ret[c(1:3,nrow(DOT.log.ret)),]

#Logarithmic Total Returns

#Clean up the data
options(digits=3)
DOT.log.ret[c(1:3,nrow(DOT.log.ret)),]

#-------



options(digits = 5) #to include decimals
SOL.ret <- data.SOL[,5]
SOL.ret[c(1:3,nrow(SOL.ret)),]

#Logarithmic Total Returns
SOL.log.ret <- data.SOL[,5]
SOL.log.ret[c(1:3,nrow(SOL.log.ret)),]
SOL.log.ret$SOL.log.ret <- diff(log(as.numeric(SOL.log.ret)))
SOL.log.ret[c(1:3,nrow(SOL.log.ret)),]

#Logarithmic Total Returns
#Clean up the data
options(digits=3)
SOL.log.ret[c(1:3,nrow(SOL.log.ret)),]

#Compare Log Returns with Arithmetic
#Returns

#BTC
options(digits=3,scipen=100) #increase the threshold to allow reading the small differences
tot.rets <- cbind(BTC.tot.ret,BTC.log.ret)
tot.rets[c(1:3,nrow(tot.rets)),]

summary(tot.rets$BTC.tot.ret)

tot.rets$
  summary(tot.rets$BTC.log.ret)
max(abs(tot.rets$BTC.tot.ret - tot.rets$BTC.log.ret),na.rm=TRUE)
min(abs(tot.rets$BTC.tot.ret - tot.rets$BTC.log.ret),na.rm=TRUE)
options(digits=7,scipen=0)

#ADA
options(digits=3,scipen=100) #increase the
#threshold to allow reading the small differences
tot.rets <- cbind(ADA.tot.ret,ADA.log.ret)
tot.rets[c(1:3,nrow(tot.rets)),]
max(abs(tot.rets$ADA.tot.ret - tot.rets$ADA.log.ret),na.rm=TRUE)
min(abs(tot.rets$ADA.tot.ret - tot.rets$ADA.log.ret),na.rm=TRUE)
options(digits=7,scipen=0)

#LINK
options(digits=3,scipen=100) #increase the
#threshold to allow reading the small differences
tot.rets <- cbind(LINK.tot.ret,LINK.log.ret)
tot.rets[c(1:3,nrow(tot.rets)),]
max(abs(tot.rets$LINK.tot.ret - tot.rets$LINK.log.ret),na.rm=TRUE)
min(abs(tot.rets$LINK.tot.ret - tot.rets$LINK.log.ret),na.rm=TRUE)
options(digits=7,scipen=0)

#Cumulating Multi-Day Returns

AAVE.acum <- AAVE.tot.ret
AAVE.acum[c(1:3,nrow(AAVE.acum)),]
AAVE.acum[1,1] <-0
AAVE.acum[c(1:3,nrow(AAVE.acum)),]
AAVE.acum$GrossRet <- 1+AAVE.acum$AAVE.tot.ret
AAVE.acum[c(1:3,nrow(AAVE.acum)),]
AAVE.acum$GrossCum <- cumprod(AAVE.acum$GrossRet)
AAVE.acum[c(1:3,nrow(AAVE.acum)),]
AAVE.acum$NetCum <- AAVE.acum$GrossCum-1
AAVE.acum[c(1:3,nrow(AAVE.acum)),]

#PLOT
plot(AAVE.acum$AAVE.tot.ret,
     type="l",
     xlab="Date",
     ylab="Multi Day Returns Investment ($)",
     ylim=c(-1,1.5),
     col = "black",
     main="
                    
                            AAVE Stocks Performance Based on 
                             Total Returns - BLACK 
                             Gross returns - RED 
                             Gross Cumulative - GREEN 
                             Net Cumulative Price Returns - BLUE July 2019 -
                             July 2020",
     legend(x = "topleft",
            col = c("black", "red","green", "blue"), lty = 1, lwd = 1,
            legend = c('Total Ret', 'Gross Ret','Gross Cumulative','Net Cumulative')),
     abline(h=1,col="red")
     
)

lines(AAVE.acum$GrossRet,
      type="l", col = "red"
)
lines(AAVE.acum$GrossCum,
      type="l",col = "green"
)
lines(AAVE.acum$NetCum,
      type="l", col = "blue"
)



#BTC
BTC.acum <- BTC.tot.ret
BTC.acum[c(1:3,nrow(BTC.acum)),]
BTC.acum[1,1] <-0
BTC.acum[c(1:3,nrow(BTC.acum)),]
BTC.acum$GrossRet <- 1+BTC.acum$BTC.tot.ret
BTC.acum[c(1:3,nrow(BTC.acum)),]
BTC.acum$GrossCum <- cumprod(BTC.acum$GrossRet)
BTC.acum[c(1:3,nrow(BTC.acum)),]
BTC.acum$NetCum <- BTC.acum$GrossCum-1
BTC.acum[c(1:3,nrow(BTC.acum)),]


#PLOT
plot(BTC.acum$BTC.tot.ret,
     type="l",
     xlab="Date",
     ylab="Multi Day Returns Investment ($)",
     ylim=c(-1,1.5),
     col = "black",
     main="
                    
                          BTC Stocks Performance Based on Total Returns - BLACK 
                         Gross returns - RED 
                         Gross Cumulative - GREEN 
                         Net Cumulative Price Returns - BLUE June 31, 2019 -June 31, 2020")

lines(BTC.acum$GrossRet,
      type="l", col = "red"
)
lines(BTC.acum$GrossCum,
      type="l",col = "green"
)
lines(BTC.acum$NetCum,
      type="l", col = "blue"
)
legend(x = "topleft",
       col = c("black", "red","green", "blue"), lty = 1, lwd = 1,
       legend = c('Total Ret', 'Gross Ret','Gross Cumulative','Net Cumulative'))
abline(h=1,col="red")

#ADA
ADA.acum <- ADA.tot.ret
ADA.acum[c(1:3,nrow(ADA.acum)),]
ADA.acum[1,1] <-0
ADA.acum[c(1:3,nrow(ADA.acum)),]
ADA.acum$GrossRet <- 1+ADA.acum$ADA.tot.ret
ADA.acum[c(1:3,nrow(ADA.acum)),]
ADA.acum$GrossCum <- cumprod(ADA.acum$GrossRet)
ADA.acum[c(1:3,nrow(ADA.acum)),]
ADA.acum$NetCum <- ADA.acum$GrossCum-1
ADA.acum[c(1:3,nrow(ADA.acum)),]


#PLOT
plot(ADA.acum$ADA.tot.ret,
     type="l",
     xlab="Date",
     ylab="Multi Day Returns Investment ($)",
     ylim=c(-1,1.5),
     col = "black",
     main="
                    
                    ADA Stocks Performance Based on Total Returns 
                         Gross returns - 
                         Gross Cumulative -  
                         Net Cumulative Price Returns -  June 31, 2019 - June 31, 2020")

lines(ADA.acum$GrossRet,
      type="l", col = "red",lty=c(1,3))
lines(ADA.acum$GrossCum,
      type="l",col = "green",lty=c(1,3))
lines(ADA.acum$NetCum,
      type="l", col = "blue",lty=c(1,3))

legend("topleft", c('Total Ret', 'Gross Ret','Gross Cumulative','Net Cumulative'),
       col = c("black", "red","green", "blue"), lty = C(1,3), lwd = 1,
       abline(h=1,col="red"))


#LINK
LINK.acum <- LINK.tot.ret
LINK.acum[c(1:3,nrow(LINK.acum)),]
LINK.acum[1,1] <-0
LINK.acum[c(1:3,nrow(LINK.acum)),]
LINK.acum$GrossRet <- 1+LINK.acum$LINK.tot.ret
LINK.acum[c(1:3,nrow(LINK.acum)),]
LINK.acum$GrossCum <- cumprod(LINK.acum$GrossRet)
LINK.acum[c(1:3,nrow(LINK.acum)),]
LINK.acum$NetCum <- LINK.acum$GrossCum-1
LINK.acum[c(1:3,nrow(LINK.acum)),]

#PLOT
plot(LINK.acum$LINK.tot.ret,
     type="l",
     xlab="Date",
     ylab="Multi Day Returns Investment ($)",
     ylim=c(-1,1.5),
     col = "black",
     main="
                    
                            LINK Stocks Performance Based on Total Returns - BLACK 
                             Gross returns - RED 
                             Gross Cumulative - GREEN 
                             Net Cumulative Price Returns - BLUE December 31, 2006 -
                             December 31, 2009")

lines(LINK.acum$GrossRet,
      type="l", col = "red"
)
lines(LINK.acum$GrossCum,
      type="l",col = "green"
)
lines(LINK.acum$NetCum,
      type="l", col = "blue"
)
legend(x = "topleft",
       col = c("black", "red","green", "blue"), lty = 1, lwd = 1,
       legend = c('Total Ret', 'Gross Ret','Gross Cumulative','Net Cumulative'))
abline(h=1,col="red")

#SOL
SOL.acum <- SOL.tot.ret
SOL.acum[c(1:3,nrow(SOL.acum)),]
SOL.acum[1,1] <-0
SOL.acum[c(1:3,nrow(SOL.acum)),]
SOL.acum$GrossRet <- 1+SOL.acum$SOL.tot.ret
SOL.acum[c(1:3,nrow(SOL.acum)),]
SOL.acum$GrossCum <- cumprod(SOL.acum$GrossRet)
SOL.acum[c(1:3,nrow(SOL.acum)),]
SOL.acum$NetCum <- SOL.acum$GrossCum-1
SOL.acum[c(1:3,nrow(SOL.acum)),]

#PLOT
plot(SOL.acum$SOL.tot.ret,
     type="l",
     xlab="Date",
     ylab="Multi Day Returns Investment ($)",
     ylim=c(-1,1.5),
     col = "black",
     main="
                    
                    
                    SOL Stocks Performance Based on Total Returns - BLACK 
                             Gross returns - RED 
                             Gross Cumulative - GREEN 
                             Net Cumulative Price Returns - BLUE December 31, 2006 -
                             December 31, 2009")

lines(SOL.acum$GrossRet,
      type="l", col = "red"
)
lines(SOL.acum$GrossCum,
      type="l",col = "green"
)
lines(SOL.acum$NetCum,
      type="l", col = "blue"
)
legend(x = "topleft",
       col = c("black", "red","green", "blue"), lty = 1, lwd = 1,
       legend = c('Total Ret', 'Gross Ret','Gross Cumulative','Net Cumulative'))
abline(h=1,col="red")


#DOT
DOT.acum <- DOT.tot.ret
DOT.acum[c(1:3,nrow(DOT.acum)),]
DOT.acum[1,1] <-0
DOT.acum[c(1:3,nrow(DOT.acum)),]
DOT.acum$GrossRet <- 1+DOT.acum$DOT.tot.ret
DOT.acum[c(1:3,nrow(DOT.acum)),]
DOT.acum$GrossCum <- cumprod(DOT.acum$GrossRet)
DOT.acum[c(1:3,nrow(DOT.acum)),]
DOT.acum$NetCum <- DOT.acum$GrossCum-1
DOT.acum[c(1:3,nrow(DOT.acum)),]

#PLOT
plot(DOT.acum$DOT.tot.ret,
     type="l",
     xlab="Date",
     ylab="Multi Day Returns Investment ($)",
     ylim=c(-1,1.5),
     col = "black",
     main="
                              
                             DOT Stocks Performance Based on Total Returns - BLACK 
                             Gross returns - RED 
                             Gross Cumulative - GREEN 
                             Net Cumulative Price Returns - BLUE December 31, 2006 -
                             December 31, 2009")

lines(DOT.acum$GrossRet,
      type="l", col = "red"
)
lines(DOT.acum$GrossCum,
      type="l",col = "green"
)
lines(DOT.acum$NetCum,
      type="l", col = "blue"
)
legend(x = "topleft",
       col = c("black", "red","green", "blue"), lty = 1, lwd = 1,
       legend = c('Total Ret', 'Gross Ret','Gross Cumulative','Net Cumulative'))
abline(h=1,col="red")


# Cumulating Logarithmic Returns



#BTC
BTC.logcum <- BTC.log.ret
BTC.logcum[c(1:3,nrow(BTC.logcum)),]
BTC.logcum[1,1] <- 0
BTC.logcum[c(1:3,nrow(BTC.logcum)),]
logcumret=sum(BTC.logcum$BTC.log.ret)
logcumret
BTC.cumret=exp(logcumret)-1
BTC.cumret

#ADA
ADA.logcum <- ADA.log.ret
ADA.logcum[c(1:3,nrow(ADA.logcum)),]
ADA.logcum[1,1] <- 0
ADA.logcum[c(1:3,nrow(ADA.logcum)),]
logcumret=sum(ADA.logcum$ADA.log.ret)
logcumret
ADA.cumret=exp(logcumret)-1
ADA.cumret

#LINK
LINK.logcum <- LINK.log.ret
LINK.logcum[c(1:3,nrow(LINK.logcum)),]
LINK.logcum[1,1] <- 0
LINK.logcum[c(1:3,nrow(LINK.logcum)),]
logcumret=sum(LINK.logcum$LINK.log.ret)
logcumret
LINK.cumret=exp(logcumret)-1
LINK.cumret

#Comparing Price Returns and Total Returns

AAVE.Ret <- cbind(AAVE.prc.ret,AAVE.tot.ret)
names(AAVE.Ret) <- c("prc.ret","tot.ret")
AAVE.Ret[c(1:3,nrow(AAVE.Ret)),]
AAVE.Ret$prc.ret[1] <- 0
AAVE.Ret$tot.ret[1] <- 0
AAVE.Ret[c(1:3,nrow(AAVE.Ret)),]
AAVE.Ret$gross.prc <- 1+AAVE.Ret$prc.ret
AAVE.Ret$gross.tot <- 1+AAVE.Ret$tot.ret
AAVE.Ret[c(1:3,nrow(AAVE.Ret)),]
AAVE.Ret$cum.prc <- cumprod(AAVE.Ret$gross.prc)
AAVE.Ret$cum.tot <- cumprod(AAVE.Ret$gross.tot)
AAVE.Ret[c(1:3,nrow(AAVE.Ret)),]
AAVE.y.range <- range(AAVE.Ret[,5:6])
AAVE.y.range

#BTC

BTC.Ret <- cbind(BTC.prc.ret,BTC.tot.ret)
names(BTC.Ret) <- c("prc.ret","tot.ret")
BTC.Ret[c(1:3,nrow(BTC.Ret)),]
BTC.Ret$prc.ret[1] <- 0
BTC.Ret$tot.ret[1] <- 0
BTC.Ret[c(1:3,nrow(BTC.Ret)),]
BTC.Ret$gross.prc <- 1+BTC.Ret$prc.ret
BTC.Ret$gross.tot <- 1+BTC.Ret$tot.ret
BTC.Ret[c(1:3,nrow(BTC.Ret)),]
BTC.Ret$cum.prc <- cumprod(BTC.Ret$gross.prc)
BTC.Ret$cum.tot <- cumprod(BTC.Ret$gross.tot)
BTC.Ret[c(1:3,nrow(BTC.Ret)),]
BTC.y.range <- range(BTC.Ret[,5:6])
BTC.y.range

#ADA
ADA.Ret <- cbind(ADA.prc.ret,ADA.tot.ret)
names(ADA.Ret) <- c("prc.ret","tot.ret")
ADA.Ret[c(1:3,nrow(ADA.Ret)),]
ADA.Ret$prc.ret[1] <- 0
ADA.Ret$tot.ret[1] <- 0
ADA.Ret[c(1:3,nrow(ADA.Ret)),]
ADA.Ret$gross.prc <- 1+ADA.Ret$prc.ret
ADA.Ret$gross.tot <- 1+ADA.Ret$tot.ret
ADA.Ret[c(1:3,nrow(ADA.Ret)),]
ADA.Ret$cum.prc <- cumprod(ADA.Ret$gross.prc)
ADA.Ret$cum.tot <- cumprod(ADA.Ret$gross.tot)
ADA.Ret[c(1:3,nrow(ADA.Ret)),]
ADA.y.range <- range(ADA.Ret[,5:6])
ADA.y.range

#LINK
LINK.Ret <- cbind(LINK.prc.ret,LINK.tot.ret)
names(LINK.Ret) <- c("prc.ret","tot.ret")
LINK.Ret[c(1:3,nrow(LINK.Ret)),]
LINK.Ret$prc.ret[1] <- 0
LINK.Ret$tot.ret[1] <- 0
LINK.Ret[c(1:3,nrow(LINK.Ret)),]
LINK.Ret$gross.prc <- 1+LINK.Ret$prc.ret
LINK.Ret$gross.tot <- 1+LINK.Ret$tot.ret
LINK.Ret[c(1:3,nrow(LINK.Ret)),]
LINK.Ret$cum.prc <- cumprod(LINK.Ret$gross.prc)
LINK.Ret$cum.tot <- cumprod(LINK.Ret$gross.tot)
LINK.Ret[c(1:3,nrow(LINK.Ret)),]
LINK.y.range <- range(LINK.Ret[,5:6])
LINK.y.range

#SOL
SOL.Ret <- cbind(SOL.prc.ret,SOL.tot.ret)
names(SOL.Ret) <- c("prc.ret","tot.ret")
SOL.Ret[c(1:3,nrow(SOL.Ret)),]
SOL.Ret$prc.ret[1] <- 0
SOL.Ret$tot.ret[1] <- 0
SOL.Ret[c(1:3,nrow(SOL.Ret)),]
SOL.Ret$gross.prc <- 1+SOL.Ret$prc.ret
SOL.Ret$gross.tot <- 1+SOL.Ret$tot.ret
SOL.Ret[c(1:3,nrow(SOL.Ret)),]
SOL.Ret$cum.prc <- cumprod(SOL.Ret$gross.prc)
SOL.Ret$cum.tot <- cumprod(SOL.Ret$gross.tot)
SOL.Ret[c(1:3,nrow(SOL.Ret)),]
SOL.y.range <- range(SOL.Ret[,5:6])
SOL.y.range


#DOT
DOT.Ret <- cbind(DOT.prc.ret,DOT.tot.ret)
names(DOT.Ret) <- c("prc.ret","tot.ret")
DOT.Ret[c(1:3,nrow(DOT.Ret)),]
DOT.Ret$prc.ret[1] <- 0
DOT.Ret$tot.ret[1] <- 0
DOT.Ret[c(1:3,nrow(DOT.Ret)),]
DOT.Ret$gross.prc <- 1+DOT.Ret$prc.ret
DOT.Ret$gross.tot <- 1+DOT.Ret$tot.ret
DOT.Ret[c(1:3,nrow(DOT.Ret)),]
DOT.Ret$cum.prc <- cumprod(DOT.Ret$gross.prc)
DOT.Ret$cum.tot <- cumprod(DOT.Ret$gross.tot)
DOT.Ret[c(1:3,nrow(DOT.Ret)),]
DOT.y.range <- range(DOT.Ret[,5:6])
DOT.y.range

#PLOT
plot(AAVE.Ret$cum.tot,
     type="l",
     auto.grid=FALSE,
     xlab="Date",
     ylab="Value of Investment ($)",
     ylim=y.range,
     minor.ticks=FALSE,
     main="AAVE Stocks Performance Based on Total
                               Returns and Price Returns")
lines(AAVE.Ret$cum.prc,
      type="l",
      lty=3)
abline(h=1,col="red")
legend("topleft",
       c("Value Based on Total Return",
         "Value Based on Price Return"),
       col=c("black","black"),
       lty=c(1,3))
#AAVE
plot(AAVE.Ret$cum.tot,
     type="l",
     auto.grid=FALSE,
     xlab="Date",
     ylab="Value of Investment ($)",
     #ylim=y.range,
     minor.ticks=FALSE,
     main="AAVE Stocks Performance Based on Total
                               Returns and Price Return 2019-20")
lines(AAVE.Ret$cum.prc,
      type="l",
      lty=3)
abline(h=1,col=2)
legend("topleft",
       c("Value Based on Total Return",
         "Value Based on Price Return"),
       col=c("black","black"),
       lty=c(2,1))

#BTC
plot(BTC.Ret$cum.tot,
     type="l",
     auto.grid=FALSE,
     xlab="Date",
     ylab="Value of Investment ($)",
     #ylim=y.range,
     minor.ticks=FALSE,
     main="BTC Stocks Performance Based on Total
                               Returns and Price Returns 2019-20")
lines(BTC.Ret$cum.prc,
      type="l",
      lty=3)
abline(h=1,col=2)
legend("topleft",
       c("Value Based on Total Return",
         "Value Based on Price Return"),
       col=c("black","black"),
       lty=c(2,1))

#ADA
plot(ADA.Ret$cum.tot,
     type="l",
     auto.grid=FALSE,
     xlab="Date",
     ylab="Value of Investment ($)",
     ylim= c(0,2),
     minor.ticks=FALSE,
     main="ADA Stocks Performance Based on Total
                               Returns and Price Returns 2019-20")
lines(ADA.Ret$cum.prc,
      type="l",
      lty=3)
abline(h=1,col="red")
legend("topleft",
       c("Value Based on Total Return",
         "Value Based on Price Return"),
       col=c("black","black"),
       lty=c(1,3))

#LINK

plot(LINK.Ret$cum.tot,
     type="l",
     auto.grid=FALSE,
     xlab="Date",
     ylab="Value of Investment ($)",
     ylim= c(0,2),
     minor.ticks=FALSE,
     main="LINK Stocks Performance Based on Total
                               Returns and Price Returns 2019-20")
lines(LINK.Ret$cum.prc,
      type="l",
      lty=3)
abline(h=1,col="red")
legend("topleft",
       c("Value Based on Total Return",
         "Value Based on Price Return"),
       col=c("black","black"),
       lty=c(1,3))

#SOL

plot(SOL.Ret$cum.tot,
     type="l",
     auto.grid=FALSE,
     xlab="Date",
     ylab="Value of Investment ($)",
     ylim= c(0,2),
     minor.ticks=FALSE,
     main="SOL Stocks Performance Based on Total
                               Returns and Price Returns 2019-20")
lines(SOL.Ret$cum.prc,
      type="l",
      lty=3)
abline(h=1,col="red")
legend("topleft",
       c("Value Based on Total Return",
         "Value Based on Price Return"),
       col=c("black","black"),
       lty=c(1,3))

#DOT

plot(DOT.Ret$cum.tot,
     type="l",
     auto.grid=FALSE,
     xlab="Date",
     ylab="Value of Investment ($)",
     ylim= c(0,2),
     minor.ticks=FALSE,
     main="DOT Stocks Performance Based on Total
                               Returns and Price Returns 2019-20")
lines(DOT.Ret$cum.prc,
      type="l",
      lty=3)
abline(h=1,col="red")
legend("topleft",
       c("Value Based on Total Return",
         "Value Based on Price Return"),
       col=c("black","black"),
       lty=c(1,3))
#Calculating Portfolio return

#Comparing Performance of Multiple
#Securities: Total Returns 

multi <- data.AAVE[,5]
multi <- merge(multi,data.LINK[,5])
multi <- merge(multi,data.ADA[,5])
multi <- merge(multi,data.BTC[,5])
multi <- merge(multi,data.SOL[,5])
multi <- merge(multi,data.DOT[,5])

multi[c(1:3,nrow(multi)),]

multi.df <- cbind(data.frame(index(multi)),
                  data.frame(multi))
names(multi.df) <-
  paste(c("date","AAVE","LINK","ADA","BTC","SOL","DOT"))
multi.df[c(1:3,nrow(multi.df)),]

multi.df$AAVE.idx <-
  multi.df$AAVE/multi.df$AAVE[1]
multi.df$LINK.idx <-
  multi.df$LINK/multi.df$LINK[1]
multi.df$ADA.idx <-
  multi.df$ADA/multi.df$ADA[1]
multi.df$BTC.idx <- multi.df$BTC/multi.df$BTC[1]
options(digits=5)
multi.df$SOL.idx <-
  multi.df$SOL/multi.df$SOL[1]
multi.df$DOT.idx <- multi.df$DOT/multi.df$DOT[1]
options(digits=5)

multi.df[c(1:3,nrow(multi.df)),6:9]


y.range <- range(multi.df[,6:9]) 
# find  minimum/maximum of all 6 securities
y.range

#Plot
plot(x=multi.df$date,
     y=multi.df$LINK.idx,
     ylim=y.range,
     type="l",
     xlab="Date",
     ylab="Value of Investment ($)",
     col="blue",
     lty=1,
     lwd=2,
     main="Value of $1 Investment in
                                       AAVE, BTC, ADA, and LINK")
lines(x=multi.df$date,
      y=multi.df$AAVE.idx,
      col="green",
      lty=1,
      lwd=2)
lines(x=multi.df$date,y=multi.df$BTC.idx,
      col="red",
      lty=1,
      lwd=2)
lines(x=multi.df$date,
      y=multi.df$ADA.idx,
      col="black",
      lty=1,
      lwd=2)
lines(x=multi.df$date,
      y=multi.df$SOL.idx,
      col="dark blue",
      lty=1,
      lwd=2)
lines(x=multi.df$date,
      y=multi.df$DOT.idx,
      col="dark red",
      lty=1,
      lwd=2)
abline(h=1,lty=1,col="black")
legend("topleft",
       c("AAVE","BTC","ADA","LINK","SOL","DOT"),
       col=c("green","red","gray","blue","dark blue","dark red"),
       lty=c(1,1,1,1,1,1),
       lwd=c(2,2,2,2,2,2))


#--------------------------------------------------
