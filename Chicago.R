setwd("~/")
data=read.csv("realest.csv",header=TRUE)
par(mfrow=c(1,2))
par(mar = c(4, 4, 2, 2))
library("car")
# Q-Q pplots
qqPlot(data$Price,ylab="sample quantiles",main="Price")
qqPlot(log(data$Price),ylab="sample quantiles",main="log(Price)")

# Scatter plots
plot(data$Tax,data$Price,xlab="Tax",ylab="Price",col="blue")
plot(log(data$Tax),log(data$Price), xlab="Log of Tax",ylab="Log of Price",col="blue")

# Model 1
M1=lm(Price ~ Bedroom + Space + Lot + Tax + 
        Bathroom + Garage + Condition, data=data)
summary(M1)

# Model 2
M2=lm(log(Price) ~ Bedroom + log(Space) + log(Lot) + log(Tax) + 
        Bathroom + Garage + Condition, data=data)
summary(M2)
