#### Read the data, which contain weekly log return of SP500 index and 10 stocks
load("StockData.RData")
library(forecast)
library(tseries)
library(fUnitRoots)
# Dataset "StockData.test" will be used for grading. 
# Don't delete the next two comments
#load("StockData.test.Rdata")
#StockData <-StockData.test

n1 <- 208 #the length of train set
n2 <- 52  # the length of test set
p <- dim(StockData)[2]  # The first column is the log return for SP500

# Testing periods for recording your forecasts
index.test.Step1 <- (n1+1):(n1+n2)
index.test.Step2 <- (n1+2):(n1+n2)
index.test.Step3 <- (n1+3):(n1+n2)

# Variables for mean squared errors
MSE <- rep(0,3) # The mean squared errors for 1-Step, 2-Step, 3-Step forecasts
MSE.Stock <- matrix(rep(0,(p-1)*3),3,(p-1)) # MSE for each stock

########## Your code to build the time series model  

muldiff <- function(x,i){
    if (i == 0)(x) else
    for (j in 1:i){
        x <- diff(x)
    }
    x
}

find_d <- function(x){
    # tracing back from d = 10
    i <- 10
    while (i >= 0){
        if (pp.test(muldiff(x,i))$p.value < 0.05)
            i <- i - 1
        else
            break
    }
    i + 1
}

own_arima <- function(x){
    aic <- matrix(rep(0,16),4,4)
    d <- find_d(x)
    for (p in 0:3){
        for (q in 0:3){
            aic[p+1, q+1] <- arima(x, order = c(p,d,q), method = "ML")$aic
        }
    }
    if (which.min(aic)%%4 == 0)
        {
        p <- which.min(aic)/4 - 1
        q <- 3
        }else
    {
        p <- which.min(aic)%/%4
        q <- which.min(aic)%%4 - 1
    }
    m <- Arima(x, order = c(p,d,q), method = "ML")
    list(m, c(p, d, q))
}

for (j in 2:p)
{
  Stock  <- StockData[,j]
  step1.forecasts <- rep(0,n2);
  step2.forecasts <- rep(0,n2);
  step3.forecasts <- rep(0,n2);
  
  for (i in 1:n2)
  {
    index.train <- i:(i+n1-1)  # Moving training windows
    m <- own_arima(StockData[,j][index.train])
    model <- m[[1]]
    step1.forecasts[i] <- unlist(forecast(model, h = 1)[4])[1]
    step2.forecasts[i] <- unlist(forecast(model, h = 2)[4])[2]
    step3.forecasts[i] <- unlist(forecast(model, h = 3)[4])[3]
  }
  
  MSE.Stock[1,(j-1)] <- sum(( Stock[index.test.Step1]- step1.forecasts)^2)
  MSE.Stock[2,(j-1)] <- sum(( Stock[index.test.Step2]- step2.forecasts[1:(n2-1)])^2)
  MSE.Stock[3,(j-1)] <- sum(( Stock[index.test.Step3]- step3.forecasts[1:(n2-2)])^2)
}


######## Your code for Trading stratedy
### Example: My trading strategy is buy and sell each stock with same weight

W <- NULL
for (i in 208:259){ 
    W <- rbind(W, portfolio.optim(StockData[(i-24):i, 2:11])$pw)
}
return.stock <- rowSums(StockData[209:260, 2:11] * W)

######### Output. Don't change any codes below.
MSE <- rowMeans(MSE.Stock)
Ex.Return <- return.stock - StockData[(n1+1):(n1+n2),1] # excessive returns over SP500
Total.Return <- sum(Ex.Return)
Var.Return <- var(Ex.Return)
Ratio.Return <- mean(Ex.Return)/sqrt(Var.Return)

print(MSE)
print(Total.Return)
print(Var.Return)
print(Ratio.Return)
