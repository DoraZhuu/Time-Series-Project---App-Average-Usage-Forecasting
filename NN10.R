#### Read the data, which contain weekly log return of SP500 index and 10 stocks
load("/Users/kouekiho/Code/R /STA 4003 Project/StockData.RData")
library(caret)
library(neuralnet)
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
Train_set <- function(i, h){
  Stock <- StockData[,i]
  slice <- createTimeSlices(Stock, initialWindow = 10 + h, horizon = 0)
  idx <- data.frame(matrix(unlist(slice[[1]][1:(199 - h)]), ncol = 10 + h, byrow = TRUE))
  idd <- matrix(nrow = 199 - h, ncol = 10 + h)
  for (i in 1 : (199 - h)){
    for (j in 1 : (10 + h)){
      idd[i, j] <- Stock[idx[i, j]]
    }
  }
  colnames(idd) <- colnames(idx) 
  idd
}

Pred_set <- function(i, h){
  Stock <- StockData[,i]
  slice <- createTimeSlices(Stock, initialWindow = 10 + h, horizon = 0)
  idy <- data.frame(matrix(unlist(slice[[1]][199:250]), ncol = 10 + h, byrow = TRUE))
  idt <- matrix(nrow = (53 - h), ncol = 10 + h)
  for (i in 1:(53 - h)){
    for (j in 1:(10 + h)){
      idt[i,j] <- Stock[idy[i,j]]
    }
  }
  colnames(idt) <- colnames(idy)
  idt
}

set.seed(10002)
for (j in 2:p)
{
  Stock  <- StockData[,j]
  
  net1 <- neuralnet(X11 ~ ., data = Train_set(j,1), hidden = 3, learningrate = 0.01, stepmax = 1e+06)
  net2 <- neuralnet(X12 ~ .-X11, data = Train_set(j,2), hidden = 3, learningrate = 0.01, stepmax = 1e+06)
  net3 <- neuralnet(X13 ~ .-X12-X11, data = Train_set(j,3), hidden = 3, learningrate = 0.01, stepmax = 1e+06)
  
  step1.forecasts <- predict(net1, newdata = Pred_set(j, 1))
  step2.forecasts <- predict(net2, newdata = Pred_set(j, 2))
  step3.forecasts <- predict(net3, newdata = Pred_set(j, 3))
  
  
  MSE.Stock[1, (j-1)] <- mean((Stock[index.test.Step1]- step1.forecasts)^2)
  MSE.Stock[2, (j-1)] <- mean((Stock[index.test.Step2]- step2.forecasts)^2)
  MSE.Stock[3, (j-1)] <- mean((Stock[index.test.Step3]- step3.forecasts)^2)
  
}


######## Your code for Trading stratedy
### Example: My trading strategy is buy and sell each stock with same weight
return.stock <- rowMeans(StockData[(n1+1):(n1+n2),2:p])



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
