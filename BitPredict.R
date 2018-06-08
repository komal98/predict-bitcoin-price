library("anytime")
library("bsts")
library("car")
library("caret")
library("forecast")
library("keras")
library("MCMCpack")
library("smooth")
library("tensorflow")
library("tseries")
library("TTR")

#importing data
train <- read.csv(file.choose())
test <- read.csv(file.choose())
head(train)
testdata <- test[,2]

#Converting data for analysis
train$Date <- as.Date(anytime(train$Date))
test$Date <- as.Date(anytime(test$Date))
train$Volume <- gsub(",", "", train$Volume)
train$Market.Cap <- gsub(",", "", train$Market.Cap)
train$Market.Cap <- as.numeric(train$Market.Cap)
train$Volume <- as.numeric(train$Volume)

#Difference between high and low on each day
a <- matrix(c(0), nrow = 0, ncol = 1)
for(i in 1:nrow(train)){
  a <- rbind(a, train[i,3] - train[i,4])
  i <- i + 1
}
train <- cbind(train,a)


#Volume has missing values#
#Data Manipulation#
fifty_avg <- round(mean(train$Volume[train$a < 50], na.rm = TRUE), digits = 2)
hun_avg <- round(mean(train$Volume[train$a > 50 & train$a < 100], na.rm = TRUE), digits = 2)
hf_avg <- round(mean(train$Volume[train$a > 100 & train$a < 150], na.rm = TRUE), digits = 2)
th_avg <- round(mean(train$Volume[train$a > 150 & train$a < 350], na.rm = TRUE), digits = 2)
for(i in 1:nrow(train)){
  if(is.na(train[i,6])){
    if(train$a[i] < 50){
      train$Volume[i] <- fifty_avg
    } else if(train$a[i] < 100){
      train$Volume[i] <- hun_avg
    } else if(train$a[i] < 150){
      train$Volume[i] <- hf_avg
    } else if(train$a[i] < 350){
      train$Volume[i] <- th_avg
    }else
      print("Uncaught Title")
  }
}
train <- train[, - 8] #Removing column 8
ggplot(train, aes(Date, Close)) + geom_line() + scale_x_date("year") + ylim(0,10000) + ylab("Closing Price")

#Convert data set to time series
Train <- xts(train[, -1], order.by = as.POSIXct(train$Date)) 
tsr <- ts(Train[,4], frequency = 365.25,start = c(2013,4,27))
plot(Train$Close,type='l',lwd = 1.5,col='red', ylim = c(0,10000), main = "Bitcoin Closing Price")
#checking for trends and seasonality
dects <- decompose(tsr) #Obtaining the trends and seasonality
plot(dects)

#Holt's Forecasting
holtt <-  holt(Train[1289:1655,'Close'], type = "additive", damped = F) #holt forecast values
holtf <- forecast(holtt, h = 10)
holtdf <- as.data.frame(holtf)
plot(holtf, ylim = c(0,10000)) 
holtfdf <- cbind(test, holtdf[,1])
accuracy(holtdf[,1], testdata)
ggplot() + geom_line(data = holtfdf, aes(Date, holtfdf[,2]), color = "blue") + geom_line(data = holtfdf, aes(Date, holtfdf[,3]), color = "Dark Red")

#Exponential Triple Smoothing
ETS <- ets((Train[,'Close'])) # ETS forecast values
ETSf <- forecast(ETS, h = 10)
etsdf <- as.data.frame(ETSf)
plot(forecast(ETS, h = 10), ylim = c(0,10000)) #ETS forecast plot works perfectly
etsp <- predict(ETS, n.ahead = 10, prediction.interval = T, level = 0.95)
accuracy(etsdf[,1], testdata)

#ARIMA
tsdf <- diff(Train[,4], lag = 2)
tsdf <- tsdf[!is.na(tsdf)]
adf.test(tsdf)
plot(tsdf, type = 1, ylim = c(-1000, 1000))

#ACF AND PACF plots
acf(tsdf)
pacf(tsdf)

gege <- arima(Train[,4], order = c(4,2,11))
gegef <- as.data.frame(forecast(gege, h = 10))
accuracy(gegef[,1], testdata)
gegefct <- cbind(test, gegef[,1])
plot(forecast(gege, h = 10), ylim = c(0,10000))
ggplot() + geom_line(data = gegefct, aes(Date, gegefct[,2]), color = "blue") + geom_line(data = gegefct, aes(Date, gegefct[,3]), color = "Dark Red")

#Bayseian Regression
ss <- AddLocalLinearTrend(list(), Train[,4]) #Adding linear trend to model
ss <- AddSeasonal(ss, Train[,4], nseasons = 365.25) #Adding seasonal trend to model
model1 <- bsts(Train[,4],
               state.specification = ss,
               niter = 10)

plot(model1, ylim = c(0,10000)) #Plot based on bayesian regression of the model
pred1 <- predict(model1, horizon = 10)
plot(pred1, plot.original = 50,ylim = c(0,9000))
pred1$mean
accuracy(pred1$mean, testdata)

model2 <- bsts(Close ~ ., state.specification = ss,
               niter = 10,
               data = as.data.frame(Train))
model3 <- bsts(Close ~ ., state.specification = ss,
               niter = 10,
               data = as.data.frame(Train),
               expected.model.size = 10)
CompareBstsModels(list("Model 1" = model1, "Model 2" = model2, "Model 3" = model3), colors = c("blue", "red", "green"))
