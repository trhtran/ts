
library(ggplot2)
library(lmtest)
library("fUnitRoots")
library(FitAR)
library(forecast)
library(Metrics)


#theme_set(theme_minimal())


#library(xts)

MyData <- read.csv(file="timeseries.txt", header=TRUE, sep=",")
nrow(MyData)
train <- MyData[1:84,]
valid <- MyData[85:nrow(MyData),]


train_ts <- ts(train$PP, start=c(2011,01), frequency=12)
valid_ts <- ts(valid$PP, start=c(2018,01), frequency=12)

timeseriescomponents = decompose(train_ts)

plot(timeseriescomponents)



# https://datascienceplus.com/time-series-analysis-using-arima-model-in-r/
# https://github.com/SubhasreeUC/Master/blob/master/TimeSeriesExample.R  

urkpssTest(train_ts, type = c("tau"), lags = c("short"),use.lag = NULL, doplot = TRUE)
tsstationary = diff(train_ts, differences=1)
plot(tsstationary)

acf(train_ts,lag.max=24) 


timeseriesseasonallyadjusted <- train_ts - timeseriescomponents$seasonal
tsstationary <- diff(timeseriesseasonallyadjusted, differences=1)


acf(tsstationary, lag.max=24)
pacf(tsstationary, lag.max=24)


#fitARIMA <- arima(train_ts, order=c(1,1,1),seasonal = list(order = c(1,0,0), period = 12),method="ML")
fitARIMA <- arima(tsstationary, order=c(1,1,1),seasonal = list(order = c(1,0,0), period = 12),method="CSS")

#fitARIMA <- arima(train_ts, order = c(0,1,1))


coeftest(fitARIMA) 


acf(fitARIMA$residuals)

boxresult <- LjungBoxTest (fitARIMA$residuals,k=2,StartLag=1)
plot(boxresult[,3],main= "Ljung-Box Q Test", ylab= "P-values", xlab= "Lag")
qqnorm(fitARIMA$residuals)
qqline(fitARIMA$residuals)


prediction <- predict(fitARIMA,12)

prediction

model = auto.arima(tsstationary, trace=TRUE) 
#fitARIMA <- auto.arima(myts,max.p = 5,max.q = 5,max.P = 5,max.Q = 5,max.d = 3,seasonal = TRUE,ic = 'aicc')






# Forecast

futurVal <- forecast(model,h=12, level=c(99.5))
plot(futurVal)
plot(valid)

rmse(valid$PP, prediction$pred)

