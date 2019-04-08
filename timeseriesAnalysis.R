
library(ggplot2)
theme_set(theme_minimal())


#library(xts)

MyData <- read.csv(file="timeseries.txt", header=TRUE, sep=",")

#MyData_ts <- xts(MyData$PP, order.by = as.POSIXct(MyData$date))

#plot(MyData_ts, main = "PP", xlab = "date", ylab = "$", col="blue")
#components.ts = decompose(MyData_ts)



myts <- ts(MyData$PP, start=c(2011,01), frequency=12)

timeseriescomponents = decompose(myts)
plot(components.ts)
plot(timeseriescomponents)

library("fUnitRoots")

# https://datascienceplus.com/time-series-analysis-using-arima-model-in-r/
  

urkpssTest(myts, type = c("tau"), lags = c("short"),use.lag = NULL, doplot = TRUE)
tsstationary = diff(myts, differences=1)
plot(tsstationary)

acf(MyData_ts,lag.max=34) 


timeseriesseasonallyadjusted <- myts - timeseriescomponents$seasonal
tsstationary <- diff(timeseriesseasonallyadjusted, differences=1)


acf(tsstationary, lag.max=34)
pacf(tsstationary, lag.max=34)


fitARIMA <- arima(myts, order=c(1,1,1),seasonal = list(order = c(1,0,0), period = 12),method="ML")
library(lmtest)
coeftest(fitARIMA) 


acf(fitARIMA$residuals)
library(FitAR)
boxresult-LjungBoxTest (fitARIMA$residuals,k=2,StartLag=1)
plot(boxresult[,3],main= "Ljung-Box Q Test", ylab= "P-values", xlab= "Lag")
qqnorm(fitARIMA$residuals)
qqline(fitARIMA$residuals)


# Forecast
auto.arima(myts, trace=TRUE) 

predict(fitARIMA,n.ahead = 12)

library(forecast)

futurVal <- forecast(fitARIMA,h=10, level=c(99.5))
plot(futurVal)
