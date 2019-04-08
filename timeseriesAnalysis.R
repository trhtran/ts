
library(ggplot2)
theme_set(theme_minimal())


library(xts)

MyData <- read.csv(file="timeseries.txt", header=TRUE, sep=",")

MyData_ts <- xts(MyData$PP, order.by = as.POSIXct(MyData$date))

plot(MyData_ts, main = "PP", xlab = "date", ylab = "$", col="blue")



components.ts = decompose(MyData_ts)



myts <- ts(MyData$PP, start=c(2011,01), frequency=12)

components.ts = decompose(myts)
plot(components.ts)

