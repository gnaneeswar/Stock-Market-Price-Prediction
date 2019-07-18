
# Importing the required library and setting the working directory, reading data

library("readxl")
library(forecast)
library(astsa)
library(urca)
library(tseries)

setwd("C:\\Users\\Gnaneeswar\\Desktop\\Praxis\\Econometrics\\session3codesanddeck")
Stocks <- read_excel("StockPrice.xls")

# Visuvalizing the data
View(Stocks)

# Taking the close price alone and converting it into time series.
Stocks<-ts(Stocks[,2])
class(Stocks)

# Split into train and test
Stocks.train=ts(Stocks[1:200])
Stocks.test= ts(Stocks[201:248])

plot(stocks) #time series plot of stocks

# plotting a regression line on it.
abline(reg = lm(stocks~c(1:248))) # we can observe that the fit is poor

### Checking if the series stationary by KPSS and Unit Root Test

Stocks %>% ur.kpss() %>% summary()
adf.test(Stocks)

adf.test(diff(log10(Stocks.train),1))
adf.test(diff(Stocks.train,1))

par(mfrow = c(1,2))

acf(ts(Stocks.train),main='ACF before Difference')
pacf(ts(diff((Stocks.train),1)),main='PACF before  Difference')

acf(ts(diff((Stocks.train),1)),main='ACF after Difference')
pacf(ts(diff((Stocks.train),1)),main='PACF after Difference')

### making the series stationary

stocksdiff<- ts(diff(Stocks,1))

## Taking 16th and 19th lag

xlag16<-lag(stocksdiff,-16)
xlag19<-lag(stocksdiff,-19)

# creating a new data frame

yy=cbind(stocksdiff,xlag16,xlag19)
yy<-data.frame(yy)

# autoregression of lag 16 and lag 19

stocksdiff.ar3 <- lm(stocksdiff~xlag16+xlag19,na.action=na.omit,data=yy[1:200,])   # AR(1) regression for first differences
summary(stocksdiff.ar3)
plot(stocksdiff.ar3)

# Outcalc will hold the predicted price.
outcalc <-c(stocks[200])
count=2

# predicting differences and storing it in a variable out
out<-predict.lm(stocksdiff.ar3,yy[201:248,])

# predicting differences and storing it in outcalc
for (i in out)

{
  outcalc[count]<-outcalc[count-1]+i
  count=count+1
}

# creating a dummy variable dump and storing NA values to it

dump<- c()

for(i in 1:200)
{
  dump[i]<-NA
}

# appending dump and outcalc.
dump<-append(dump,outcalc[2:49])

#### auto arima on original Data Stocks

Stocks.train=ts(Stocks[1:205])
Stocks.test= ts(Stocks[206:248])

auto.arima(Stocks.train, trace = T, stepwise = FALSE, approximation = FALSE)
ARIMAfit = auto.arima(Stocks.train, approximation=FALSE,trace=FALSE)
ARIMAfit.forecast <- forecast(ARIMAfit, h = 43)
plot(ARIMAfit.forecast,ylim=c(40,65))
summary(ARIMAfit)

# ARIMA X

setwd("C:\\Users\\Gnaneeswar\\Desktop\\Praxis\\Econometrics\\session3codesanddeck")  #set the working directory
library(forecast)
require(forecast)

# engineered time series data was obtained by doing feature engineering on python (python code attached)

stocks <- read.csv("engineeredtimeseries.csv")

# data for all the variables are available from row number 61.
# so take data from row 61 till 200 of independent variable.
# Independent variable of row 61 will influence Close price of row 62.

stockmat<-matrix(c(stocks$close20[61:200],stocks$flag[61:200]),140,2,byrow=F)
fitx <- auto.arima(stocks$Close[62:201], xreg = stockmat)
summary(fitx)

stockmatfut<-matrix(c(stocks$close20[201:247],stocks$flag[201:247]),47,2,byrow=F)
fitx.forecast <- forecast(fitx, xreg = stockmatfut, h = 47)
fitx.forecast$upper[2:48]
plot(fitx.forecast,ylim=c(40,65))
autoplot(forecast::forecast(fitx.forecast))

# finding the rmse value

sqrt(sum((fitx.forecast$upper[1:47]-Stocks[202:248,"Close"])^2)/48)
sqrt(sum((fitx.forecast$mean[1:47]-Stocks[202:248,"Close"])^2)/48)

# Plotting the autoregression predictions

par(mfrow = c(1,1))
plot(Stocks[1:248]+1,type='l',xlim=c(0,248),ylim=c(40,75),col="darkorange")
par(new=T)
plot(Stocks.train,type='l',xlim=c(0,248),ylim=c(40,65),col="royalblue")
par(new=T)
plot(dump,type='l',ylim=c(43,60),xlim=c(1,255),col="yellowgreen")

# Auto Airma fit

Stocks.train=ts(Stocks[1:205])
Stocks.test= ts(Stocks[206:248])

plot(Stocks[1:248]+1,type='l',xlim=c(0,248),ylim=c(40,75),col="darkorange")
par(new=T)
plot(Stocks.train,type='l',xlim=c(0,248),ylim=c(40,65),col="royalblue")

pred1 = predict(ARIMAfit, n.ahead =43)
lines(pred1$pred,col='yellowgreen')
lines(pred1$pred+pred1$se,col='brown')
lines(pred1$pred-pred1$se,col='brown')

### Residual Plot against fit and pacf and acf of residual for autoregressive fit

# RMSE Value
sqrt(sum((dump[201:248]-stocks[201:248])^2)/48)

# Residual vs Fit plot
plot(dump[201:248],((dump[201:248]-stocks[201:248])),col="indianred")

# plot, ACF and PACF of residuals vs time
plot(((dump[201:248]-stocks[201:248])))
acf(((dump[201:248]-stocks[201:248])))
pacf(((dump[201:248]-stocks[201:248])))
qqnorm(((dump[201:248]-stocks[201:248])))
hist(((dump[201:248]-stocks[201:248])),10)

### Random Walk model with drift

rw<-rwf(Stocks.train, h = 48, drift = TRUE, fan = FALSE,lambda = NULL, biasadj = FALSE,ylim=c(40,75))
plot(rw)
rw.forecast <- forecast(rw,h = 47)
autoplot(forecast::forecast(rw.forecast))

# RMSE value
sqrt(sum((rw$mean[1:48]-Stocks[201:248,"Close"])^2)/48)

# Summary of fit
summary(rw)


