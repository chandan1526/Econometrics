#######################################Imported modefied dataset######################################################
#######Modefication(Imputation of all required data) has been take care of through Excel############################
StockPrice <- read_excel("C:/Users/AINI/Downloads/StockPrice.xls", 
                         +     sheet = "Modified data")
d=StockPrice
library(forecast)
d=d$Close
df=ts(d,frequency = 52)
plot(df)
df_decompose=decompose(df)
plot(df_decompose)

#######################################Moving Average################################################
#Simple Moving Average
library(TTR)

S=SMA(df,3)
SM=forecast(S,36)
plot(SM,ylab = "Price",main = "Simple Moving Average for 3")

summary(SM)

S=SMA(df,7)
SM=forecast(S,36)
plot(SM,ylab = "Price",main = "Simple Moving Average for 7")

#Exponential Moving Avg
E=EMA(df,7)
EM=forecast(E,36)
plot(forecast(E,36),ylab = "Price",main = "Exponential Moving Average")

############################################Naives######################################################
install.packages('fpp')
library(fpp)

n=naive(df,h=7)
nv=forecast(n,36)
plot(forecast(n,36),ylab = "Price",main = "Naives")

n=snaive(df,h=7)
nv=forecast(n,36)
plot(forecast(n,36),ylab = "Price",main = "SeasonalNaive")
summary(nv)

n=meanf(df,h=7)
nv=forecast(n,36)
plot(forecast(n,36),ylab = "Price",main = "Naives")


########################################Arima################################################
#############################################################################################



library(tseries)
adf.test(df)

tsdisplay(StockPrice)

StockPrice=df

auto.arima(df)

auto.arima(df, trace = T)



####### Checking the stationary of data  ###################




plot(df,ylab='Price')
plot(diff(df),ylab='Price') # checking the difference of data to get the stationary
adf.test(diff(df))



par(mfrow = c(1,2))
acf((df),main='ACF Price')
pacf((df),main='PACF Price')


par(mfrow = c(1,2))
acf(diff(df),main='ACF Price')
pacf(diff(df),main='PACF Price')



#############################################Arima Model############################################

ARIMAfit = auto.arima(df)

ARIMAfit

 

summary(ARIMAfit)


par(mfrow = c(1,1))
pred = predict(ARIMAfit, n.ahead = 36)
pred




Box.test(ARIMAfit$residuals, lag = 5, type = "Ljung-Box")


forecast_price <- forecast(ARIMAfit, h = 36)

plot(forecast_price)

forecast_price$mean

forecast_price$lower
# 


par(mfrow=c(1,3))
plot.ts(ARIMAfit$residuals)
acf(ts(ARIMAfit$residuals),main='ACF Residual')
pacf(ts(ARIMAfit$residuals),main='PACF Residual')

par(mfrow=c(1,1))


checkresiduals(ARIMAfit)

########################################Holtwinter################################################
##################################################################################################
# Exponential smoothing with holt

holttrend = holt(df, h = 36)
plot(df)
summary(holttrend)
plot(holttrend)
plot(nv)

###################################################################################################
## Exponential Smoothing with ets
# Auto gemerated
ets(df)
# Forecast plot
d = ets(df)
summary(d)
plot(forecast(d, h = 36))


##################################Comparison with Arima and Hotwinter#######################################################
library(ggplot2)

autoplot(df) +
  forecast::autolayer(holttrend$mean, series = "Holt Linear Trend") +
  forecast::autolayer(forecast_price$mean, series = "ARIMA") +
  xlab("year") + ylab("Price") + 
  guides(colour=guide_legend(title="Forecast Method")) + theme(legend.position = c(0.8, 0.2)) +
  ggtitle("StockPrice") + theme(plot.title=element_text(family="Times", hjust = 0.5, color = "blue",
                                                      face="bold", size=15))

# Models
holttrend = holt(df, h = 36)
plot(holttrend)
forecast_price = forecast(forecast(ARIMAfit), h = 36)

autoplot(df) + geom_line(size=2) +
  forecast::autolayer(holttrend$fitted, series = "Holt Linear Trend", size = 1.1) +
  forecast::autolayer(forecast_price$fitted, series = "ARIMA", size = 1.1) +
  xlab("year") + ylab("Price") + 
  guides(colour=guide_legend(title="Forecast Method")) + theme(legend.position = c(0.8, 0.2)) +
  ggtitle("Stock Price") + theme(plot.title=element_text(family="Times", hjust = 0.5, 
                                                      color = "blue", face="bold", size=15))


