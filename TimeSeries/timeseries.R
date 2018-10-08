set.seed(9571)
white_noise <- rnorm(100, mean = 0, sd = 3.0)
class(white_noise)
summary(white_noise)
install.packages("forecast")
library(forecast)
acf(white_noise)

acf(white_noise,plot = FALSE)
#2/sqrt(100)
#2/sqrt(200)

library(tseries)
adf.test(white_noise)
#y1<- diff(white_noise,1)
#y2<-diff(white_noise,2)
#y3<-diff(white_noise,28)
#adf.test(y3) #augmented dicky fuller test to check stationarity
set.seed(874234242)
no_stat <- cumsum(rnorm(100, mean = 0, sd = 3.0))
acf(no_stat)   #autocor func
pacf(no_stat)   # partial autocor func
acf(diff(no_stat))
adf.test(no_stat)
adf.test(diff(no_stat))

#Earth Quakes in Greeze over 2000-2008 Magnitude is > 4

install.packages("RCurl")
library(RCurl)
seismic_raw1 <- read.table( textConnection
                            (getURL( "http://www.geophysics.geol.uoa.gr/catalog/catwo_20002008.epi")),
                            sep = "", header = F)
seismic_raw1bk<-seismic_raw1
dim(seismic_raw1bk)
tail(seismic_raw1bk)
names(seismic_raw1) <- c("date", "mo", "day", "hr", "mn", "sec", "lat", "long", "depth", "mw") 
edit(seismic_raw1)
names(seismic_raw1)
adf.test(seismic_raw1$date)
dim(seismic_raw1)
head(seismic_raw1, n = 3)

library("plyr")
seismic1 <- count(seismic_raw1, c("date", "mo"))
seismic_ts1 <- ts(seismic1$freq, start = c(2000, 1),
                  end = c(2008, 1), frequency = 12)
plot(seismic_ts1,ylab="no of earthquakes")

d <- 0 : 2
p <- 0 : 6
q <- 0 : 6

#expand.grid() function, which is a very useful function
#that will create a data frame with all the possible
#combination of these parameters:

seismic_models1 <- expand.grid(d = d, p = p, q = q)
dim(seismic_models1)
head(seismic_models1, n = 4)

getTSModelAIC <- function(ts_data, p, d, q) {
  ts_model <- arima(ts_data, order = c(p, d, q))
  return(ts_model$aic)
}

#For certain combinations of order parameters, our function will produce an error
#if it fails to converge. When this happens, we'll want to 
#report a value of infinity for the AIC value so that this model 
#is not chosen when we try to pick the best model.

getTSModelAICSafe <- function(ts_data, p, d, q) {
  result = tryCatch({
    getTSModelAIC(ts_data, p, d, q)
  }, error = function(e) {
    Inf
  })
}

seismic_models1$aic <- mapply(function(x, y, z)
  getTSModelAICSafe(seismic_ts1, x, y, z), seismic_models1$p,
  seismic_models1$d, seismic_models1$q)

subset(seismic_models1,aic == min(aic))

seismic_model1 <- arima(seismic_ts1, order = c(1, 1, 1))
summary(seismic_model1)

plot(forecast(seismic_model1, 10))
