source("E:/Documents/Practice/TsayTS/basiccode.R")

library(fBasics)
library(tseries)

data <- read.table("m-ibm3dx2608.txt",header = T)

sbim <- data[,2]

adf.test(sbim)

Box.test(sbim,lag=5,typ="Ljung")

libm <- log(sbim+1)

adf.test(libm)

Box.test(libm,lag=5,typ="Ljung")

############

gnp <- scan(file="dgnp82.txt")

gnp1 <- ts(gnp,frequency = 4, start = c(1947,2))

plot(gnp1)

m1 <- ar(gnp,method="mle")

m1$order

m2 <- arima(gnp,order=c(3,0,0))

print(m2)

constant <- (1-0.348-0.1793+.1423)*0.077   # This is for the stationary process

sqrt(m2$sigma2)  ## residual std

p1 <- c(1,-m2$coef[1:3])

polyroot(p1)

Mod(polyroot(p1))

cycle <- 2*pi/acos(1.590253/1.913308)

################

vw <- read.table('m-ibm3dx2608.txt',header = T)[,3]

m3 <- arima(vw,order=c(3,0,0))

m3

(1-.1158+0.0187+0.1042)*mean(vw)  # intercept  phi0

Box.test(m3$residuals,lag=12,type = "Ljung")

pv <- 1-pchisq(16.35,9)

m4 <- arima(vw,order=c(3,0,0), fixed=c(NA,0,NA,NA))

print(m4)

(1-.1136+0.1063)*mean(vw)  # intercept

Box.test(m4$residuals,lag=12,type = "Ljung")

pv <- 1-pchisq(16.828,10)

### intercept in the AR models built in R give the mean and not the actual phi

library(portes)

data("CRSP")

m3 <- arima(CRSP,order=c(3,0,0))

print(m3)

sqrt(m3$sigma2)

m4 <- arima(CRSP,order=c(3,0,0), fixed=c(NA,0,NA,NA))

print(m4)

prediction <- predict(m4,n.ahead = 12)

m4 <- arima(CRSP,order=c(0,0,9), fixed=c(NA,0,NA,0,0,0,0,0,NA,NA))

print(m4)

Box.test(m4$residuals,lag=12,type = "Ljung")

1 - pchisq(10.507,9)

#############

library(TSA)

eacfval <- eacf(CRSP)

eacfval$eacf

threemdata <- read.table("d-mmm.dat")[,1]

eacf(threemdata)

pacf(threemdata)

##############

library(fUnitRoots)

da <- read.table("q-gdp4708.txt",header = T)

gdp <- log(da[,4])

m1 <- ar(diff(gdp), method = "mle")

m1$order

adfTest(gdp,lags = 10,type="ct")

acf(gdp)

da <- read.table("d-sp55008.txt",header = T)

sp5 <- log(da[,7])

m2 <- ar(diff(sp5),method="mle")

adf_lag <- adfTest(sp5,lags=2,type="ct")

adfTest(sp5,lags=15,type="ct")

#####

da <- read.table("q-jnj.txt",header = T)

acf(da)

acf(diff(da$X.71))

plot(diff(da$X.71),type='l')

plot(diff(diff(da$X.71),4),type='l')
