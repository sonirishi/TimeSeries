source("E:/Documents/Practice/TsayTS/basiccode.R")

library(fBasics)
library(tseries)

da <- read.table("m-intc7308.txt",header = T)

intc <- log(da[,2]+1)  ## log of the series

acf(intc)   ## no autocorrelation

Box.test(intc,lag=5,typ="Ljung")

archTest(intc)

Box.test(intc^2,lag=5,typ="Ljung")

#################

da <- read.table("m-intc7308.txt",header = T)

intc <- log(da[,2]+1)

library(fGarch)

model_arch <- fGarch::garchFit(~garch(3,0),data = intc)

summary(model_arch)

std_residuals <- model_arch@residuals/model_arch@sigma.t

Box.test(std_residuals,type="Ljung")

model_arch_1 <- fGarch::garchFit(~garch(1,0),data = intc)

summary(model_arch_1)

model_arch_2 <- fGarch::garchFit(~garch(1,0),data = intc, cond.dist = "std")

summary(model_arch_2)

#####

exchanges <- read.table("forex-c.dat")

colnames(exchanges) <- c("cad","germ","pound","yen","franc")

model_arch_3 <- fGarch::garchFit(~garch(1,0),data = exchanges[,2], trace = F)

summary(model_arch_3)

pacf(exchanges[,2]^2)

############

sp <- read.table("sp500.dat",header = T)

acf(sp)

pacf(sp^2)

model_garch <- garchFit(~ arma(3,0) + garch(1,1), data = sp, trace = F)

summary(model_garch)

model_garch <- garchFit(~ garch(1,1), data = sp, trace = F)

summary(model_garch)

resid <- model_garch@residuals/model_garch@sigma.t

acf(resid)

acf(resid^2)

Box.test(resid,lag=12,type="Ljung")

Box.test(resid,lag=24,type="Ljung")   ## Stationarity test

predict(model_garch,5)

##############

model_garch <- garchFit(~ garch(1,1), data = sp, trace = F, cond.dist = "std")

summary(model_garch)

resid <- model_garch@residuals/model_garch@sigma.t

acf(resid)

acf(resid^2)

Box.test(resid,lag=12,type="Ljung")

Box.test(resid,lag=24,type="Ljung")   ## Stationarity test

predict(model_garch,5)

############

library(rugarch)

ewma_spec = ugarchspec(variance.model=list(model="iGARCH", garchOrder=c(1,1)), 
                       mean.model=list(armaOrder=c(0,0), include.mean=TRUE),  
                       distribution.model="norm")

ugfit = ugarchfit(spec = ewma_spec, data = sp)

print(ugfit)

source("garchm.R")

garchm <- garchM(sp$X0.0225)
