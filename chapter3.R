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
