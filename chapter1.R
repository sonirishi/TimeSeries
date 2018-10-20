source("E:/Documents/Practice/TsayTS/basiccode.R")

library(fBasics)

da <- read.table("d-ibm3dx7008.txt",header = T)

ibm <- as.numeric(as.character(da[,2]))

sibm <- ibm*100

basicStats(sibm)

s1 <- skewness(sibm)

ttest_skew <- s1/sqrt(6/length(sibm))

2*(1 - pnorm(ttest_skew))  ## P value

libm <- log(ibm+1)*100

t.test(libm)

normalTest(libm,method = 'jb')
