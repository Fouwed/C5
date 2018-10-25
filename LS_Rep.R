library(urca)
library(vars)


#DATA
  DebtToNw <- read.csv("Z1_NFCBusiness_creditMarket_Debt_asPercentageof_NetWorth.csv",
                       head = TRUE, sep=",")
  DebtToEq <- read.csv("Z1_NFCBusiness_CreditMarketDebtAsPercentageOfMarketValueOfCorpEquities.csv", 
                       head = TRUE, sep=",")
  
  dbtnw <- ts(DebtToNw$NCBCMDPNWMV, start = c(1951,4),end = c(2016,4),frequency = 4)
  dbteq <- ts(DebtToEq$NCBCMDPMVCE, start = c(1951,4),end = c(2016,3),frequency = 4)


##LOG-rgdp
  myvar <- data.frame(growth=LogRgdp, debt=dbteq)
    ##LEVEL-rgdp
      #myvar <- data.frame(growth=window(gdpts,start=c(1951,4),end=c(2016,3)), debt=dnw)        

    
## Choose optimal length for unrestricted VAR
  VARselect(myvar, lag.max = 6, type = "both")
    # SC  --> 2 lags
    # AIC & FPE & HQ --> 3 lags

  varALL <- VAR(myvar, p = 2, type = "both")
  varALL <- VAR(myvar, p = 3, type = "both")
  
#1952-->2016
print(causality(varALL, cause="growth"))
print(causality(varALL, cause="debt"))
# CAUSALITY runs from Growth --> Debt (2%)
# NOT the other way round
# CONTRADICT Lavoie & Secca results
#1983-->2016
##OLD  datapostBP <- data.frame(gpostBP=vniveaupostBpt[,1], dpostBP=vniveaupostBpt[,2])
datapostBP <- data.frame(gpostBP=window(gdpts,start=1983,end=2016), dpostBP=vniveaupostBpt[,2])
VARselect(datapostBP, lag.max = 6, type = "both")
# SC --> 2 lags
varpostBP <- VAR(datapostBP, p = 2, type = "both")
roots(varpostBP)
print(causality(varpostBP, cause="gpostBP"))
print(causality(varpostBP, cause="dpostBP"))
# BINGO : causality from G to D is confirmed (1%)
# instantaneou causality at 2%



# let's try & REPLICATE L&S causality test
# for period: 1962 - 1998
# NOTE : I SHOULD replicate this on CANADA
# butall i habe is USA data
myvarLS <- window(myvar,start=c(1962,1),end=c(1998,4))

gLS<-window(LogRgdp,start=1962,end=1998)
dLS<-window(dbteq,start=1962,end=1998)
myvarLS<-cbind(gLS,dLS)
VARselect(myvarLS, lag.max = 6, type = "both")
# SC  --> 1 lags
# AIC & FPE & HQ --> 3 lags

varLS <- VAR(myvarLS, p = 3, type = "b")
print(causality(varLS, cause="gLS"))
print(causality(varLS, cause="dLS"))
# L&Secca results are partially confirmed :
# no causality from G --> D
# nor the o-w-round
# not simulataneous causality

# REMEMBER AND TRY CANADA DATA !!!