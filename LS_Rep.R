library(urca)
library(vars)


### CANADA ####

cang <- read.csv("Can_GROWTH.csv",head = TRUE, sep=",")
candteq <- read.csv("Canada_dteq.csv",head = TRUE, sep=",")

cdLS <- ts(candteq$DTEQ, start = c(1990,1),frequency = 4)
cdhLS <- ts(candteq$DTEQHC, start = c(1990,1),frequency = 4)
cgLS <- ts(cang$CGROWTH, start = c(1990,1),frequency = 4)

ts.plot(cdLS,cdhLS)
ts.plot(cgLS)

myvarCANLS<-cbind(cgLS,cdLS)

VARselect(myvarCANLS, lag.max = 6, type = "t")

varLS <- VAR(myvarLS, p = 3, type = "t")
print(causality(varLS, cause="cgLS"))
print(causality(varLS, cause="cdLS"))


# U.S ---------------------------------------------------------------------


#US DATA ###
  DebtToNw <- read.csv("Z1_NFCBusiness_creditMarket_Debt_asPercentageof_NetWorth.csv",
                       head = TRUE, sep=",")
  DebtToEq <- read.csv("Z1_NFCBusiness_CreditMarketDebtAsPercentageOfMarketValueOfCorpEquities.csv", 
                       head = TRUE, sep=",")
  DebtToNwHIST <- read.csv("dnw_HIST.csv",head = TRUE, sep=",")
  RGDP <- read.csv("Z1_RGDP_3Dec_GDPC96.csv", head = TRUE, sep=",")
  
 
  
  dbtnw <- ts(DebtToNw$NCBCMDPNWMV, start = c(1951,4),end = c(2016,4),frequency = 4)
  dbtnwH <- ts(DebtToNwHIST$NCBCMDPNWHC, start = c(1951,4),end = c(2016,4),frequency = 4)
  dbteq <- ts(DebtToEq$NCBCMDPMVCE, start = c(1951,4),end = c(2016,3),frequency = 4)
  gdpts <- ts(RGDP$GDPC96, start = c(1947,1),frequency = 4)
  G_RATE <- ((diff(gdpts, lag=1, differences = 1)/lag(gdpts,-1)))
  LogRgdp <- log(gdpts)

  ts.plot(dbteq,dbtnw,dbtnwH)
  ts.plot(G_RATE)
  ts.plot(LogRgdp)
  
##2 ALTERNATIVES :  1-debt to EQUITIES
##                  2-debt to NETWORTH                  

  ##debt to EQUITIES
    ##2 ALTERNATIVES /  1- rgdp GROWTH   2- LOG rgdp (apendix)  
      #1- GROWTH
        myvar <- data.frame(growth=window(G_RATE,start=c(1951,4),end=c(2016,3)),
                              debt=window(dbteq,start=c(1951,4),end=c(2016,3)))
        myvarLS <- data.frame(growth=window(G_RATE,start=c(1962,1),end=c(1998,4)),
                            debt=window(dbteq,start=c(1962,1),end=c(1998,4)))
      #2- LOGRGDP (Appendix)
        myvar <- data.frame(growth=window(LogRgdp,start=c(1951,4),end=c(2016,3)),
                            debt=window(dbteq,start=c(1951,4),end=c(2016,3)))
        myvarLS <- data.frame(growth=window(LogRgdp,start=c(1962,1),end=c(1998,4)),
                              debt=window(dbteq,start=c(1962,1),end=c(1998,4)))
        
      ## Choose optimal length for unrestricted VAR
      VARselect(myvar, lag.max = 6, type = "none")
      VARselect(myvar, lag.max = 6, type = "const")
      VARselect(myvar, lag.max = 6, type = "trend")
        # SC  --> 2 lags
        # AIC & FPE & HQ --> 3 lags
    
      varALL <- VAR(myvar, p = 2, type = "none")
      varALL <- VAR(myvar, p = 2, type = "trend")
      varALL <- VAR(myvar, p = 3, type = "none")
      varALL <- VAR(myvar, p = 3, type = "constant")
      
    #1952-->2016
      print(causality(varALL, cause="growth"))
      print(causality(varALL, cause="debt"))
      # CAUSALITY runs from Growth --> Debt (2%)
      # NOT the other way round
      # CONTRADICT Lavoie & Secca results
      #1983-->2016
                           
    #1962 - 1998 to try & REPLICATE L&S causality test
      VARselect(myvarLS, lag.max = 6, type = "none")
      # SC  --> 2 lags
      # AIC & FPE & HQ --> 3 lags
      
      varLS <- VAR(myvarLS, p = 2, type = "none")
      varLS <- VAR(myvarLS, p = 3, type = "none")
  
      print(causality(varLS, cause="growth"))
      print(causality(varLS, cause="debt"))
      # 
 
##debt to NETWORTH at HISTORICAL VALUE
  
      ##2 ALTERNATIVES /  1- rgdp GROWTH   2- LOG rgdp (apendix)  
      #1- GROWTH
      myvar <- data.frame(growth=window(G_RATE,start=c(1951,4),end=c(2016,3)),
                          debt=window(dbtnwH,start=c(1951,4),end=c(2016,3)))
      myvarLS <- data.frame(growth=window(G_RATE,start=c(1962,1),end=c(1998,1)),
                            debt=window(dbtnwH,start=c(1962,1),end=c(1998,1)))
      #2- LOGRGDP (Appendix)
      myvar <- data.frame(growth=window(LogRgdp,start=c(1951,4),end=c(2016,3)),
                          debt=window(dbtnwH,start=c(1951,4),end=c(2016,3)))
      myvarLS <- data.frame(growth=window(LogRgdp,start=c(1962,1),end=c(1998,4)),
                            debt=window(dbtnwH,start=c(1962,1),end=c(1998,4)))
      
      ## Choose optimal length for unrestricted VAR
      VARselect(myvar, lag.max = 6, type = "none")
      VARselect(myvar, lag.max = 6, type = "const")
      VARselect(myvar, lag.max = 6, type = "trend")
      # SC  --> 1 lags
      # AIC & FPE & HQ --> 3 lags
      
      varALL <- VAR(myvar, p = 3, type = "const")
      varALL <- VAR(myvar, p = 3, type = "both")
      
      #1952-->2016
      print(causality(varALL, cause="growth"))
      print(causality(varALL, cause="debt"))
      # CAUSALITY runs from Growth --> Debt (1%)
      # NOT the other way round
      # CONTRADICT Lavoie & Secca results
      #1983-->2016
      
      #1962 - 1998 to try & REPLICATE L&S causality test
      VARselect(myvarLS, lag.max = 6, type = "t")
      # SC & HQ & AIC & FPE   --> 1 lags
      
      varLS <- VAR(myvarLS, p = 1, type = "t")
      varLS <- VAR(myvarLS, p = 1, type = "n")
      
      print(causality(varLS, cause="growth"))
      print(causality(varLS, cause="debt"))
      # 
