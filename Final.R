

# Nov. 24

# Init° -------------------------------------------------------------------


setwd("C:/Users/Ferdi/Documents/R/C5")
setwd("C:/Users/fouwe/Documents/R/C5")

library(tseries)
library(stats)
library(lmtest)
library(urca)
library(vars)
library(strucchange)

# Read data
  RealGrowth <- read.csv("Z1_RGDP_3Dec_GDPC96.csv", head = TRUE, sep=",")
  DebtToNw <- read.csv("Z1_NFCBusiness_creditMarket_Debt_asPercentageof_NetWorth.csv",
                       head = TRUE, sep=",")
  RgdpPerCAPITA <- read.csv("RGDPperCAPITA.csv", skip = 4, head = TRUE, sep=",")
  "Real GDP per capita in 2011US$, multiple benchmarks (Maddison Project Database (2018)) ($)"
  # Make Time Series of Debt & Gdp objects
  
  gdpts <- ts(RealGrowth$GDPC96, start = c(1947,1),frequency = 4)
  dts <- ts(DebtToNw$NCBCMDPNWMV, start = c(1951,4),frequency = 4)
  gdpCAP <- ts(RgdpPerCAPITA$GDPCAP, start = c(1800,1),frequency = 1)
  
  #Compute LOG(GDP)
  LogRgdp<-log(gdpts)
  #Compute Rgdp Growth RATE
  G_RATE <- ((diff(gdpts, lag=1, differences = 1)/lag(gdpts,-1)))
  GCAP_RATE <- ((diff(gdpCAP, lag=1, differences = 1)/lag(gdpCAP,-1)))
  #AGGREGATE Quaterly to YEARLY Rgdp
  Ygdp <- aggregate(gdpts,nfrequency = 1, FUN = sum)
  Ygdp_RATE <- ((diff(Ygdp, lag=1, differences = 1)/lag(Ygdp,-1)))

#adapt variables' names
  Prod_Inv <- inv5
  Intang_Inv <- (FinInv+IntInv)
  Debt <- dbtot
  Rgdp <- gdpts
  
#MAKE T.S  
  data_vecm6 <- ts.intersect(log(Rgdp),log(Intang_Inv), log(Debt),log(Prod_Inv))
  data_list_w <- window(data_vecm6,start=c(1952,1), end=c(2015,1), frequency=4)
  vecm_data6 <- data.frame(gdp = (data_list_w[,1]), fii = data_list_w[,2],
                           d=(data_list_w[,3]), inv = data_list_w[,4])
  
  plot(data_list_w, nc=2)
  d_data_list_w <- diff(data_list_w)
  plot (d_data_list_w, nc=2)


# STATIONARITY ------------------------------------------------------------
  
  #1- ADF:  Ho=non-stat.  H1= diff-stat.
  #2- KPSS:  Ho=stat.
  #3- Phillips-Perron Ho: non-stat.
  #4- "Elliott, Rothenberg and Stock...
        #...(1996), which utilizes GLS detrending" p.167
        # see Elliot & al. (1996, p.825) for Critical values (-3.46 at 1% here)
        # Ho=non-stat.
  adf.test(vecm_data6[,"gdp"])
  kpss.test(vecm_data6[,"gdp"])
  pp.test(vecm_data6[,"gdp"], type = "Z(t_alpha)")
  ur.ers(vecm_data6[,"gdp"])
    adf.test(diff(vecm_data6[,"gdp"]))
    kpss.test(diff(vecm_data6[,"gdp"]))
    pp.test(diff(vecm_data6[,"gdp"]), type = "Z(t_alpha)")
    ur.ers(diff(vecm_data6[,"gdp"]))
  
  adf.test(vecm_data6[,"fii"])
  kpss.test(vecm_data6[,"fii"])
  pp.test(vecm_data6[,"fii"], type = "Z(t_alpha)")
  ur.ers(vecm_data6[,"fii"])
    adf.test(diff(vecm_data6[,"fii"]))
    kpss.test(diff(vecm_data6[,"fii"]))
    pp.test(diff(vecm_data6[,"fii"]), type = "Z(t_alpha)")
    ur.ers(diff(vecm_data6[,"fii"]), model="trend") #TREND
  
  adf.test(vecm_data6[,"d"])
  kpss.test(vecm_data6[,"d"])
  pp.test(vecm_data6[,"d"], type = "Z(t_alpha)")
  ur.ers(vecm_data6[,"d"])
    adf.test(diff(vecm_data6[,"d"]))
    kpss.test(diff(vecm_data6[,"d"])) #?????????????????????????????
    pp.test(diff(vecm_data6[,"d"]), type = "Z(t_alpha)")
    ur.ers(diff(vecm_data6[,"d"]))
  
  adf.test(vecm_data6[,"inv"])
  kpss.test(vecm_data6[,"inv"])
  pp.test(vecm_data6[,"inv"], type = "Z(t_alpha)")
  ur.ers(vecm_data6[,"inv"])
    adf.test(diff(vecm_data6[,"inv"]))
    kpss.test(diff(vecm_data6[,"inv"]))
    pp.test(diff(vecm_data6[,"inv"]), type = "Z(t_alpha)")
    pp.test(diff(vecm_data6[,"inv"], differences = 2), type = "Z(t_alpha)")
    ur.ers(diff(vecm_data6[,"inv"]), model="trend") #TREND
    
#COINTEGRATION
  #LAG ORDER SELECTION
    VARselect(vecm_data6,lag.max = 8, type = "both")
    VARselect(vecm_data6,lag.max = 8, type = "c")
    VARselect(vecm_data6,lag.max = 8, type = "trend")
    
  # VAR estimat° (p=1, 2 & 7)
    p1<-VAR(vecm_data6, p=3, type = "both")
    p2<-VAR(vecm_data6, p=7, type = "both")

  # VAR diagnostic tests
    #SERIAL: Portmanteau- and Breusch-Godfrey test for serially correlated errors
      serial.test(p1,lags.pt = 16,type = "PT.asymptotic")
      serial.test(p1,lags.pt = 16,type = "PT.adjusted")
      
      serial.test(p2,lags.pt = 16,type = "PT.asymptotic")
      serial.test(p2,lags.pt = 16,type = "PT.adjusted")

    #JB: Jarque-Bera tests and multivariate skewness 
    # and kurtosis tests for the residuals of a VAR(p) or of a VECM in levels.
      normality.test(p1)
      # Non-norm.
      normality.test(p2)
      # Non-norm.
    
    #ARCH: 
      arch.test(p1,lags.multi = 5)
      #Heteroscedastic resid.
      arch.test(p2,lags.multi = 5)
      #Heteroscedastic resid.
    
    #STABILITY
      plot(stability(p2, type = "Rec-MOSUM"),nc=2)
      plot(stability(p1, type = "Rec-MOSUM"),nc=2)
      plot(stability(p2, type = "Rec-CUSUM"),nc=2) #APPENDIX
      plot(stability(p1, type = "Rec-CUSUM"),nc=2) #APPENDIX


    #JOHANSEN TEST : CHOOSE THE K=7 BECAUSE OF STABILITY (see previous graphs)
      vecm6 <- ca.jo(vecm_data6,ecdet="t",K=7)
        summary(vecm6)
      #DUMMY IN 1982  OR 1974???
        vecm6 <- ca.jo(vecm_data6,ecdet="t",K=7, dumvar = d_post)
          summary(vecm6)  
        vecm6 <- ca.jo(vecm_data6,ecdet="t",K=7, dumvar = d_post)
          summary(vecm6)  
      #dummy bring proof of cointegration
          
#THUS, I'LL USE 1980-2015 PERIOD      
      
        
                    #Test for "wrongly accept COINT" for struct. Break (Pfaff §8.2 AND Lütkepohl, H., Saikkonen, P. and Trenkler, C. (2004), )    
                        jojoStruct <- cajolst(vecm_data6)
                        summary(jojoStruct)
                        slot(jojoStruct, "bp")
                        slot(jojoStruct, "x")
                        slot(jojoStruct, "x")[126] # corrsponding to 1983
                        ## RESULTS: NO Cointegration once break accounted for (1983,1)
                        #         i.e there maybe coint just becausz of struct shift
  




















