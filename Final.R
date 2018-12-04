

# Nov. 24

# Init° -------------------------------------------------------------------





# MINSKY -----------------------------------------------------------------
#ARDL MINSKYAN GROWTH REGIME -> PInv=FNCT(U.R.FII)
  data_list<- ts.intersect(diff(ProInv),
                         capu1,profit1/(AssetTot),
                         diff(FinInv+IntInv))

#1st PERIOD: 1952-2017
  
  data_list_w <- window(data_list,start=c(1952,1), end=c(2015,1), frequency=4)
  
  ardl_data <- data.frame(inv = (data_list_w[,1]), u = data_list_w[,2],
                          r=(data_list_w[,3]), fii = data_list_w[,4])
  
  plot (data_list_w, nc=2)
  
  Alt1select1 <- ardl::auto.ardl(inv~u+r+fii, data=ardl_data, ymax=20,
                                 xmax=c(10,10,10),case=5,verbose = T,ic = "aic")
  
  Mod_ii_c5<-ardl::ardl(inv ~ -1+u+r+fii, data=ardl_data, ylag=20,
                        xlag=c(0,7,8), case = 5)
  
  bounds.test(Mod_ii_c5)
  coint(Mod_ii_c5)
  
  #Ho:INDEPENDANT
    Box.test(Mod_ii_c5$residuals,lag = 9, type="Ljung-Box",fitdf=4)
  
  #Ho:constant error variance
    car::ncvTest(Mod_ii_c5)
  
  shapiro.test(Mod_ii_c5$residuals) 
  #Royston (1995) to be adequate for p.value < 0.1.
  
                    #DUMMY
                      dum_2<- c(rep(0,132),rep(1,121))
                              dum_3<- c(rep(0,223),rep(1,30))
                      dum_3<- c(rep(0,223),rep(1,5),rep(0,25) )
                      dum_t <- cbind(dum_2,dum_3)
                      dum_fin<- ts(dum_t, start = c(1952,1),frequency = 4)
                      
                      ardl_data_dum <- data.frame(inv = (data_list_w[,1]), u = data_list_w[,2],
                                            r=(data_list_w[,3]), fii = data_list_w[,4],
                                            dum1= dum_fin[,1], dum2= dum_fin[,2])
                    
                    #1DUMMY 1985
                      Alt1select1 <- ardl::auto.ardl(inv~u+r+fii|dum1, data=ardl_data_dum, ymax=20,
                                                   xmax=c(10,10,10),case=5,verbose = T,ic = "aic")
                      Mod_ii_c5<-ardl::ardl(inv ~ u+r+fii|dum1, data=ardl_data_dum, ylag=20,
                                            xlag=c(4,9,8), case = 5)
                    #2 DUMMIES 1985 & 2007
                      Alt1select1 <- ardl::auto.ardl(inv~u+r+fii|dum1+dum2, data=ardl_data_dum, ymax=30,
                                                   xmax=c(12,12,12),case=3,verbose = T,ic = "aic")
                    
                      Mod_ii_c5<-ardl::ardl(inv ~ u+r+fii|dum1+dum2, data=ardl_data_dum, ylag=20,
                                            xlag=c(4,0,9), case = 5)
                      bounds.test(Mod_ii_c5)
                      coint(Mod_ii_c5)
                      
                      Box.test(Mod_ii_c5$residuals,lag = 9, type="Ljung-Box",fitdf=4)
                      car::ncvTest(Mod_ii_c5)
                      shapiro.test(Mod_ii_c5$residuals)
    

#2nd PERIOD: 1980-2017
  data_list<- ts.intersect(log(inv5),
                             capu1,profit1/(AssetTot),
                             log(FinInv+IntInv))
  data_list_w <- window(data_list,start=c(1985,1), end=c(2015,1), frequency=4)
  
  ardl_data <- data.frame(inv = (data_list_w[,1]), u = data_list_w[,2],
                          r=(data_list_w[,3]), fii = data_list_w[,4])
  
  plot (data_list_w, nc=2)
  
  Mod_ii_c5<-ardl::ardl(inv ~ -1+u+r+fii, data=ardl_data, ylag=13,
                        +                         xlag=c(6,1,8), case = 1)
  
  bounds.test(Mod_ii_c5)
  coint(Mod_ii_c5)
  
  Box.test(Mod_ii_c5$residuals,lag = 9, type="Ljung-Box",fitdf=4)
  car::ncvTest(Mod_ii_c5)
  shapiro.test(Mod_ii_c5$residuals)

  
                #1DUMMY 2007 
                  dum_4<- c(rep(0,110),rep(1,5),rep(0,26) )
                  dum_fin<- ts(dum_4, start = c(1980,1),frequency = 4)
                  ardl_data_dum <- data.frame(inv = (data_list_w[,1]), u = data_list_w[,2],
                                              r=(data_list_w[,3]), fii = data_list_w[,4],
                                              dum4= dum_4)
                
                  Alt1select1 <- ardl::auto.ardl(inv~u+r+fii|dum4, data=ardl_data_dum, ymax=20,
                                                 xmax=c(10,10,10),case=3,verbose = T,ic = "r2")
                  Mod_ii_c5<-ardl::ardl(inv ~ u+r+fii|dum4, data=ardl_data_dum, ylag=19,
                                        xlag=c(7,9,10), case = 3)


#ARDL PInv=FNCT(U.R.FInv.INnv.Dnw) ####
# this model, in turn, disantangles the determination impact of fininv-intinv-debt

data_list<- ts.intersect(log(inv5), capu1,profit1/(AssetTot),
                          FinInvHistRatio,log(IntInv),dbtnw)
                  
           #old alt     data_list<- ts.intersect(log(inv5),capu1,log(profit1),
                    #               (FinInvHistRatio),log(IntInv),dbtnw)
                    #mod <- ardl( inv~u+r+fi+ii+d, data=ardl_data, ylag=4, 
                            #xlag=c(4, 0, 4, 4, 1), case=1, quiet=FALSE )

data_list_w <- window(data_list,start=c(1985,1), end=c(2015,1), frequency=4)

plot (data_list_w, nc=2)

ardl_data <- data.frame(inv = (data_list_w[,1]), u = data_list_w[,2],
                        r=(data_list_w[,3]), fi = (data_list_w[,4]),
                        ii = data_list_w[,5], d = data_list_w[,6])

              
c5select3 <- ardl::auto.ardl(inv~u+r+fi+ii+d, data=ardl_data, ymax=4,
                             xmax=c(4,4,4,4,4),case=1,verbose = T, ic="r2")
mod <- ardl( inv~u+r+fi+ii+d, data=ardl_data, ylag=4, xlag=c(4, 0, 4, 4, 1), case=1, quiet=FALSE )
summary(mod)
bounds.test(mod)
coint(mod)

#PROGRESSIVELY INTRODUCING VARIABLES IN REGRESSIONS
c5select3 <- ardl::auto.ardl(inv~u+r, data=ardl_data, ymax=4,
                             xmax=c(4,4),case=1,verbose = T, ic="r2")
mod <- ardl( inv~u+r, data=ardl_data, ylag=2, xlag=c(2, 2), case=1, quiet=FALSE )

c5select3 <- ardl::auto.ardl(inv~u+r+ii, data=ardl_data, ymax=8,
                             xmax=c(4,4,4),case=1,verbose = T, ic="r2")
mod <- ardl( inv~u+r+ii, data=ardl_data, ylag=5, xlag=c(4, 1, 0), case=1, quiet=FALSE )

c5select3 <- ardl::auto.ardl(inv~u+r+ii+fi, data=ardl_data, ymax=8,
                             xmax=c(8,8,8,8),case=1,verbose = T, ic="r2")
mod <- ardl( inv~u+r+fi+ii, data=ardl_data, ylag=5, xlag=c(4, 0, 0, 4), case=1, quiet=FALSE )


summary(mod)
ardl::bounds.test(mod)
ardl::coint(mod)

# DIAG Tests
#Ho:INDEPENDANT
Box.test(mod$residuals,lag = 9, type="Ljung-Box",fitdf=4)

#Ho:constant error variance
car::ncvTest(mod)

shapiro.test(mod$residuals) 
#Royston (1995) to be adequate for p.value < 0.1.



                    #dummy 2007
                    dum_5<- c(rep(0,90),rep(1,6),rep(0,25) )
                    data_list_w <- window(data_list,start=c(1980,1), end=c(2015,1), frequency=4)
                    
                    ardl_data_dum <- data.frame(inv = (data_list_w[,1]), u = data_list_w[,2],
                                            r=(data_list_w[,3]), fi = (data_list_w[,4]),
                                            ii = data_list_w[,5], d = data_list_w[,6],
                                            dum4= dum_4)
                    
                    
                    c5select3 <- ardl::auto.ardl(inv~u+r+fi+ii+d|dum4, data=ardl_data_dum, ymax=4,
                                                 xmax=c(4,4,4,4,4),case=3,verbose = T, ic="aic")
                    
                    mod <- ardl( inv~u+r+fi+ii+d|dum4, data=ardl_data_dum, ylag=1, xlag=c(1, 0, 4, 4, 1), case=3, quiet=FALSE )
                    
                    bounds.test(mod)
                    coint(mod)




# SVECM ####


setwd("C:/Users/Ferdi/Documents/R/C5")
setwd("C:/Users/fouwe/Documents/R/C5")

library(tseries)
library(stats)
library(lmtest)
library(urca)
library(vars)
library(strucchange)

#Read data
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
  invraw5 <- read.csv("INV5_RealGrossPrivateDomesticInvestment.csv",head = TRUE, sep=",")
  #All FinAsset (including UNIDENTIFIED)
  fininvraw <- read.csv("FinInv.csv", skip = 1,head = TRUE, sep=",")
  #ONLY Identified Financial assets
  finInvIndeed <- read.csv("PURGED_FININV.csv", skip = 2,head = TRUE, sep=",")
  #UnIdentified Financial assets
  intanginv <- read.csv("IntangibleInv.csv", skip = 1,head = TRUE, sep=",")
  prodinvraw <- read.csv("FinInv2.csv", skip = 1,head = TRUE, sep=",")
  
  
  DebtToNw <- read.csv("Z1_NFCBusiness_creditMarket_Debt_asPercentageof_NetWorth.csv",
                       head = TRUE, sep=",")
  DebtToEq <- read.csv("Z1_NFCBusiness_CreditMarketDebtAsPercentageOfMarketValueOfCorpEquities.csv",
                       head = TRUE, sep=",")
  DebtTot <- read.csv("NFCDEBT.csv",
                      head = TRUE, sep=",")
  
inv5 <- ts(invraw5$GPDIC1, start = c(1947,1),end = c(2016,4),frequency = 4)
TotFinInv <- ts(fininvraw$fininv, start = c(1951,4),end = c(2016,4),frequency = 4)
#2nd. is financialy "pure" Fin Assets
FinInv <- ts(finInvIndeed$FININDEED, start = c(1951,4),
             end = c(2015,1),frequency = 4)

#INTANGIBLE INVESTMENT SERIES
IntInv <- ts(intanginv$intinv, start = c(1951,4),
             end = c(2015,1),frequency = 4)

#PRODUCTIVE INVESTMENT SERIES
ProInv <- ts(prodinvraw$physasset, start = c(1951,4),end = c(2016,4),frequency = 4)


FinInvHistRatio <- ts(fininvraw$hfininvratio, start = c(1951,4),end = c(2016,4),frequency = 4)
FinInvRatio <- ts(fininvraw$fininvratio, start = c(1951,4),frequency = 4)
AssetTot <- ts(fininvraw$totinv, start = c(1951,4),frequency = 4)

dbtnw <- ts(DebtToNw$NCBCMDPNWMV, start = c(1951,4),end = c(2016,4),frequency = 4)
dbteq <- ts(DebtToEq$NCBCMDPMVCE, start = c(1951,4),frequency = 4)
dbtot <- ts(DebtTot$CRDQUSANABIS, start = c(1952,1),frequency = 4)
#


  
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
    
##COINTEGRATION ####
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
        #REJECT COINTEGRATION AT 5%
      
      #DUMMY IN 1985
      dd_1<- c(rep(0,132),rep(1,121)) #corresponding to 1985:Q1
      d_post<- ts(d_1, start = c(1952,1),frequency = 4)
                  #DUMMY IN 1980
                    d_1<- c(rep(0,112),rep(1,141)) #corresponding to 1980:Q1
                    d_post<- ts(d_1, start = c(1952,1),frequency = 4)
                  #DUMMY1982
                    d_1<- c(rep(0,120),rep(1,133)) #corresponding to 1982:Q1
                    d_post<- ts(d_1, start = c(1952,1),frequency = 4)
                  #DUMMY1984
                    d_1<- c(rep(0,128),rep(1,125)) #corresponding to 1984:Q1
                    d_post<- ts(d_1, start = c(1952,1),frequency = 4)
        
        vecm6 <- ca.jo(vecm_data6,ecdet="t",K=7, dumvar = d_post)
          summary(vecm6)  
        vecm6 <- ca.jo(vecm_data6,ecdet="t",K=3, dumvar = d_post)
          summary(vecm6)  
      #dummy bring proof of cointegration 1%
          
#THUS, I'LL USE 1985-2015 PERIOD      
  data_list_w <- window(data_vecm6,start=c(1985,1), end=c(2015,1), frequency=4)
  vecm_data6 <- data.frame(gdp = (data_list_w[,1]), fii = data_list_w[,2],
                           d=(data_list_w[,3]), inv = data_list_w[,4])
  plot(data_list_w, nc=2)     
  
  #LAG ORDER SELECTION
    VARselect(vecm_data6,lag.max = 8, type = "both")
    VARselect(vecm_data6,lag.max = 8, type = "c")
    VARselect(vecm_data6,lag.max = 8, type = "trend")
  
  #FIRST, MAKE SURE THAT 2007 CRISIS DIDN'T INDUCE BREAK IN 1980-2015 PERIOD
      #test for mistaking shift in data for cointegration
      #Test for "wrongly accept COINT" for struct. Break (Pfaff §8.2 AND Lütkepohl, H., Saikkonen, P. and Trenkler, C. (2004), )    
                        jojoStruct <- cajolst(vecm_data6, K=3)
                        summary(jojoStruct)
                         slot(jojoStruct, "bp")
                        slot(jojoStruct, "x")
                        slot(jojoStruct, "x")[88] # corrsponding to 2007:Q1
                        ## RESULTS: aDJUSTED VARIABLES CONFFIRM Coint. at 1% 85-2015
                        #         
#VAR estimat° (p=1, 2 & 7)
  p1<-VAR(vecm_data6, p=2, type = "both")
  p2<-VAR(vecm_data6, p=3, type = "both")
  p7<-VAR(vecm_data6, p=5, type = "both")
# VAR diagnostic tests
  #SERIAL: Portmanteau- and Breusch-Godfrey test for serially correlated errors
  serial.test(p1,lags.pt = 16,type = "PT.asymptotic")
  serial.test(p1,lags.pt = 16,type = "PT.adjusted")
  
  serial.test(p2,lags.pt = 16,type = "PT.asymptotic")
  serial.test(p2,lags.pt = 16,type = "PT.adjusted")
  
  serial.test(p7,lags.pt = 16,type = "PT.asymptotic")
  serial.test(p7,lags.pt = 16,type = "PT.adjusted")
  
#JB: Jarque-Bera tests and multivariate skewness 
  # and kurtosis tests for the residuals of a VAR(p) or of a VECM in levels.
  normality.test(p1)
  # Non-norm.
  normality.test(p2)
  # Non-norm.
  normality.test(p7)
  # Non-norm.
  
#ARCH: 
  arch.test(p1,lags.multi = 5)
  #Heteroscedastic resid.
  arch.test(p2,lags.multi = 5)
  #NON Heteroscedastic resid.
  arch.test(p7,lags.multi = 5)
  #NON Heteroscedastic resid.
#RESULTS: LAG=5 beacause of wellbehaved character

#reorder data set for debt priority
  # 1-growth causes debt
    vecm_data6 <- vecm_data6[ , c("d","gdp","fii","inv")]
  # 2-demand/supply seting relation (D=debt ; S=TotInv)  
    vecm_data6 <- vecm_data6[ , c("gdp","d","fii","inv")]
  
vecm6 <- ca.jo(vecm_data6,ecdet="c",K=5)  #if ProIinv: K=(2,c):2coint    if inv5: K=(5,c): 1r
summary(vecm6)
  
# 1 coint relat°
  SR<-matrix(NA,nrow = 4,ncol = 4)
  LR<-matrix(NA,nrow = 4,ncol = 4)
  LR[1:4,1]<-0
  SR[3,2]<-0
  SR[3,4]<-0
  LR[3,4]<-0
  LR
  SR
  
svecm6<-SVEC(vecm6,LR=LR,SR=SR,r=1,lrtest=F,boot = T,runs = 100)    
  
    svecm6
    svecm6$SR / svecm6$SRse
    svecm6$LR
    svecm6$LR / svecm6$LRse
svecm6.irf<-irf(svecm6)
plot(svecm6.irf)

#RESULT:#inv5:
# K=(5,c) : g affects d (LT) ;   g affects fii (LT) ??   ;
          # g affects inv  ;  fii impact  g  ;fii impact inv  ; 

    svecm6.irf<-irf(svecm6, n.ahead = 16)
    plot(svecm6.irf)
    svecm6.irf<-irf(svecm6, n.ahead = 40)
    plot(svecm6.irf)

fevd.d <- fevd(svecm6, n.ahead = 16)$d
fevd.d   

#OVERIDENTIFICATION
  #g HAS NO LONG-RUN IMPACT ON debt (LR(1,2) = 0)
    LR[1,2]<-0
     LR
     svecm.oi <- SVEC(vecm6,LR=LR,SR=SR,r=1,lrtest=TRUE,boot = FALSE,
                      max.iter = 600)
     svecm.oi <- update(svecm6,LR=LR,lrtest=TRUE,boot = FALSE,
                        max.iter = 300)
     svecm.oi$LRover
  #THOUGH NON CONVERGENT...
   #reject the null that g have no LR impact on d
     
     

# SR justification : g do not cause ii -------------------------------------

     #myvarLS <- data.frame(growth=window(LogRgdp,start=c(1985,1),end=c(2015,1)),
      #                     debt=window(dbtnwH,start=c(1985,1),end=c(2015,1)))
     
     myvarLS <- data.frame(growth=window(LogRgdp,start=c(1985,1),end=c(2015,1)),
                           debt=window(diff(FinInv+IntInv),start=c(1985,1),end=c(2015,1)))
     
     ## Choose optimal length for unrestricted VAR
     VARselect(myvar, lag.max = 6, type = "trend")
     # SC  -->  3lags
     # AIC & FPE -->  6lags
     #  HQ -->  5lags
     
     varALL <- VAR(myvar, p = 5, type = "both")
     
     #1952-->2016
     print(causality(varALL, cause="growth"))
     print(causality(varALL, cause="debt"))
     
          
### E N D  ###





















