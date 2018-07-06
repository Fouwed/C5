# Empirical Macro Model of my THESIS
#
# INIT ####
# set working Directory
setwd("C:/Users/Ferdi/Documents/R/C5")
setwd("C:/Users/fouwe/Documents/R/C5")

library(tseries)
library(vars)
library(lmtest)
library(urca)
library(ardl)
library(outliers)
library(strucchange)
## library(gvlma)

# Data --------------------------------------------------------------------

# Read data
invraw5 <- read.csv("INV5_RealGrossPrivateDomesticInvestment.csv",head = TRUE, sep=",")

profraw1 <- read.csv("ProfitsAfterTax.csv", head = TRUE, sep=",")

uraw1 <- read.csv("Capacity1_Utilization_Manuf.csv", head = TRUE, sep=",")

fininvraw <- read.csv("FinInv.csv", skip = 1,head = TRUE, sep=",")
    #All FinAsset (including UNIDENTIFIED)
finInvIndeed <- read.csv("PURGED_FININV.csv", skip = 2,head = TRUE, sep=",")
    #ONLY Identified Financial assets

intanginv <- read.csv("IntangibleInv.csv", skip = 1,head = TRUE, sep=",")
    #UnIdentified Financial assets

prodinvraw <- read.csv("FinInv2.csv", skip = 1,head = TRUE, sep=",")

DebtToNw <- read.csv("Z1_NFCBusiness_creditMarket_Debt_asPercentageof_NetWorth.csv",
                     head = TRUE, sep=",")
DebtToEq <- read.csv("Z1_NFCBusiness_CreditMarketDebtAsPercentageOfMarketValueOfCorpEquities.csv",
                     head = TRUE, sep=",")
DebtTot <- read.csv("NFCDEBT.csv",
                    head = TRUE, sep=",")
#
# Make Time Series of data
inv5 <- ts(invraw5$GPDIC1, start = c(1947,1),end = c(2016,4),frequency = 4)
profit1 <- ts(profraw1$NFCPATAX, start = c(1947,1),end = c(2016,4),frequency = 4)
capu1 <- ts(uraw1$CAPUTLB00004SQ, start = c(1948,1),end = c(2016,4),frequency = 4)

FinInv <- ts(finInvIndeed$FININDEED, start = c(1951,4),
             end = c(2015,1),frequency = 4)

IntInv <- ts(intanginv$intinv, start = c(1951,4),
             end = c(2015,1),frequency = 4)
    #INTANGIBLE INVESTMENT SERIES

ProInv <- ts(prodinvraw$physasset, start = c(1951,4),end = c(2016,4),frequency = 4)
    #PRODUCTIVE INVESTMENT SERIES

FinInvHistRatio <- ts(fininvraw$hfininvratio, start = c(1951,4),end = c(2016,4),frequency = 4)
FinInvRatio <- ts(fininvraw$fininvratio, start = c(1951,4),frequency = 4)
AssetTot <- ts(fininvraw$totinv, start = c(1951,4),frequency = 4)

dbtnw <- ts(DebtToNw$NCBCMDPNWMV, start = c(1951,4),end = c(2016,4),frequency = 4)
dbteq <- ts(DebtToEq$NCBCMDPMVCE, start = c(1951,4),frequency = 4)
dbtot <- ts(DebtTot$CRDQUSANABIS, start = c(1952,1),frequency = 4)
#


# DataSET ----------------------------------------------------------------

#Data sets arangement (as a DATAFRAME)
  #Create LIST of 6 variables (i-u-r-Fi-Ii-D)

    #After Peter's comment, I change 3 variables (Inv, Profit, FinInv)

data_list<- ts.intersect(log(ProInv+IntInv+FinInv), 
                         (capu1),
                         ((profit1/(ProInv+IntInv+FinInv))),
                         dbtot/(ProInv+IntInv+FinInv),
                         ((FinInv+IntInv)/(ProInv+IntInv+FinInv)),
                         #log(IntInv),
                         #log(FinInv), 
                         #LogRgdp, log(inv5),
                         #log(dbtot),
                         (dbtnw))

data_list_w <- window(data_list,start=c(1983,1), end=c(2015,1), frequency=4)

ardl_data <- data.frame(gtot = (data_list_w[,1]),
                        u = data_list_w[,2],
                        r=(data_list_w[,3]), 
                        d = data_list_w[,4],
                        etha = data_list_w[,5],
                        #ii = data_list_w[,6], 
                        #fi = data_list_w[,7],
                        #gdp = data_list_w[,8],
                        #inv = data_list_w[,9],
                        #lgd = data_list_w[,10],
                        dtonw = data_list_w[,6])
#
plot.ts(ardl_data[,1:6])
data_list_w[101:102,2]<-data_list_w[100,2]
data_list_w[103:105,2]<-data_list_w[106,2]
ardl_data[76,"r"]<-ardl_data[75,"r"]
ur.ers(ardl_data[,"r"], model="const")

outlier(ardl_data)

#PLOTS
  ts.plot(gdpts, ylab="Q-RGDP (Level)")
  ts.plot(G_RATE, ylab="Q-Growth (Level)")
  abline(h=0)
  abline(v=2003.75, col="grey50")
  abline(h=0.0125, col="red")
  ts.plot(moymob, type = "h", ylab=paste0("SMA-",malevel," :Q-Growth (Level)"))
  plot.default(G_RATE, type = "h")
  plot.default(Ygdp_RATE, type = "h")
  plot.default(GCAP_RATE, type = "h")
  abline(v=1983.75, col="grey50")
  ts.plot(LogRgdp)
  ts.plot(Ygdp)
  
#LongRun growth
  ts.plot(gdpCAP)
  abline(v =1984)
  abline(v =1945)
  abline(v =1880)
  abline(v =1930)
  
# plot level & Diff-lev
  par(mfrow=c(2,2))
  ts.plot(LogRgdp<-vniveau[,1], ylabflibra="RGDP (log)")
  ts.plot(dnw<-vniveau[,2], ylab="Debt/net worth (level)")

# plot of diff_Debt_level is informative of
  # the transformation in debt dynamics occuring from mid 1980's
    ts.plot(gd<-vdiff_niveau[,1], ylab="RGDP (diff.)")
    ts.plot(dnwd<-vdiff_niveau[,2], ylab="Debt/net worth (diff.)")
    
### COINTEGRAT? #####
   
    #Johansen test
    #Ho: no cointegrat? (r=0 against r>0 , then r<=1 against r>1 etc..)
    # A rank r>0 implies a cointegrating relationship
    # between two or possibly more time series
    
  #MAX
    jojolevel<-ca.jo(cbind(LogRgdp,dnw),ecdet="const") 
    summary(jojolevel)
  ## RESULTS: Cointegration
    
  #TRACE
    jojolevTrace<-ca.jo(cbind(LogRgdp,dnw),ecdet="const",type="trace")
    summary(jojolevTrace)
  ## RESULTS: Cointegration
  
  #Test for "wrongly accept COINT" for struct. Break 
      #(Pfaff §8.2 AND Lütkepohl, H., Saikkonen, P. and Trenkler, C. (2004), )
    jojoStruct <- cajolst(cbind(LogRgdp,dnw))
    summary(jojoStruct)
    slot(jojoStruct, "bp")
    slot(jojoStruct, "x")
    slot(jojoStruct, "x")[126] # corrsponding to 1983
  ## RESULTS: NO Cointegration once break accounted for (1983,1)
    #         i.e there maybe coint just becausz of struct shift

  #TEST 3 separated periods FOR FINANCIALIZATION ACCOUNT
    vniveaupostBpt <- window(vniveau,start=1983,end=2016)
    vniveauante <- window(vniveau,start=1951,end=1983)
    vniveaubtwn <- window(vniveau,start=1985,end=2007)
  #RESAMPLE
    
  #POST 
    #JOHANSEN
    jojopostBpt <- ca.jo(vniveaupostBpt[,(1:2)],ecdet="trend",type="trace") #,type="trace")
    summary(jojopostBpt)
  ##RESULTS: COINT at 1% from 1983 on !!!
    #   i.e one may estimate a VECM for G&D
    #   goto SVAR section
    
  ##ANTE FIN°
    #Johansen TRACE
      jojoAnteTrace<-ca.jo(vniveauante[,(1:2)],ecdet="trend",type="trace") #,type="trace")
      summary(jojoAnteTrace)
    #Johansen MAX
      jojoAnteMax<-ca.jo(vniveau[,(1:2)],ecdet="trend") #,type="trace")
      summary(jojoAnteMax)
  ###RESULTS: NO COINT Neither way 
      
    #Phillips Ouliaris test  # Ho: no cointegrat?
      po.test(vniveauante[,(1:2)], demean = T, lshort = T)      # No COINT
      po.test(vniveauante[,2:1], demean = T, lshort = T) # neither the other way round
    ###RESULTS: Confirms NO COINT 
    
    
  #Test the 1983-2016 period for "wrongly accept COINT" while struct. Break
    #Pfaff §8.2 AND Lütkepohl, H., Saikkonen, P. and Trenkler, C. (2004), )
    jojoStr2007 <- cajolst(vniveaupostBpt[,1:2])
    summary(jojoStr2007)
    slot(jojoStr2007, "bp")
    slot(jojoStr2007, "x")
    slot(jojoStr2007, "x")[101] # corrsponding to 2008:1
  ## RESULTS: Cointegration is confirmed after data readjustment for break (2008,1)
    #         i.e no effect of 2008 financial crisis on D&G comovement
    
        
      
      
      library(strucchange)
      
      
### STRUCT. BREAK TESTS -------------------------------
  #1- StrBrk_1 : EFP
      # EFP = empirical fluct° process
      # Ho: no struct. break (Kleiber p.171)
      
    ## GROWTH ##
      #RGDP LEVEL - Type= MOsum - ALL DATA RANGE
        efpMo_rgdp <- efp(gdpts ~ 1, type = "Rec-MOSUM",h=0.053)#1980's break whithin 0.05-->0.07
        # 0.053 --> 3 years
        plot(efpMo_rgdp)
        abline(v=1983.75, col="grey50")
      ###RESULTS:RGDP BREAK in 1983   ##
        
      #Log(RGDP) - Type= CUMsum & narrowing data range from 1973 on...
        post73<-window(LogRgdp,start=1974)
        efpCum_g73 <- efp(post73 ~ 1)
        plot(efpCum_g73)
        abline(v=1984.00, col="grey50")
      ###RESULTS:logRGDP BREAK in 1984   ###
    
        
    ## DEBT ##
      ### type= MOSUM
      efpMo_d <- efp(dnw ~ 1, type = "Rec-MOSUM",h=0.053)#1980's break whithin 0.05-->0.07
      # 0.053 --> 3 years
      plot(efpMo_d)
      abline(v=1965.75, col="grey50")
      abline(v=1984.75, col="grey50")
    ###RESULTS: BREAK in 1965 then 1984 for dnw   #
      
      
    #Out of conf. interv.
      # Ho: No Struct. Break
      sctest(efpMo_rgdp)
      sctest(efpCum_g73)
      sctest(efpMo_d)
   

  #2- StrBrk_2 : Fstat
      
    ## GROWTH ##
      #LOG-rgdp
        fs.growth <- Fstats(LogRgdp ~ 1)
        breakpoints(fs.growth)
        bp.growth <- breakpoints(LogRgdp ~ 1,breaks = 1)
        summary(bp.growth)
        fmg0 <- lm(LogRgdp ~ 1)
        fmgf <- lm(LogRgdp ~ breakfactor(bp.growth),breaks = 4)
        plot(LogRgdp)
        lines(ts(fitted(fmg0), start=c(1951.75)), col = 3)
        lines(ts(fitted(fmgf), start = c(1951.75), frequency = 4), col = 4)
        lines(bp.growth)
      ###RESULTS: BREAK in 1983:3 for LogRgdp   #   

    ## DEBT ##
      fs.debt2 <- Fstats(dnw ~ 1)
      breakpoints(fs.debt2)
      bp.debt2 <- breakpoints(dnw ~ 1,breaks = 1)
      summary(bp.debt2)
      fmdnw0 <- lm(dnw ~ 1)
      fmdnwf <- lm(dnw ~ breakfactor(bp.debt2))#,breaks = 1))
      plot(dnw)
      lines(ts(fitted(fmdnw0), start=c(1951)), col = 3)
      lines(ts(fitted(fmdnwf), start = c(1951,4), frequency = 4), col = 4)
      lines(bp.debt2)
    ###RESULTS: BREAK in 1985:3 for dnw   #      
      
    ## Long Term: Rgdp Per CAPITA 1800-2016 ##
      fs.gpercap <- Fstats(gdpCAP ~ 1)
      ##Modulating the number of BP...
      bp.gpercap <- breakpoints(gdpCAP ~ 1,breaks = 5)
      summary(bp.gpercap)
      fmg0 <- lm(gdpCAP ~ 1)
      fmgf <- lm(gdpCAP ~ breakfactor(bp.gpercap))#,breaks = 1))
      plot(gdpCAP)
      lines(ts(fitted(fmgf), start = c(1800,1), frequency = 1), col = 4)
      lines(bp.gpercap)
    ###RESULTS: BREAK in 1984 for gdpCAP whenever BrkPts > 1   #   
      

      
# ---- Reduced Form VAR ----------------------------- #
      
  ## Choose optimal length for unrestricted VAR
      VARselect(var_data, lag.max = 6, type = "both")
      # SC & HQ --> 2 lags
      # AIC & FPE --> 3 lags
      
      ## Order the 2 variables
      DcG <- myvar[, c("debt","growth")]
      GcD <- myvar[, c("growth","debt")]
      
      ## Estimate the VAR (for lag length 2 then 3)
      ## Here "both" means we include a constant and a time trend
      GvarD <- VAR(var_data, p = 2, type = "both")
      GvarD <- VAR(var_data, p = 3, type = "both")
      
      
      ## See results (for now, no restrictions on PRIORITY. both RFVAR are symetric)
      DvarG
      GvarD
   
      summary(GvarD)
      
      ## See results for any equation in detail.
      summary(GvarD, equation = "growth")
      
      # Stability: see the roots of the companion matrix for the VAR
      # The moduli of the roots should all lie within the unit circle for the VAR to be stable
      # A stable VAR is stationary.
      roots(GvarD)
      # Two roots are close to unity.
      
      
  ##### Residuals' Diagnostic tests
      
      #SERIAL: Portmanteau- and Breusch-Godfrey test for serially correlated errors
      serial.test(GvarD,lags.pt = 16,type = "PT.asymptotic")
      serial.test(GvarD,lags.pt = 16,type = "PT.adjusted")
      
      #JB: Jarque-Bera tests and multivariate skewness 
      # and kurtosis tests for the residuals of a VAR(p) or of a VECM in levels.
        normality.test(GvarD)
      # Norm. OK
      
      #ARCH: 
        arch.test(GvarD,lags.multi = 5)
      #Heteroscedastic resid.
      
 
### VECM: (G,D) ####
        vecm <- ca.jo(cbind(dnw,LogRgdp),ecdet="trend",K=2)
        vecm <- ca.jo(var_data,ecdet="trend",K=3)
        vecm.r1<-cajorls(vecm,r=1)
        alpha<-coef(vecm.r1$rlm)[1,]
        beta<-vecm.r1$beta
        resids<-resid(vecm.r1$rlm)
        N<-nrow(resids)
        sigma<-crossprod(resids)/N
        
        #alpha t-stats  
        alpha.se<-sqrt(solve(crossprod(cbind(vecm@ZK %*% beta,
                                             vecm@Z1))) [1,1]*diag(sigma))
        alpha.t<-alpha/alpha.se
        
        #beta t-stats  
        beta.se<-sqrt(diag(kronecker(solve(crossprod(vecm@RK [,-1])),
                                     solve(t(alpha) %*% solve(sigma) %*% alpha))))
        beta.t<-c(NA,beta[-1]/beta.se)
        
        #Display alpha & beta (with respect. t-stat) 
        alpha
        alpha.t
        beta
        beta.t
        
# SVECM:  Growth --> Debt ---------------------------------------------------
        #SVECM
        vecm <- ca.jo(cbind(LogRgdp,dnw),ecdet="trend",K=2)
        vecm <- ca.jo(vniveaupostBpt[,(1:2)],ecdet="trend",K=2)
        vecm <- ca.jo(var_data,ecdet="trend",K=3)
        
        SR<-matrix(NA,nrow = 2,ncol = 2)
        LR<-matrix(NA,nrow = 2,ncol = 2)
        LR[1:2,2]<-0
        SR
        LR
        
        svecm<-SVEC(vecm,LR=LR,SR=SR,r=1,lrtest=F,boot = T,runs = 100)  
        svecm
        svecm$SR
        #t-stat
        svecm$SR / svecm$SRse
        svecm$LR
        svecm$LR / svecm$LRse
        
        svecm.irf<-irf(svecm, n.ahead = 48)
        svecm.irf
        plot(svecm.irf)
        
             

        
# SVECM : Y=(rgdp, ii, d, r) --------------------------------        
        # VAR Lag Order
        VARselect(vecm_data,lag.max = 8, type = "both")
        # VAR estimat° (p=1, 2 & 7)
        p1<-VAR(vecm_data, p=1, type = "both")
        p2<-VAR(vecm_data, p=2, type = "both")
        p7<-VAR(vecm_data, p=3, type = "both")
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
        #Heteroscedastic resid.
        arch.test(p7,lags.multi = 5)
        #Heteroscedastic resid.
        
        #Stability : Recursive CUMSUM
        plot(stability(p1),nc=2)
        plot(stability(p2),nc=2)
        plot(stability(p7),nc=2)
        #    
        
        #VECM - Y=gdp,ii,d,inv      
        #reorder data set for debt priority
        vecm_data <- vecm_data[ , c("d","gdp","fii","inv")]
        
        vecm <- ca.jo(vecm_data,ecdet="trend",K=4) #Alternative specif° #1 pass 1 coint. relat° at 5%
        summary(vecm)
        vecm.r1<-cajorls(vecm,r=1)
        alpha<-coef(vecm.r1$rlm)[1,]
        beta<-vecm.r1$beta
        resids<-resid(vecm.r1$rlm)
        N<-nrow(resids)
        sigma<-crossprod(resids)/N
        
        #alpha t-stats  
        alpha.se<-sqrt(solve(crossprod(cbind(vecm@ZK %*% beta,
                                             vecm@Z1))) [1,1]*diag(sigma))
        alpha.t<-alpha/alpha.se
        
        #beta t-stats  
        beta.se<-sqrt(diag(kronecker(solve(crossprod(vecm@RK [,-1])),
                                     solve(t(alpha) %*% solve(sigma) %*% alpha))))
        beta.t<-c(NA,beta[-1]/beta.se)
        
        #Display alpha & beta (with respect. t-stat) 
        alpha
        alpha.t
        beta
        beta.t  
        
        
        #SVECM
        vecm <- ca.jo(vecm_data,ecdet="trend",K=6)
        
        SR<-matrix(NA,nrow = 4,ncol = 4)
        LR<-matrix(NA,nrow = 4,ncol = 4)
        LR[1:4,1]<-0
        SR[3,2]<-0
        SR[3,4]<-0
        LR[3,4]<-0
        
        SR[4,3]<-0
        
        SR
        LR
        
        svecm<-SVEC(vecm,LR=LR,SR=SR,r=1,lrtest=F,boot = T,runs = 100)  
        svecm
        svecm$SR
        #t-stat
        svecm$SR / svecm$SRse
        svecm$LR
        svecm$LR / svecm$LRse
        
        svecm.irf<-irf(svecm)
        svecm.irf
        plot(svecm.irf)
        
        fevd.d <- fevd(svecm, n.ahead = 148)$dbtnw
        fevd.d
        
        
# Stationarity ------------------------------------------------------------

#1- ADF:  Ho=non-stat.  H1= diff-stat.
#2-KPSS:  Ho=stat.

  #LEVELS
    adf.test(ardl_data[,"gtot"])
    kpss.test(ardl_data[,"gtot"])
    adf.test(ardl_data[,"u"])
    kpss.test(ardl_data[,"u"])
    adf.test(ardl_data[,"r"])
    kpss.test(ardl_data[,"r"])
    adf.test(ardl_data[,"d"])
    kpss.test(ardl_data[,"d"])
  # 1st. DIFF
    adf.test(diff(ardl_data[,"gtot"]))
    kpss.test(diff(ardl_data[,"gtot"]))
    adf.test(diff(ardl_data[,"u"]))
    kpss.test(diff(ardl_data[,"u"]))
    adf.test(diff(ardl_data[,"r"]))
    kpss.test(diff(ardl_data[,"r"]))
    adf.test(diff(ardl_data[,"d"]))
    kpss.test(diff(ardl_data[,"d"]))

# all I(1)


# Coint -------------------------------------------------------------------

coint52_2016 <- ca.jo(cbind(ecmeq[,1:5])) #,ecdet="const",type="trace")
summary(coint52_2016)
# as Ratio of TotalAsset -> Coint Rank= 1
coint52_85 <- ca.jo(cbind(ecmeqante[,1:5])) #,ecdet="const",type="trace")
summary(coint52_85)
# Coint Rank=2
coint85_2016 <- ca.jo(cbind(ecmeqpost[,1:5])) #,ecdet="const",type="trace")
summary(coint85_2016)
# as Ratio of TotalAsset -> Coint Rank=1
coint85_2007 <- ca.jo(cbind(ecmeqbtwn[,1:5])) #,ecdet="const",type="trace")
summary(coint85_2007)
# as Ratio of TotalAsset -> Coint Rank=1

# test for structural breack btw 85 & 2016 (2007 crisis)
cointbreak07 <- cajolst(ecmeqpost[,1:5])
summary(cointbreak07)
slot(cointbreak07,"bp")
# COINT rank 1 CONFIRMED even Break=2008:Q2



# Lag selection -----------------------------------------------------------
dy <- diff(data_list_w[,1])
y1 <- lag(data_list_w[,1])
u1 <- lag(data_list_w[,2])
r1 <- lag(data_list_w[,3])
d1 <- lag(data_list_w[,4])

for(i in 1:2) {
  du[i] <- diff(data_list_w[,2], i)
}

du0<-data_list_w[,2]
du1<-diff(data_list_w[,2],1)
du2<-diff(data_list_w[,2],2)
du3<-diff(data_list_w[,2],3)
du4<-diff(data_list_w[,2],4)

dr0<-data_list_w[,3]
dr1<-diff(data_list_w[,3],1)
dr2<-diff(data_list_w[,3],2)
dr3<-diff(data_list_w[,3],3)
dr4<-diff(data_list_w[,3],4)

dd0<-data_list_w[,4]
dd1<-diff(data_list_w[,4],1)
dd2<-diff(data_list_w[,4],2)
dd3<-diff(data_list_w[,4],3)
dd4<-diff(data_list_w[,4],4)
dd5<-diff(data_list_w[,4],5)
dd6<-diff(data_list_w[,4],6)
dd7<-diff(data_list_w[,4],7)
dd8<-diff(data_list_w[,4],8)


#
s <- ts.intersect(dy,
                  du0,dr0,dd0,
                  du1,du2,du3,du4,
                  dr1,dr2,
                  dd1,dd2,dd3,dd4,dd5,dd6,dd7,dd8,
                  y1,u1,r1,d1)
VARselect(s[,"dy"],lag.max = 18, type = "both",
          exogen = cbind(s[,"du0"],s[,"dr0"],s[,"dd0"],
                         s[,"du1"],s[,"du2"],s[,"du3"],s[,"du4"],
                         s[,"dr1"],s[,"dr1"],
                         s[,"dd1"],s[,"dd2"],s[,"dd3"],s[,"dd4"],
                         s[,"dd5"],s[,"dd6"],s[,"dd7"],s[,"dd8"],
                         s[,"y1"],s[,"u1"],s[,"r1"],s[,"d1"]))
# 2 methods (ARDL Chapter p.54)
# 1- Informat° Criteria
# 2- iid residuals


# Ardl  ------------------------------------------------------------

  #Merging Fi & ii into Fii=total intangibles(financial+goodwill)

#CROISS=Fii 

Mod_sos<-ardl::ardl(gtot ~ u+r+d, data=ardl_data, ylag=16,
                    xlag=c(4,2,8), case = 5)

Mod_sos<-ardl::ardl(gtot ~ u + r +d, data=ardl_data, ylag=8,
                    xlag=c(4,8,8), case = 3)
summary(Mod_sos)

######## WALD TEST OK -->  long run relationship btw i~u.r.fi+DEBT ###
bounds.test(Mod_sos)

coint(Mod_sos)

plot(Mod_sos)

# I.I.D TESTS
Box.test(Mod_sos$residuals,lag = 9, type="Ljung-Box",fitdf=4)
#Ho:INDEPENDANT 

shapiro.test(Mod_sos$residuals) #Royston (1995) to be adequate for p.value < 0.1.
#Ho:nORMALITY

car::ncvTest(Mod_sos)
#Ho:constant error variance


qqnorm(Mod_sos$residuals)
qqline(Mod_sos$residuals)  
bgtest(Mod_sos$residuals)

boxplot(Mod_sos$residuals)
hist(Mod_sos$residuals)
shapiro.test(Mod_sos$residuals) #Royston (1995) to be adequate for p.value < 0.1.




