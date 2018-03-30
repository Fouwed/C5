setwd("C:/Users/Ferdi/Documents/R/C5")
library(tseries)
library(stats)
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
#PLOTS
  ts.plot(gdpts)
  ts.plot(G_RATE)
  abline(h=0)
  abline(v=2003.75, col="grey50")
  abline(h=0.0125, col="red")
  
  plot.default(G_RATE, type = "h")
  plot.default(Ygdp_RATE, type = "h")
  plot.default(GCAP_RATE, type = "h")
  ts.plot(LogRgdp)
  ts.plot(Ygdp)
  

#LongRun growth
ts.plot(gdpCAP)
abline(v =1984)
abline(v =1945)
abline(v =1880)
abline(v =1930)


# Bind the series into a list
vniveau<-ts.intersect(LogRgdp,dts,lag(LogRgdp,k=-1),lag(dts,lag=-1))
####     vlog<-log(vniveau)
vdiff_niveau<-diff(vniveau)
####     vdiff_log<-diff(vlog)

vdiff_2_niveau<-diff(vniveau,k=2)
gd2<-vdiff_2_niveau[,1]
ts.plot(dts)
ts.plot(gdpts)
ts.plot(LogRgdp)
ts.plot(gd2)
# plot level & Diff-lev
par(mfrow=c(2,2))
ts.plot(LogRgdp<-vniveau[,1], ylab="RGDP (log)")
ts.plot(dnw<-vniveau[,2], ylab="Debt/net worth (level)")
# plot of diff_Debt_level is informative of
# the transformation in debt dynamics occuring from mid 1980's
ts.plot(gd<-vdiff_niveau[,1], ylab="RGDP (diff.)")
ts.plot(dnwd<-vdiff_niveau[,2], ylab="Debt/net worth (diff.)")

# plot log & Diff-log
####     ts.plot(gl<-vlog[,1])
####     ts.plot(dnwl<-vlog[,2])
####     ts.plot(gld<-vdiff_log[,1])
####     ts.plot(dnwld<-vdiff_log[,2])
####     par(mfrow=c(1,1))


#glag<-vlog[,3]
#dnwlag<-vlog[,4]

# like Kleiber p.165: 2 ways to adress Stationarity :
  #1- ADF:  Ho=non-stat.  H1= diff-stat.
  #2-KPSS:  Ho=stat.

# LEVELS
adf.test(LogRgdp)
adf.test(gd)
adf.test((dnw))
adf.test((dnwd))
# g: non-stat. but is diff-stat.
# dnw: non-stat. but is diff-stat.

# LOGs
####     adf.test(gl)
####     adf.test(gld)
####     adf.test(dnwl)
####     adf.test(dnwld)
####     # g: non-stat. but is diff-stat.
####     # dnw: non-stat. but is diff-stat

# Phillips-Perron test for stationarity
# Ho: non-stat.
  #LEVEL
    pp.test(LogRgdp, type = "Z(t_alpha)")
    pp.test(dnw, type = "Z(t_alpha)")
  #Diff
    pp.test(gd, type = "Z(t_alpha)")
    pp.test(dnwd, type = "Z(t_alpha)")
  #RESULTS CONFIRM ADF results of 
     # non-stat. in LEVEL but Stat. in DIFF.

library(urca)

#Alternative test is "Elliott, Rothenberg and Stock...
  #...(1996), which utilizes GLS detrending" p.167
  # see Elliot & al. (1996, p.825) for Critical values (-3.46 at 1% here)
  # Ho=non-stat.
  #Level
    ur.ers(LogRgdp, model="const")
    ur.ers(dnw)
  #Diff
    ur.ers(gd,model="trend")
    ur.ers(dnwd,model="trend")  
    
      ####       # LOG
      ####         ur.ers(gl, model="const")
      ####         ur.ers(dnwl)
      ####       # Log-Diff
      ####         ur.ers(gld,model="trend")
      ####         ur.ers(dnwld,model="trend")
    
#also CONFIRM ADF & PP results (only at 2.5% for LOG-Debt)

#2- KPSS  # Ho: stationnarity
  #Level
    kpss.test(LogRgdp,null = "Trend")
    kpss.test(dnw,null = "Trend") #both reject Ho of stat.
  #Diff no-trend
    kpss.test(gd) # Growth not diff-stat. --> try including a trend:
    kpss.test(dnwd)
  #Diff + Trend
    kpss.test(gd,null = "Trend")
    kpss.test(dnwd,null = "Trend")
#also CONFIRM ADF, PP & ERS results.
    
      ####       #LOG
      ####         kpss.test(gl,null = "Trend")
      ####         kpss.test(dnwl,null = "Trend") #both reject Ho of stat.
      ####       #Log-Diff ; no-trend
      ####         kpss.test(gld)
      ####        kpss.test(dnwld) # Debt not diff-stat. --> try including a trend:
      ####      #Log-Diff Trend
      ####         kpss.test(gld,null = "Trend")
      ####         kpss.test(dnwld,null = "Trend")



    ###  NOTE that ADF,PP,ERS are UnitRoot test
    #       while KPSS is Stationarity test
    
    
    # WATCHOUT !!          ##
    # do UR test again to
    # consider time trends
    # versus constant
    # of deterministic
    # component           ##



### COINTEGRAT? #####

  #Elliot &. also have cointegr?
  
  #Phillips Ouliaris test  # Ho: no cointegrat?
    # LEVELS
      po.test(cbind(LogRgdp,dnw), demean = T, lshort = T)      #  NO COINT
      po.test(cbind(dnw,LogRgdp), demean = T, lshort = T) # idem the other way round
     ### RESULTS:  NO Cointegration
      
      ####         # LOG
      ####           po.test(vlog[,1:2], demean = T, lshort = T)      #  COINT
      ####           po.test(vlog[,2:1], demean = T, lshort = T) # idem the other way round
      ####          ## RESULTS:  Cointegration
  
  #Johansen test
    #Ho: no cointegrat? (r=0 against r>0 , then r<=1 against r>1 etc..)
          # A rank r>0 implies a cointegrating relationship
          # between two or possibly more time series
      ####            #LOG
      ####              jojo<-ca.jo(vlog[,(1:2)])#,ecdet="const",type="trace")
      ####              summary(jojo)
      ####              # NO COINT in log ##
      
    #MAX
      jojolevel<-ca.jo(cbind(LogRgdp,dnw),ecdet="const") #,type="trace")
      summary(jojolevel)
     # Ho: "no cointeg?" is rejected at 1% --> possible cointeg? for r<=1
          #  for r<=1: tstat < crit.val. --> cointegration btw (x+1) variables
    #TRACE
       jojolevTrace<-ca.jo(cbind(LogRgdp,dnw),ecdet="const",type="trace")
      summary(jojolevTrace)
     ## RESULTS: Cointegration
#

#Test for "wrongly accept COINT" for struct. Break (Pfaff §8.2 AND Lütkepohl, H., Saikkonen, P. and Trenkler, C. (2004), )
  #LEVEL
    jojoStruct <- cajolst(cbind(LogRgdp,dnw))
    summary(jojoStruct)
    slot(jojoStruct, "bp")
    slot(jojoStruct, "x")
    slot(jojoStruct, "x")[126] # corrsponding to 1983
    ## RESULTS: NO Cointegration once break accounted for (1983,1)
    #         i.e there maybe coint just becausz of struct shift
  
    ####              #LOG
    ####                jojoLOGStruct <- cajolst(vlog[,(1:2)])
    ####                summary(jojoLOGStruct)
    ####                slot(jojoLOGStruct, "bp")
    ####                ## RESULTS: NO Cointegration once break accounted for
#
    
#TEST 3 separated periods FOR FINANCIALIZATION ACCOUNT
  #RESAMPLE
    #LEVEL
      vniveaupostBpt <- window(vniveau,start=1983,end=2016)
      vniveauante <- window(vniveau,start=1951,end=1985)
      vniveaubtwn <- window(vniveau,start=1985,end=2007)
      ####                #LOG
      ####                  vlogpostBpt <- window(vlog,start=1983,end=2016)
      ####                  vlogante <- window(vlog,start=1951,end=1983)
      ####                  vlogBtw <- window(vlog,start=1983,end=2007)
    
  #POST 
    #Phillips Ouliaris test  # Ho: no cointegrat?
      #LEVEL
        #P.O
        po.test(vniveaupostBpt[,1:2], demean = T, lshort = T)      #  NO COINT
        po.test(vniveaupostBpt[,2:1], demean = T, lshort = T) # idem the other way round
        ## RESULTS:  NO Cointegration
    #JOHANSEN
        jojopostBpt <- ca.jo(vniveaupostBpt[,(1:2)],ecdet="const",type="trace") #,type="trace")
        summary(jojopostBpt)
      ##RESULTS: COINT at 1% from 1983 on !!!
      #           i.e one may estimate a VECM for G&D
      #           goto SVAR section
      
      ####                #LOG
      ####                  po.test(vlogpostBpt[,1:2], demean = T, lshort = T) 
      ####                 po.test(vlogpostBpt[,2:1], demean = T, lshort = T) # idem the other way round
      ####                 #JO
      ####                 jojoLOGpostBpt <- ca.jo(vlogpostBpt[,(1:2)],ecdet="const",type="trace") #,type="trace")
      ####                 summary(jojoLOGpostBpt)
      ####                 ## RESULTS: COINT
  

  ## ANTE FIN°
    #LEVEL
      #P.O
      po.test(vniveauante[,(1:2)], demean = T, lshort = T)      # No COINT
      po.test(vniveauante[,2:1], demean = T, lshort = T) # neither the other way round
      #Johansen
      jojoAnteTrace<-ca.jo(vniveauante[,(1:2)],ecdet="const",type="trace") #,type="trace")
      summary(jojoAnteTrace)
      ###  5% COINT 
      jojoAnteMax<-ca.jo(vniveau[,(1:2)],ecdet="const") #,type="trace")
      summary(jojoAnteMax)
      ###  1% COINT 
      
      ####                #LOG
      ####                  po.test(vlogante[,1:2], demean = T, lshort = T) 
      ####                  po.test(vlogante[,2:1], demean = T, lshort = T) 
      ####                 #Johansen
      ####                 jojoLOGante<-ca.jo(vlogante[,(1:2)],ecdet="const",type="trace") #,type="trace")
      ####                 summary(jojoLOGante)


  ## BETWEEN FIN°
    #LEVEL
      #P.O
        po.test(vniveaubtwn[,(1:2)], demean = T, lshort = T)      # No COINT
        po.test(vniveaubtwn[,2:1], demean = T, lshort = T) # neither the other way round
        #Johansen
        jojoBTW<-ca.jo(vniveaubtwn[,(1:2)],ecdet="const",type="trace") #,type="trace")
        summary(jojoBTW)
       ###  COINT 
      

library(strucchange)

### STRUCT. BREAK TESTS
  #1- StrBrk_1 : EFP ----------------------------------------------------------
    # EFP = empirical fluct? process
    # Ho: no struct. break (Kleiber p.171)
    
            
    ## GROWTH ##
      #Type= MOsum
        #RGDP LEVEL - ALL DATA RANGE
          efpMo_rgdp <- efp(gdpts ~ 1, type = "Rec-MOSUM",h=0.053)#1980's break whithin 0.05-->0.07
            # 0.053 --> 3 years
          plot(efpMo_rgdp)
        ### RGDP BREAK in early 1980's   ##
      #
      #Type= CUMsum
        #Log(RGDP) & narrowing data range from 1973 on...
          post73<-window(LogRgdp,start=1974)
          efpCum_g73 <- efp(post73 ~ 1)
          plot(efpCum_g73)
          ### logRGDP BREAK in 1984   ###
      #
    ## DEBT ##
      ### type= MOSUM
        efpMo_d <- efp(dnw ~ 1, type = "Rec-MOSUM",h=0.053)#1980's break whithin 0.05-->0.07
        # 0.053 --> 3 years
        plot(efpMo_d)
      # BREAK in 1973 then mid 1980's for dnw   #
    
      ### type= Recursive-CUMSUM
        efpReCum_d <- efp(dnw ~ 1)
        # 0.053 --> 3 years
        plot(efpReCum_d, alpha = 0.05, alt.boundary = TRUE)
      # BREAK in late 1960's       
        
  
    #Out of conf. interv.
        # struc. br. if p.value<conf.int.  i.e reject Ho
      sctest(efpMo_rgdp)
      sctest(efpCum_g73)
      sctest(efpMo_d)
      
      ####         sctest(efpMo_g73)
      ####         sctest(sc_efp2_g)
      
      
      # struct. breaks found for diff(realGDP)
      plot(efpMo_rgdp)
      plot(efpCum_g73)
      plot(efpMo_d)
      
      plot(efpCum_g73, alpha = 0.05, alt.boundary = TRUE)
    #
  #
      
  #2- StrBrk_2 : Fstat --------------------------------------------------------
  
    ## GROWTH ##
      #LOG-rgdp
        fs.growth <- Fstats(LogRgdp ~ 1)
        plot(fs.growth)
        breakpoints(fs.growth)
        plot(LogRgdp)
        lines(breakpoints(fs.growth))
        bp.growth <- breakpoints(LogRgdp ~ 1,breaks = 1)
        summary(bp.growth)
        fmg0 <- lm(LogRgdp ~ 1)
        fmgf <- lm(LogRgdp ~ breakfactor(bp.growth))#,breaks = 1))
        plot(LogRgdp)
        lines(ts(fitted(fmg0), start=c(1951)), col = 3)
        lines(ts(fitted(fmgf), start = c(1951,4), frequency = 4), col = 4)
        lines(bp.growth)
      # 
      #RGDP
        fs.gdpts <- Fstats(gdpts ~ 1)
        plot(fs.gdpts)
        breakpoints(fs.gdpts)
        plot(gdpts)
        lines(breakpoints(fs.gdpts))
        ##Break: 1989.1
        #
        bp.gdpts <- breakpoints(gdpts ~ 1,breaks = 1)
        summary(bp.gdpts)
        fmg0 <- lm(gdpts ~ 1)
        fmgf <- lm(gdpts ~ breakfactor(bp.gdpts))#,breaks = 1))
        plot(gdpts)
        lines(ts(fitted(fmg0), start=c(1951)), col = 3)
        lines(ts(fitted(fmgf), start = c(1951,4), frequency = 4), col = 4)
        lines(bp.gdpts)
      #  
      ##Sample OUT of 2007 crisis##
        g2007<-window(LogRgdp,start=1952,end=2008)
        bp.growth2007 <- breakpoints(g2007 ~ 1,breaks = 1)
        summary(bp.growth2007)
        fmg0 <- lm(g2007 ~ 1)
        fmgf <- lm(g2007 ~ breakfactor(bp.growth2007))#,breaks = 1))
        plot(g2007)
        lines(ts(fitted(fmg0), start=c(1951)), col = 3)
        lines(ts(fitted(fmgf), start = c(1951,4), frequency = 4), col = 4)
        lines(bp.growth2007)
      #

    ## DEBT ##
      fs.debt2 <- Fstats(dnw ~ 1)
      plot(fs.debt2)
      plot(dnw)
      breakpoints(fs.debt2)
      lines(breakpoints(fs.debt2))
      ####### Break 1985.3

      bp.debt2 <- breakpoints(dnw ~ 1,breaks = 1)
      summary(bp.debt2)
      fmdnw0 <- lm(dnw ~ 1)
      fmdnwf <- lm(dnw ~ breakfactor(bp.debt2))#,breaks = 1))
      plot(dnw)
      lines(ts(fitted(fmdnw0), start=c(1951)), col = 3)
      lines(ts(fitted(fmdnwf), start = c(1951,4), frequency = 4), col = 4)
      lines(bp.debt2)
    ##

    ## Rgdp Per CAPITA ##
      #gdpCAP
      fs.gpercap <- Fstats(gdpCAP ~ 1)
      plot(fs.gpercap)
      breakpoints(fs.gpercap)
      plot(gdpCAP)
      lines(breakpoints(fs.gpercap))
      ##Modulating the number of BP...
        bp.gpercap <- breakpoints(gdpCAP ~ 1,breaks = 5)
        summary(bp.gpercap)
        fmg0 <- lm(gdpCAP ~ 1)
        fmgf <- lm(gdpCAP ~ breakfactor(bp.gpercap))#,breaks = 1))
        plot(gdpCAP)
        lines(ts(fitted(fmg0), start=c(1800)), col = 3)
        lines(ts(fitted(fmgf), start = c(1800,1), frequency = 1), col = 4)
        lines(bp.gpercap)
      # 


# SVAR --------------------------------------------------------------------


##LOG-rgdp
  myvar <- data.frame(growth=LogRgdp, debt=dnw)
##LEVEL-rgdp
  myvar <- data.frame(growth=window(gdpts,start=c(1951,4),end=c(2016,3)), debt=dnw)        
library(urca)
library(vars)
# --------------------------------------------------------------------------- #
# ---------------------------- Reduced Form VAR ----------------------------- #

## Choose optimal length for unrestricted VAR
VARselect(myvar, lag.max = 6, type = "both")
  # SC --> 2 lags

## Order the 2 variables
DcG <- myvar[, c("debt","growth")]
GcD <- myvar[, c("growth","debt")]

## Estimate the VAR (for lag length 2)
## Here "both" means we include a constant and a time trend
DvarG <- VAR(DcG, p = 2, type = "both")
GvarD <- VAR(GcD, p = 2, type = "both")
## See results (for now, no restrictions on PRIORITY. both RFVAR are symetric)
DvarG
GvarD

## See more details.
## The variance-covariance matrix (and the correlation matrix)
## can be found at the very end (scroll down)
summary(GvarD)

## See results for any equation in detail.
summary(GvarD, equation = "growth")

# Stability: see the roots of the companion matrix for the VAR
# The moduli of the roots should all lie within the unit circle for the VAR to be stable
# A stable VAR is stationary.
roots(GvarD)
# One of the roots is close to unity.

# since DEBT & GROWTH are coint (see 'jojopostBpt')
# and the syst. ROOTS are close to 1
# I resort to VECM (VAR in diff + EC term)
# COINT = EC term
  # First:  CAUSALITY test 
    # then  VECM: G-D where G PRIOR to D
    # then  SVECM:G-D-FinInv-Inv PFAFF ?8

# As an experiment, estimate the VAR without the time trend but a constant,
GvarD.new <- VAR(myvar, p = 2, type = "const")
roots(GvarD.new)

# Granger Causality -------------------------------------------------------

                # Note: this conducts block exogeneity test 
#1952-->2016
  print(causality(GvarD, cause="growth"))
  print(causality(GvarD, cause="debt"))
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
gLS<-window(vniveau[,1],start=1962,end=1998)
dLS<-window(vniveau[,2],start=1962,end=1998)
myvarLS<-cbind(gLS,dLS)
VARselect(myvarLS, lag.max = 6, type = "both")
varLS <- VAR(myvarLS, p = 2, type = "both")
print(causality(varLS, cause="gLS"))
print(causality(varLS, cause="dLS"))
# L&Secca results are partially confirmed :
  # no causality from G --> D
  # nor the o-w-round
  # not simulataneous causality

# REMEMBER AND TRY CANADA DATA !!!

# SVECM:  Growth --> Debt ---------------------------------------------------


















# NOTE: The results for the reduced form VAR are not of much use beside
# summarising the dynamic patterns of correlatiosn among the variables.
# To carry out causal analysis, we need to recover the structural errors
# from the reduced form errors. We can do so by estimating a SVAR:
# (1) Recursive SVAR
# (2) Nonrecursive SVAR


# ------------------------------------------------------------------------- #
# --------------------- Recursive SVAR ------------------------------------ #

## Orthogonalized impulse response function: note that R uses the Cholesky
## decompostion, i.e., the matrix multiplying the vector of "structural errors" is 
## lower triangular (because the inverse of a lower triangular matrix is 
## also lower triangular). This should be kept in mind while choosing the
## "ordering" of the variables. Also note that the units of the impulse are
## measured in standard deviation of the transformed reduced form errors.
## Hence, make sure that the standard deviation of the variables are not orders 
## of magnitude apart.

forecast.horiz <- 25 # Forecast Horizon

## Impulse Response Function for a impulse to "debt"
## Note: the particular ordering chosen for this VAR implies that "prod" is
## causally prior to the other variables. Hence, shocks to "debt" will 
## have contemporaneous impacts on all the other variables
GvarD.impdebt.irf <- irf(GvarD, impulse="debt", n.ahead=forecast.horiz, boot=TRUE)
pdf("Impulse_debt.pdf")
plot(GvarD.impdebt.irf)
dev.off()

## Impulse Response Function for a impulse to "growth"
GvarD.impgrowth.irf <- irf(GvarD, impulse="growth", n.ahead=forecast.horiz, boot=TRUE)
pdf("Impulse_growth.pdf")
plot(GvarD.impgrowth.irf)
dev.off()


### OLD - Struct break ### -----------------------------------------------------------------



# set list for Alternative regression fits
#1 #2  #3            #4
vbn21<-ts.intersect(LogRgdp,gd,lag(LogRgdp,k=-1),lag(gd,k=-1))
vbn22<-ts.intersect(gl,gld,lag(gl,k=-1),lag(gld,k=-1))
#1  #2   #3            #4

### REcursive CUMSUM
efpCum_g <- efp(LogRgdp ~ 1)     #vbn21[,1]
plot(efpCum_g)  # break in 1973 oil crisis


efpMo_g73 <- efp(post73 ~ 1, type = "Rec-MOSUM",h=0.087)#1980's break whithin 0.08-->0.1
# 0.087 --> 3 years
plot(efpMo_g73)


### DIFF. RGDP
efp_gd <- efp(diff(gdpts) ~ 1)   #vbn21[,2]
plot(efp_gd)
###  BREAK in early 1990's ###
#        for g.diff          #




# Iteration on h parameter (window of the mobile average)
#/for(i in 100:200){
#print(ii<-i*.001)
# sc_efp_g <- efp(post73 ~ 1, type = "Rec-MOSUM",h=ii)
# plot(sc_efp_g,main = ii)
#}


# Alternative efp-regression
Mpost73<-window(vbn21,start=1974)
sc_efp1_g <- efp(Mpost73[,1] ~ 1+Mpost73[,3], type = "OLS-MOSUM")
plot(sc_efp1_g)
### RAS ###

sc_efp2_g <- efp(vbn21[,1] ~ 1+vbn21[,3], type = "OLS-MOSUM")
plot(sc_efp2_g)
### RAS ###


### REcursive CUMSUM
######    efpCum_d <- efp(dnw ~ 1)     #vbn21[,1]
######    plot(efpCum_d)  # break in 1973 oil crisis
######    # R.A.S
######    # narrowing data range from 1973 on...
######    dpost73<-window(dnw,start=1976)
######    efpCum_d73 <- efp(dpost73 ~ 1)
######    plot(efpCum_d73)
######    efpMo_d73 <- efp(dpost73 ~ 1, type = "Rec-MOSUM",h=0.087)#1980's break whithin 0.08-->0.1
######    # 0.087 --> 3 years
######    plot(efpMo_d73)

## OLD Growth Log ##
Lfs.growth <- Fstats(gl ~ 1)
plot(Lfs.growth)
breakpoints(Lfs.growth)
plot(gl)
lines(breakpoints(Lfs.growth))

Lbp.growth <- breakpoints(gl ~ 1,breaks = 1)
summary(Lbp.growth)
fmg0 <- lm(gl ~ 1)
fmgf <- lm(gl ~ breakfactor(Lbp.growth))#,breaks = 1))
plot(gl)
lines(ts(fitted(fmg0), start=c(1951)), col = 3)
lines(ts(fitted(fmgf), start = c(1951,4), frequency = 4), col = 4)
lines(Lbp.growth)
####################################  window(LogRgdp,start=1952,end=2008)
gl2007<-window(gl,start=1955,end=2008)
bp.growthl2007 <- breakpoints(gl2007 ~ 1,breaks = 1)
summary(bp.growthl2007)
##



## OLD  DEBT  Log ##
Lfs.debt2 <- Fstats(dnwl ~ 1)
plot(Lfs.debt2)
plot(dnwl)
breakpoints(Lfs.debt2)
lines(breakpoints(Lfs.debt2))

Lbp.debt2 <- breakpoints(dnwl ~ 1,breaks = 2)
summary(Lbp.debt2)

fmdnw0 <- lm(dnwl ~ 1)
fmdnwf <- lm(dnwl ~ breakfactor(Lbp.debt2))#,breaks = 1))
plot(dnwl)
lines(ts(fitted(fmdnw0), start=c(1951)), col = 3)
lines(ts(fitted(fmdnwf), start = c(1951,4), frequency = 4), col = 4)
lines(Lbp.debt2)
##

