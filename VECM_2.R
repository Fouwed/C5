
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
#MOBILE AVERAGE
library(TTR)
malevel<-12
moymob<-TTR::SMA(G_RATE,malevel)

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


# Bind the series into a list
vniveau<-ts.intersect(LogRgdp,dbteq,lag(LogRgdp,k=-1),lag(dts,lag=-1))
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
ts.plot(LogRgdp<-vniveau[,1], ylabflibra="RGDP (log)")
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
po.test(cbind(LogRgdp,dbteq), demean = T, lshort = T)      #  NO COINT
po.test(cbind(dbteq,LogRgdp), demean = T, lshort = T) # idem the other way round
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

# 1962->2016  
LogRgdp<- window(LogRgdp, start = c(1962,4),end = c(2016,3),frequency = 4)
dbteq <- ts(DebtToEq$NCBCMDPMVCE, start = c(1962,4),end = c(2016,3),frequency = 4)

#MAX
jojolevel<-ca.jo(cbind(LogRgdp,dbteq),ecdet="trend",type="eigen",K=2)
summary(jojolevel)
#Lags=3
jojolevel<-ca.jo(cbind(LogRgdp,dbteq),ecdet="trend",type="eigen",K=3)
summary(jojolevel)
# NO cointeg
# Ho: "no cointeg?" is not rejected even at 10% --> 
#  for r<=1: tstat < crit.val. --> cointegration btw (x+1) variables

#TRACE
jojolevTrace<-ca.jo(cbind(LogRgdp,dbteq),ecdet="trend",type="trace",K=2)
summary(jojolevTrace)
#Lags=3
jojolevTrace<-ca.jo(cbind(LogRgdp,dbteq),ecdet="trend",type="trace",K=3)
summary(jojolevTrace)
## RESULTS: NO Cointegration

#DUMMY
d_1<- c(rep(0,81),rep(1,135)) #corresponding to 1983:00
d_post<- ts(d_1, start = c(1962,4),frequency = 4)
#d_0<- c(rep(0,124),rep(1,136))
#d_1<- c(rep(0,89),rep(1,171))
#d_post<- ts(d_1, start = c(1951,4),frequency = 4)

jojodum<-ca.jo(cbind(LogRgdp,dbteq),ecdet="trend", dumvar=d_post,type="eigen",K=2)
summary(jojodum)

jojodum<-ca.jo(cbind(LogRgdp,dbteq),ecdet="trend", dumvar=d_post,type="eigen",K=3)
summary(jojodum)
#TRACE
jojodumTrace<-ca.jo(cbind(LogRgdp,dbteq),ecdet="trend",
                    type="trace", dumvar=d_post, K=2)
summary(jojodumTrace)

jojodumTrace<-ca.jo(cbind(LogRgdp,dbteq),ecdet="trend",
                    type="trace", dumvar=d_post, K=3)
summary(jojodumTrace)


##COINT btw GrowthRate & dbtnwHIST 
    # 1951->2016  
    G_RATE<- window(G_RATE, start = c(1951,4),end = c(2016,3),frequency = 4)
    dbtnwH<- window(dbtnwH, start = c(1951,4),end = c(2016,3),frequency = 4)
    #MAX
    jojolevel<-ca.jo(cbind(G_RATE,dbtnwH),ecdet="trend",type="eigen",K=2)
    summary(jojolevel)
    #Lags=3
    jojolevel<-ca.jo(cbind(G_RATE,dbtnwH),ecdet="trend",type="eigen",K=3)
    summary(jojolevel)
    # cointeg
    
    #TRACE
    jojolevTrace<-ca.jo(cbind(G_RATE,dbtnwH),ecdet="trend",type="trace",K=2)
    summary(jojolevTrace)
    #Lags=3
    jojolevTrace<-ca.jo(cbind(G_RATE,dbtnwH),ecdet="trend",type="trace",K=3)
    summary(jojolevTrace)
    ## RESULTS: Cointegration






#Test for "wrongly accept COINT" for struct. Break (Pfaff §8.2 AND Lütkepohl, H., Saikkonen, P. and Trenkler, C. (2004), )
#LEVEL
jojoStruct <- cajolst(cbind(LogRgdp,dbteq))
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
vniveauante <- window(vniveau,start=1951,end=1983)
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
jojopostBpt <- ca.jo(vniveaupostBpt[,(1:2)],ecdet="trend",type="trace") #,type="trace")
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
jojoAnteTrace<-ca.jo(vniveauante[,(1:2)],ecdet="trend",type="trace") #,type="trace")
summary(jojoAnteTrace)
###  NO COINT 
jojoAnteMax<-ca.jo(vniveau[,(1:2)],ecdet="trend") #,type="trace")
summary(jojoAnteMax)
###  NO COINT 

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
jojoBTW<-ca.jo(vniveaubtwn[,(1:2)],ecdet="trend",type="trace") #,type="trace")
summary(jojoBTW)
###  COINT 



#Test the 1983-2016 period for "wrongly accept COINT" for struct. Break (Pfaff §8.2 AND Lütkepohl, H., Saikkonen, P. and Trenkler, C. (2004), )
#LEVEL
jojoStr2007 <- cajolst(vniveaupostBpt[,1:2])
summary(jojoStr2007)
slot(jojoStr2007, "bp")
slot(jojoStr2007, "x")
slot(jojoStr2007, "x")[101] # corrsponding to 2008:1
## RESULTS: Cointegration is confirmed after data readjustment for break (2008,1)
#         i.e no worry about 2008 financial crisis




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
abline(v=1983.75, col="grey50")
### RGDP BREAK in 1983   ##
#
#Type= CUMsum
#Log(RGDP) & narrowing data range from 1973 on...
post73<-window(LogRgdp,start=1974)
efpCum_g73 <- efp(post73 ~ 1)
plot(efpCum_g73)
abline(v=1984.00, col="grey50")
### logRGDP BREAK in 1984   ###
#
## DEBT ##
### type= MOSUM
efpMo_d <- efp(dnw ~ 1, type = "Rec-MOSUM",h=0.053)#1980's break whithin 0.05-->0.07
# 0.053 --> 3 years
plot(efpMo_d)
abline(v=1965.75, col="grey50")
abline(v=1984.75, col="grey50")

# BREAK in 1965 then 1984 for dnw   #

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
fmgf <- lm(LogRgdp ~ breakfactor(bp.growth),breaks = 4)
plot(LogRgdp)
lines(ts(fitted(fmg0), start=c(1951.75)), col = 3)
lines(ts(fitted(fmgf), start = c(1951.75), frequency = 4), col = 4)
lines(bp.growth)
#LOG-rgdp PPOST 1973
bp.growth <- breakpoints(post73 ~ 1,breaks = 4)
summary(bp.growth)
fmg0 <- lm(post73 ~ 1)
fmgf <- lm(post73 ~ breakfactor(bp.growth),breaks = 4)
plot(post73)
lines(ts(fitted(fmg0), start=c(1974)), col = 3)
lines(ts(fitted(fmgf), start = c(1974), frequency = 4), col = 4)
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
<<<<<<< HEAD
# 

## Rgdp Growth RATE ##
#gdpCAP
fs.g_rate <- Fstats(G_RATE ~ 1)
plot(fs.g_rate)
breakpoints(fs.g_rate)
plot(G_RATE)
lines(breakpoints(fs.g_rate))
##Modulating the number of BP...
bp.g_rate <- breakpoints(G_RATE ~ 1,breaks = 5)
summary(bp.g_rate)
fmg0 <- lm(G_RATE ~ 1)
fmgf <- lm(G_RATE ~ breakfactor(bp.g_rate))#,breaks = 1))
plot(G_RATE)
lines(ts(fitted(fmg0), start=c(1800)), col = 3)
lines(ts(fitted(fmgf), start = c(1800,1), frequency = 1), col = 4)
lines(bp.g_rate)
# 

# SVAR --------------------------------------------------------------------


##LOG-rgdp
myvar <- data.frame(growth=LogRgdp, debt=dnw)
##LEVEL-rgdp
myvar <- data.frame(growth=window(gdpts,start=c(1951,4),end=c(2016,3)), debt=dnw)        
library(urca)
library(vars)
=======
  
  #LOGgdpCAP
  fs.lgpercap <- Fstats(log(gdpCAP) ~ 1)
plot(fs.lgpercap)
breakpoints(fs.lgpercap)
plot(log(gdpCAP))
lines(breakpoints(fs.lgpercap))
##Modulating the number of BP...
bp.lgpercap <- breakpoints(log(gdpCAP) ~ 1,breaks = 5)
summary(bp.lgpercap)
fmg0 <- lm(log(gdpCAP) ~ 1)
fmgf <- lm(log(gdpCAP) ~ breakfactor(bp.lgpercap))#,breaks = 1))
plot(log(gdpCAP))
lines(ts(fitted(fmg0), start=c(1800)), col = 3)
lines(ts(fitted(fmgf), start = c(1800,1), frequency = 1), col = 4)
lines(bp.lgpercap)
# 


# VAR --------------------------------------------------------------------

##LOG-rgdp
myvar <- data.frame(growth=LogRgdp, debt=dnw)
##LEVEL-rgdp
myvar <- data.frame(growth=window(gdpts,start=c(1951,4),end=c(2016,3)), debt=dnw)        
library(urca)
library(vars)


var_ts <- ts.intersect(LogRgdp, log(dbtnw))

var_list <- window(var_ts,start=c(1952,1), end=c(2015,1), frequency=4)
var_list <- window(var_ts,start=c(1984,1), end=c(2008,1), frequency=4)

var_data <- data.frame(gdp = (var_list[,1]), d=(var_list[,2]))

>>>>>>> 73536efe42245415bd30f0ec0f0928683b3594e6
# --------------------------------------------------------------------------- #
# ---------------------------- Reduced Form VAR ----------------------------- #

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
# Two roots are close to unity.


##### Residuals' Diagnostic tests ####

#SERIAL: Portmanteau- and Breusch-Godfrey test for serially correlated errors
serial.test(GvarD,lags.pt = 16,type = "PT.asymptotic")
serial.test(GvarD,lags.pt = 16,type = "PT.adjusted")

#JB: Jarque-Bera tests and multivariate skewness 
# and kurtosis tests for the residuals of a VAR(p) or of a VECM in levels.
normality.test(GvarD)
# Non-norm.

#ARCH: 
arch.test(GvarD,lags.multi = 5)
#Heteroscedastic resid.

#Stability : Recursive CUMSUM
plot(stability(GvarD),nc=2)
#

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

# Granger Causality -- -----------------------------------------------------

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

fevd.d <- fevd(svecm, n.ahead = 48)$dbtnw
fevd.d
#



# SVECM : Y=(rgdp, ii, d, r) ----------------------------------------------

#Consider 4 alternative specif°
        # 1-demand/supply seting relation (D=debt ; S=TotInv)  
                
        data_vecm1 <- ts.intersect(LogRgdp,
                                             (diff(IntInv+FinInv)/(IntInv+FinInv)), 
                                             (dbtnw),log(inv5))
        # 2-Etha (REMEMBER AND CHANGE Fii to ETHA)
        data_vecm1 <- ts.intersect(LogRgdp, (FinInv+IntInv)/(ProInv+IntInv+FinInv),
                                   dbtnw,log(inv5))
        
        # 3-FAP  
        data_vecm1 <- ts.intersect(LogRgdp,log(FinInv+IntInv), dbtnw,log(FAP))
        
        # 4-S&P INDEX  
        data_vecm1 <- ts.intersect(LogRgdp,log(FinInv+IntInv), dbtnw,log(INDEX)) 
        
        # 5-Investment & Profit  
        data_vecm1 <- ts.intersect(log(inv5),(log(FinInv+IntInv)), 
                                   log(dbtnw),log(profit1))
# 6- 30 aout  
data_vecm1 <- ts.intersect(LogRgdp,log(FinInv+IntInv), log(dbtot),log(ProInv)) 

data_list_w <- window(data_vecm1,start=c(1980,1), end=c(2015,1), frequency=4)
vecm_data6 <- data.frame(gdp = (data_list_w[,1]), fii = data_list_w[,2],
                         d=(data_list_w[,3]), inv = data_list_w[,4])

#1- ADF:  Ho=non-stat.  H1= diff-stat.
#2- KPSS:  Ho=stat.
#3- Phillips-Perron Ho: non-stat.
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
adf.test(diff(vecm_data6[,"fii"]))
kpss.test(diff(vecm_data6[,"fii"]))
pp.test(diff(vecm_data6[,"fii"]), type = "Z(t_alpha)")

adf.test(vecm_data6[,"d"])
kpss.test(vecm_data6[,"d"])
pp.test(vecm_data6[,"d"], type = "Z(t_alpha)")
adf.test(diff(vecm_data6[,"d"]))
kpss.test(diff(vecm_data6[,"d"]))
pp.test(diff(vecm_data6[,"d"]), type = "Z(t_alpha)")

adf.test(vecm_data6[,"inv"])
kpss.test(vecm_data6[,"inv"])
pp.test(vecm_data6[,"inv"], type = "Z(t_alpha)")
adf.test(diff(vecm_data6[,"inv"]))
kpss.test(diff(vecm_data6[,"inv"]))
pp.test(diff(vecm_data6[,"inv"]), type = "Z(t_alpha)")
pp.test(diff(vecm_data6[,"inv"], differences = 2), type = "Z(t_alpha)")


library(urca)

#Alternative test is "Elliott, Rothenberg and Stock...
#...(1996), which utilizes GLS detrending" p.167
# see Elliot & al. (1996, p.825) 
# for Critical values (-3.46 at 1% here)
# Ho=non-stat.
#Level
ur.ers(diff(vecm_data6[,"fii"]), model="const")
ur.ers(dnw)
#Diff
ur.ers(gd,model="trend")
ur.ers(dnwd,model="trend")



        VARselect(vecm_data6,lag.max = 8, type = "both")
        p1<-VAR(vecm_data6, p=5, type = "both")
        p2<-VAR(vecm_data6, p=2, type = "both")
        p7<-VAR(vecm_data6, p=3, type = "both")
        serial.test(p1,lags.pt = 16,type = "PT.asymptotic")
        serial.test(p1,lags.pt = 16,type = "PT.adjusted")
        serial.test(p2,lags.pt = 16,type = "PT.asymptotic")
        serial.test(p2,lags.pt = 16,type = "PT.adjusted")
        serial.test(p7,lags.pt = 16,type = "PT.asymptotic")
        serial.test(p7,lags.pt = 16,type = "PT.adjusted")
        normality.test(p1)
        normality.test(p2)
        normality.test(p7)
        arch.test(p1,lags.multi = 5)
        arch.test(p2,lags.multi = 5)
        arch.test(p7,lags.multi = 5)
        plot(stability(p1),nc=2)
        plot(stability(p2),nc=2)
        plot(stability(p7),nc=2)
        vecm_data6 <- vecm_data6[ , c("d","gdp","fii","inv")]
        vecm6 <- ca.jo(vecm_data6,ecdet="trend",K=4) #Alternative specif° #1 pass 1 coint. relat° at 5%
        summary(vecm6)
        vecm6.r1<-cajorls(vecm6,r=1)
        alpha6<-coef(vecm6.r1$rlm)[1,]
        beta6<-vecm6.r1$beta
        resids6<-resid(vecm6.r1$rlm)
        N6<-nrow(resids6)
        sigma <- crossprod(resids6) / N6
        alpha6.se<-sqrt(solve(crossprod(cbind(vecm6@ZK %*% beta6,
                                              vecm6@Z1))) [1,1]*diag(sigma))
        alpha6.t<-alpha6/alpha6.se
        beta6.se<-sqrt(diag(kronecker(solve(crossprod(vecm6@RK [,-1])),
                                      solve(t(alpha6) %*% solve(sigma) %*% alpha6))))
        beta6.t<-c(NA,beta6[-1]/beta6.se)
        alpha6
        alpha6.t
        beta6
        beta6.t
        svecm6<-SVEC(vecm6,LR=LR,SR=SR,r=1,lrtest=F,boot = T,runs = 100)
        svecm6
        svecm6$SR / svecm6$SRse
        svecm6$LR
        svecm6$LR / svecm6$LRse
        svecm6.irf<-irf(svecm6)
        plot(svecm6.irf)
        svecm6.irf<-irf(svecm6, n.ahead = 16)
        plot(svecm6.irf)
        svecm6.irf<-irf(svecm6, n.ahead = 40)
        plot(svecm6.irf)




#WATCHOUT : use alternatively data_list_w according to sample PERIODs

#52-2015
data_list_w <- window(data_vecm1,start=c(1952,1), end=c(2015,1), frequency=4)

#52 - 2007
data_list_w <- window(data_vecm1,start=c(1952,1), end=c(2008,1), frequency=4)

#52 - 1982
data_list_w <- window(data_vecm1,start=c(1952,1), end=c(1982,1), frequency=4)

#1985 - 2016
data_list_w <- window(data_vecm1,start=c(1980,1), end=c(2015,4), frequency=4)

#1984 - 2008
data_list_w <- window(data_vecm1,start=c(1984,1), end=c(2008,1), frequency=4)



vecm_data <- data.frame(gdp = (data_list_w[,1]), fii = data_list_w[,2],
                        d=(data_list_w[,3]), inv = data_list_w[,4])


# Descript. analysis
summary(data_list_w)
plot(data_list_w, nc=2)
plot(vecm_data, nc=2)

# Stationnarity tests
summary(ur.df(vecm_data[,"gdp"],type = "trend", lags = 2))
# Value of test-statistic is: -1.3864 while 
# Critical values = tau3 -3.98 -3.42 -3.13 
summary(ur.df(diff(vecm_data[,"gdp"]),type = "drift", lags = 1))
# Value of test-statistic is: -8.2791 while 
# Critical values = tau2 -3.44 -2.87 -2.57
summary(ur.df(vecm_data[,"fii"],type = "drift", lags = 1))
# Value of test-statistic is: -0.6672 while 
# Critical values = tau2 -3.44 -2.87 -2.57
summary(ur.df(diff(vecm_data[,"fii"]),type = "none", lags = 0))
# Value of test-statistic is: -13.6779 while 
# Critical values = tau1 -2.58 -1.95 -1.62


#1- ADF:  Ho=non-stat.  H1= diff-stat.
adf.test(vecm_data[,"gdp"])
adf.test(vecm_data[,"fii"])
adf.test(vecm_data[,"d"])
adf.test(vecm_data[,"inv"])

adf.test(diff(vecm_data[,"gdp"]))
adf.test(diff(vecm_data[,"fii"]))
adf.test(diff(vecm_data[,"d"]))
adf.test(diff(vecm_data[,"inv"]))

#2-KPSS:  Ho=stat.
kpss.test(vecm_data[,"inv"])
kpss.test(vecm_data[,"d"])
kpss.test(vecm_data[,"fii"])
kpss.test(vecm_data[,"gdp"])

kpss.test(diff(vecm_data[,"inv"]))
kpss.test(diff(vecm_data[,"d"]))
kpss.test(diff(vecm_data[,"fii"]))
kpss.test(diff(vecm_data[,"gdp"]))     

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

fevd.d <- fevd(svecm, n.ahead = 16)$dbtnw
fevd.d

###




# SVECM FINAL -------------------------------------------------------------

      data_vecm6 <- ts.intersect(LogRgdp,diff(FinInv+IntInv), 
                                 (dbtot/diff(AssetTot)),log(inv5)) 
      data_vecm6 <- ts.intersect(LogRgdp,log(FinInv+IntInv), 
            log(dbtot),log(ProInv)) 
      data_vecm6 <- ts.intersect(LogRgdp,log(FinInv+IntInv), 
                dbteq,log(ProInv)) 
      data_vecm6 <- ts.intersect(LogRgdp,log(FinInv+IntInv), 
                dbtnw,log(ProInv)) 
      data_vecm6 <- ts.intersect(LogRgdp,log(FinInv+IntInv), 
                dbtnwH,log(ProInv)) 
# 6- 30 aout  
data_vecm6 <- ts.intersect(LogRgdp,log(FinInv+IntInv), log(dbtot),log(inv5))
      data_vecm6 <- ts.intersect(LogRgdp,log(FinInv+IntInv), log(dbtot),log(ProInv))

data_list_w <- window(data_vecm6,start=c(1985,1), end=c(2015,1), frequency=4)
        data_list_w <- window(data_vecm6,start=c(1963,1), end=c(2015,1), frequency=4)
        data_list_w <- window(data_vecm6,start=c(1980,1), end=c(2015,1), frequency=4)
    data_list_w <- window(data_vecm6,start=c(1952,1), end=c(2015,1), frequency=4)

vecm_data6 <- data.frame(gdp = (data_list_w[,1]), fii = data_list_w[,2],
                         d=(data_list_w[,3]), inv = data_list_w[,4])

plot(data_list_w, nc=2)



VARselect(vecm_data6,lag.max = 8, type = "both")
  VARselect(vecm_data6,lag.max = 8, type = "n")
VARselect(vecm_data6,lag.max = 8, type = "c")
VARselect(vecm_data6,lag.max = 8, type = "trend")

    # VAR estimat° (p=1, 2 & 7)
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

#reorder data set for debt priority
  # 1-growth causes debt
    vecm_data6 <- vecm_data6[ , c("d","gdp","fii","inv")]
  # 2-demand/supply seting relation (D=debt ; S=TotInv)  
    vecm_data6 <- vecm_data6[ , c("gdp","d","fii","inv")]
    
vecm6 <- ca.jo(vecm_data6,ecdet="c",K=5)  #if ProIinv: K=(2,c):2coint    if inv5: K=(5,c): 1r
summary(vecm6)
    vecm6 <- ca.jo(vecm_data6,ecdet="t",K=2)  #if ProIinv: K=(2,t):1coint    if inv5: K=(5,t): 1r
      summary(vecm6)
    vecm6 <- ca.jo(vecm_data6,ecdet="c",K=3)  #1coint
      summary(vecm6)
    vecm6 <- ca.jo(vecm_data6,ecdet="t",K=3)  #NOcoint (only at 10%)
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
  #RESULT: 
        #ProInv:
            # K=(2,t) : g affects d ;   g affects fii   ;  fii neg-impact  inv
            # K=(3,c) : g affects d ;   g affects fii   ;  g impact inv (LT) ; fii impact g
            # K=(3,t) : g affects d 
#inv5:
  # K=(5,c) : g affects d (LT) ;   g affects fii (LT) ??   ; g affects inv  ;  fii impact  g  ;fii impact inv  ; 
          # K=(5,t) : ???
          
      # 2 coint relat°
          SR2<-matrix(NA,nrow = 4,ncol = 4)
          LR2<-matrix(NA,nrow = 4,ncol = 4)
          LR2[1:4,1:2]<-0
          SR2[3,2]<-0
          SR2[3,4]<-0
          LR2
          SR2
      svecm6<-SVEC(vecm6,LR=LR2,SR=SR2,r=2,lrtest=F,boot = T,runs = 100)
      #RESULT: K=(2,c) : g affects d ;   g affects fii   ;  fii neg-impact  inv 

    svecm6
    svecm6$SR / svecm6$SRse
    svecm6$LR
    svecm6$LR / svecm6$LRse
svecm6.irf<-irf(svecm6)
plot(svecm6.irf)
    svecm6.irf<-irf(svecm6, n.ahead = 16)
    plot(svecm6.irf)
    svecm6.irf<-irf(svecm6, n.ahead = 40)
    plot(svecm6.irf)

#DUMMY
#DEBTOT
d_1<- c(rep(0,120),rep(1,133)) #corresponding to 1982:Q1
d_post<- ts(d_1, start = c(1952,1),frequency = 4)
#DEBTEQ
d_1<- c(rep(0,105),rep(1,124)) #corresponding to 1984:50
d_post<- ts(d_1, start = c(1963,1), end=c(2015,1),frequency = 4)

VARselect(vecm_data6,lag.max = 8, type = "both", exogen =d_post)
p1<-VAR(vecm_data6, p=3, type = "c", exogen =d_post)
p2<-VAR(vecm_data6, p=7, type = "c", exogen =d_post)

vecm6 <- ca.jo(vecm_data6,ecdet="t",K=3, dumvar = d_post)
summary(vecm6)

# Fine TUNING -------------------------------------------------------------


# 6- 30 aout  
data_vecm6 <- ts.intersect(LogRgdp,log(FinInv+IntInv), 
                           log(dbtot),log(ProInv)) 
#dnw
data_vecm6 <- ts.intersect(LogRgdp,log(FinInv+IntInv),
                           log(dbtnw),log(inv5)) 
#inv5
data_vecm6 <- ts.intersect(LogRgdp,log(FinInv),
                           log(dbtnw),log(inv5)) 
#FinInv
data_vecm6 <- ts.intersect(LogRgdp,log(FinInv),
                           log(dbtnw), log(ProInv))

data_list_w <- window(data_vecm6,start=c(1980,1), end=c(2015,1), frequency=4)
vecm_data6 <- data.frame(gdp = (data_list_w[,1]), fii = data_list_w[,2],
                         d=(data_list_w[,3]), inv = data_list_w[,4])

VARselect(vecm_data6,lag.max = 8, type = "both")
p1<-VAR(vecm_data6, p=7, type = "both")
p2<-VAR(vecm_data6, p=8, type = "both")
p7<-VAR(vecm_data6, p=9, type = "both")
serial.test(p1,lags.pt = 16,type = "PT.asymptotic")
serial.test(p1,lags.pt = 16,type = "PT.adjusted")
serial.test(p2,lags.pt = 16,type = "PT.asymptotic")
serial.test(p2,lags.pt = 16,type = "PT.adjusted")
serial.test(p7,lags.pt = 16,type = "PT.asymptotic")
serial.test(p7,lags.pt = 16,type = "PT.adjusted")
normality.test(p1)
normality.test(p2)
normality.test(p7)
arch.test(p1,lags.multi = 5)
arch.test(p2,lags.multi = 5)
arch.test(p7,lags.multi = 5)
plot(stability(p1),nc=2)
plot(stability(p2),nc=2)
plot(stability(p7),nc=2)
vecm_data6 <- vecm_data6[ , c("d","gdp","fii","inv")]
vecm6 <- ca.jo(vecm_data6,ecdet="trend",K=8) #Alternative specif° #1 pass 1 coint. relat° at 5%
summary(vecm6)
vecm6.r1<-cajorls(vecm6,r=1)
alpha6<-coef(vecm6.r1$rlm)[1,]
beta6<-vecm6.r1$beta
resids6<-resid(vecm6.r1$rlm)
N6<-nrow(resids6)
alpha6.se<-sqrt(solve(crossprod(cbind(vecm6@ZK %*% beta6,
                                      vecm6@Z1))) [1,1]*diag(sigma))
alpha6.t<-alpha6/alpha6.se
beta6.se<-sqrt(diag(kronecker(solve(crossprod(vecm6@RK [,-1])),
                              solve(t(alpha6) %*% solve(sigma) %*% alpha6))))
beta6.t<-c(NA,beta6[-1]/beta6.se)
alpha6
alpha6.t
beta6
beta6.t

SR6<-matrix(NA,nrow = 4,ncol = 4)
LR6<-matrix(NA,nrow = 4,ncol = 4)
LR6[1:4,1]<-0
LR6[3,4]<-0
SR6[3,2]<-0
SR6[3,4]<-0

svecm6<-SVEC(vecm6,LR=LR6,SR=SR6,r=1,lrtest=F,boot = T,runs = 1000)
svecm6
svecm6$SR / svecm6$SRse
svecm6$LR
svecm6$LR / svecm6$LRse
svecm6.irf<-irf(svecm6)
plot(svecm6.irf)
svecm6.irf<-irf(svecm6, n.ahead = 16)
plot(svecm6.irf)
svecm6.irf<-irf(svecm6, n.ahead = 40)
plot(svecm6.irf)






# OLDIES ------------------------------------------------------------------


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


### OLD - Struct break 


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

