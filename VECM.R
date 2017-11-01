setwd("C:/Users/Ferdi/Documents/R/WD")
library(tseries)
# Read data
RealGrowth <- read.csv("Z1_RGDP_3Dec_GDPC96.csv", head = TRUE, sep=",")
DebtToNw <- read.csv("Z1_NFCBusiness_creditMarket_Debt_asPercentageof_NetWorth.csv",
                     head = TRUE, sep=",")

# Make Time Series of Debt & Gdp objects
gts <- ts(RealGrowth$GDPC96, start = c(1947,1),frequency = 4)
dts2 <- ts(DebtToNw$NCBCMDPNWMV, start = c(1951,4),frequency = 4)

# Bind the series into a list
vniveau2<-ts.intersect(gts,dts2,lag(gts,k=-1),lag(dts2,lag=-1))
vlog2<-log(vniveau2)
vdiff_n2<-diff(vniveau2)
vdiff_l2<-diff(vlog2)

# plot level & Diff-lev
par(mfrow=c(2,2))
ts.plot(g<-vniveau2[,1], ylab="RGDP (level)")
ts.plot(dnw<-vniveau2[,2], ylab="Debt/net worth")
# plot of diff_Debt_level is informative of
# the transformation in debt dynamics occuring from mid 1980's
ts.plot(gd<-vdiff_n2[,1], ylab="RGDP (diff.)")
ts.plot(dnwd<-vdiff_n2[,2], ylab="Debt/net worth (diff.)")

# plot log & Diff-log
ts.plot(gl<-vlog2[,1])
ts.plot(dnwl<-vlog2[,2])
ts.plot(gld<-vdiff_l2[,1])
ts.plot(dnwld<-vdiff_l2[,2])
par(mfrow=c(1,1))


#glag<-vlog2[,3]
#dnwlag<-vlog2[,4]

# like Kleiber p.165: 2 ways to adress Stationarity :
  #1- ADF:  Ho=non-stat.  H1= diff-stat.
  #2-KPSS:  Ho=stat.

# LEVELS
adf.test(g)
adf.test(gd)
adf.test((dnw))
adf.test((dnwd))
# g: non-stat. but is diff-stat.
# dnw: non-stat. but is diff-stat.

# LOGs
adf.test(gl)
adf.test(gld)
adf.test(dnwl)
adf.test(dnwld)
# g: non-stat. but is diff-stat.
# dnw: non-stat. but is diff-stat

# Phillips-Perron test for stationarity
  # Ho: non-stat.
pp.test(gl, type = "Z(t_alpha)")
pp.test(dnwl, type = "Z(t_alpha)")
  # Diff
pp.test(gld, type = "Z(t_alpha)")
pp.test(dnwld, type = "Z(t_alpha)")
# CONFIRM ADF results of non-stat. in log-variables

# alternative test is "Elliott, Rothenberg and Stock...
  #...(1996), which utilizes GLS detrending" p.167
  # see Elliot & al. (1996, p.825) for Critical values (-3.46 at 1% here)
library(urca)
ur.ers(gl, model="const")
ur.ers(dnwl)
  # Diff
ur.ers(gld,model="trend")
ur.ers(dnwld,model="trend")
#also CONFIRM ADF & PP results (only at 2.5% for Debt)

#2- KPSS
  # Ho: stationnarity
  # LOG
kpss.test(gl,null = "Trend")
kpss.test(dnwl,null = "Trend") #both reject Ho of stat.
  # Diff ; no-trend
kpss.test(gld)
kpss.test(dnwld) # Debt not diff-stat. --> try including a trend:
  # Diff Trend
kpss.test(gld,null = "Trend")
kpss.test(dnwld,null = "Trend")
#also CONFIRM ADF, PP & ERS results.


###  NOTE that ADF,PP,ERS are UnitRoot test
#       while KPSS is Stationarity test


# WATCHOUT !!          ##
# do UR test again to
# consider time trends
# versus constant
# of deterministic
# component           ##



### COINTEGRAT? #####

# Elliot &. also have cointegr?

# Phillips Ouliaris test
#   Ho: no cointegrat?

# LOG
po.test(vlog2[,1:2], demean = T, lshort = T)      # No COINT
po.test(vlog2[,2:1], demean = T, lshort = T) # neither the other way round
# LEVELS
po.test(vniveau2[,1:2], demean = T, lshort = T)      # No COINT
po.test(vniveau2[,2:1], demean = T, lshort = T) # neither the other way round



## RESULTS: NO Cointegration


# Johansen test
#   Ho: no cointegrat? (r=0 against r>0 , then r<=1 against r>1 etc..)
  # A rank r>0 implies a cointegrating relationship
    # between two or possibly more time series
jojo<-ca.jo(vlog2[,(1:2)])#,ecdet="const",type="trace")
summary(jojo)
# NO COINT in log ##

jojolevel<-ca.jo(vniveau2[,(1:2)],ecdet="const") #,type="trace")
summary(jojolevel)
  # Ho: "no cointeg?" is rejected at 1% --> possible cointeg? for r<=1
        #  for r<=1: tstat < crit.val. --> cointegration btw (x+1) variables
jojolevTrace<-ca.jo(vniveau2[,(1:2)],ecdet="const",type="trace")
summary(jojolevTrace)
## RESULTS: possible Cointegration in Levels
#

# test for "wrongly accept COINT" for struct. Break (Pfaff ?8.2)
jojoStruct <- cajolst(vniveau2[,(1:2)])
slot(jojoStruct, "bp")
slot(jojoStruct, "x")
slot(jojoStruct, "x")[126] # corrsponding to 1983
## RESULTS: NO Cointegration once break accounted for (1983,1)
#         i.e there maybe coint just becausz of struct shift

# Thus, test COINT from 1983 to 2016
vniveau2postBpt <- window(vniveau2,start=1983,end=2016)
jojopostBpt <- ca.jo(vniveau2postBpt[,(1:2)],ecdet="const",type="trace") #,type="trace")
summary(jojopostBpt)
## RESULTS: COINT at 1% from 1983 on !!!
#           i.e one may estimate a VECM for G&D
#           goto SVAR section



## DIVIDE THE SAMPLE FOR FINANCIALIZATION ACCOUNT
vniveau2ante <- window(vniveau2,start=1951,end=1985)
vniveau2btwn <- window(vniveau2,start=1985,end=2007)

## ANTE FIN°
po.test(vniveau2ante[,(1:2)], demean = T, lshort = T)      # No COINT
po.test(vniveau2ante[,2:1], demean = T, lshort = T) # neither the other way round

jojoante<-ca.jo(vniveau2ante[,(1:2)],ecdet="const",type="trace") #,type="trace")
summary(jojoante)
### umbiguous COINT (G.E nocoint but JO coint)

## POST FIN°  (including 2007 crisis aftermath)
po.test(vniveau2post[,(1:2)], demean = T, lshort = T)      # No COINT
po.test(vniveau2post[,2:1], demean = T, lshort = T) # neither the other way round
# NO COINT
jojopost<-ca.jo(vniveau2post[,(1:2)],ecdet="const") #,type="trace")
summary(jojopost)
### ambiguous COINT

## Post Fin°  (excluding post 2007 crisis)
po.test(vniveau2btwn[,(1:2)], demean = T)      # No COINT
po.test(vniveau2btwn[,2:1], demean = T) # neither the other way round
# NO COINT
jojobtwn<-ca.jo(vniveau2btwn[,(1:2)],ecdet="const") #,type="trace")
summary(jojobtwn)
# COINTEGRATION for Johanson



#### Structural break tests ##########



library(strucchange)

# StrBrk_1 : EFP ----------------------------------------------------------
# EFP = empirical fluct? process
# Ho: no struct. break (Kleiber p.171)

####### GROWTH ########


  # set list for Alternative regression fits
                   #1 #2  #3            #4
vbn21<-ts.intersect(g,gd,lag(g,k=-1),lag(gd,k=-1))
vbn22<-ts.intersect(gl,gld,lag(gl,k=-1),lag(gld,k=-1))
                    #1  #2   #3            #4

  ### DIFF.
efp_gd <- efp(gd ~ 1)   #vbn21[,2]
plot(efp_gd)
  ###  BREAK in early 1990's ###
  #        for g.diff          #


  ### REcursive CUMSUM
efpCum_g <- efp(g ~ 1)     #vbn21[,1]
plot(efpCum_g)  # break in 1973 oil crisis
# narrowing data range from 1973 on...
post73<-window(g,start=1974)
efpCum_g73 <- efp(post73 ~ 1)
plot(efpCum_g73)

###  BREAK in end 1980's   ###
#        for g               #



#### BUT with type= Rec. MOSUM
######  got break in 1980's
#####     for ALL DATA RANGE  ######
efpMo_g <- efp(g ~ 1, type = "Rec-MOSUM",h=0.053)#1980's break whithin 0.05-->0.07
            # 0.053 --> 3 years
plot(efpMo_g)
efpMo_g73 <- efp(post73 ~ 1, type = "Rec-MOSUM",h=0.087)#1980's break whithin 0.08-->0.1
# 0.087 --> 3 years
plot(efpMo_g73)


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
### Break in mid 90's #####




####  DEBT    #####

### REcursive CUMSUM
efpCum_d <- efp(dnwl ~ 1)     #vbn21[,1]
plot(efpCum_d)  # break in 1973 oil crisis
# R.A.S

# narrowing data range from 1973 on...
dpost73<-window(dnwl,start=1974)
efpCum_d73 <- efp(dpost73 ~ 1)
plot(efpCum_d73)

###  BREAK in mid 1980's   ###
#     then 90's for dnw      #


# Idem with type= Rec. MOSUM
efpMo_d <- efp(dnwl ~ 1, type = "Rec-MOSUM",h=0.053)#1980's break whithin 0.05-->0.07
# 0.053 --> 3 years
plot(efpMo_d)
efpMo_d73 <- efp(dpost73 ~ 1, type = "Rec-MOSUM",h=0.087)#1980's break whithin 0.08-->0.1
# 0.087 --> 3 years
plot(efpMo_d73)

###  BREAK from mid 1980's   ###
#                           ##


# struct. break when out of conf. interv.
  # struc. br. if p.value<conf.int.  i.e reject Ho
sctest(efpCum_g)
sctest(efpCum_g73)
sctest(efpMo_g)
sctest(efpMo_g73)
sctest(sc_efp2_g)


# struct. breaks found for diff(realGDP)
plot(efpCum_g73)
plot(efpMo_g)
plot(efpCum_g73)
plot(sc_efp2_g)

plot(efpCum_g73, alpha = 0.05, alt.boundary = TRUE)




# StrBrk_2 : Fstat --------------------------------------------------------

#2- Fstat = possible breaks ##


#### levels  ####

  #### GROWTH

fs.growth <- Fstats(gl ~ 1+time(g))
plot(fs.growth)
breakpoints(fs.growth)
plot(g)
lines(breakpoints(fs.growth))
###    Break: 1992.2     #####
bp.growth <- breakpoints(gl ~ 1+time(g),breaks = 1)
summary(bp.growth)
fmg0 <- lm(gl ~ 1+time(g))
fmgf <- lm(gl ~ breakfactor(bp.growth))#,breaks = 1))
plot(gl)
lines(ts(fitted(fmg0), start=c(1951)), col = 3)
lines(ts(fitted(fmgf), start = c(1951,4), frequency = 4), col = 4)
lines(bp.growth)

###  Sample OUT of 2007 crisis  ####
####################################  window(g,start=1952,end=2008)
g2007<-window(g,start=1952,end=2008)
bp.growth2007 <- breakpoints(g2007 ~ 1,breaks = 1)
summary(bp.growth2007)
fmg0 <- lm(g2007 ~ 1)
fmgf <- lm(g2007 ~ breakfactor(bp.growth2007))#,breaks = 1))
plot(g2007)
lines(ts(fitted(fmg0), start=c(1951)), col = 3)
lines(ts(fitted(fmgf), start = c(1951,4), frequency = 4), col = 4)
lines(bp.growth2007)

###    Break: 1987.1    #####



###### DEBT Level #######
fs.debt2 <- Fstats(dnw ~ 1)
plot(fs.debt2)
plot(dnw)
breakpoints(fs.debt2)
lines(breakpoints(fs.debt2))

###    Break: 1985.2     #####

bp.debt2 <- breakpoints(dnw ~ 1,breaks = 1)
summary(bp.debt2)

fmdnw0 <- lm(dnw ~ 1)
fmdnwf <- lm(dnw ~ breakfactor(bp.debt2))#,breaks = 1))
plot(dnw)
lines(ts(fitted(fmdnw0), start=c(1951)), col = 3)
lines(ts(fitted(fmdnwf), start = c(1951,4), frequency = 4), col = 4)
lines(bp.debt2)



####  LOG  ####

Lfs.growth <- Fstats(gl ~ 1)
plot(Lfs.growth)
breakpoints(Lfs.growth)
plot(gl)
lines(breakpoints(Lfs.growth))

###    Break: 1983.3     #####

Lbp.growth <- breakpoints(gl ~ 1,breaks = 1)
summary(Lbp.growth)
fmg0 <- lm(gl ~ 1)
fmgf <- lm(gl ~ breakfactor(Lbp.growth))#,breaks = 1))
plot(gl)
lines(ts(fitted(fmg0), start=c(1951)), col = 3)
lines(ts(fitted(fmgf), start = c(1951,4), frequency = 4), col = 4)
lines(Lbp.growth)
####################################  window(g,start=1952,end=2008)
gl2007<-window(gl,start=1955,end=2008)
bp.growthl2007 <- breakpoints(gl2007 ~ 1,breaks = 1)
summary(bp.growthl2007)


  ###### DEBT log #######
Lfs.debt2 <- Fstats(dnwl ~ 1)
plot(Lfs.debt2)
plot(dnwl)
breakpoints(Lfs.debt2)
lines(breakpoints(Lfs.debt2))

###    Break: 1985.2     #####

Lbp.debt2 <- breakpoints(dnwl ~ 1,breaks = 2)
summary(Lbp.debt2)

fmdnw0 <- lm(dnwl ~ 1)
fmdnwf <- lm(dnwl ~ breakfactor(Lbp.debt2))#,breaks = 1))
plot(dnwl)
lines(ts(fitted(fmdnw0), start=c(1951)), col = 3)
lines(ts(fitted(fmdnwf), start = c(1951,4), frequency = 4), col = 4)
lines(Lbp.debt2)



# SVAR --------------------------------------------------------------------


myvar <- data.frame(growth=g, debt=dnw)
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
  datapostBP <- data.frame(gpostBP=vniveau2postBpt[,1], dpostBP=vniveau2postBpt[,2])
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
gLS<-window(vniveau2[,1],start=1962,end=1998)
dLS<-window(vniveau2[,2],start=1962,end=1998)
myvarLS<-cbind(gLS,dLS)
VARselect(myvarLS, lag.max = 6, type = "both")
varLS <- VAR(myvarLS, p = 2, type = "both")
print(causality(varLS, cause="gLS"))
print(causality(varLS, cause="dLS"))
# L&Secca results are partially confirmed :
  # no causality from G --> D
  # nor the o-w-round

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



