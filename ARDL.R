# Empirical Macro Model of my THESIS
#
setwd("C:/Users/fouwe/Documents/R/")

library(tseries)
library(vars)
library(lmtest)
library(urca)
library(ardl)
## library(gvlma)

# Data --------------------------------------------------------------------

# Read data
  invraw1 <- read.csv("INV1_Accumulation_ofProducedAssetsGrossFixed.csv",head = TRUE, sep=",")
  invraw2 <- read.csv("INV2_FixedPrivateInvestment.csv",head = TRUE, sep=",")
  invraw3 <- read.csv("INV3_RealPrivateFixedInvestment.csv", head = TRUE, sep=",")
  invraw4 <- read.csv("INV4_GrossPrivateDomesticInvestment.csv",head = TRUE, sep=",")
  invraw5 <- read.csv("INV5_RealGrossPrivateDomesticInvestment.csv",head = TRUE, sep=",")
  invraw6 <- read.csv("INV6_Nominal Investment__GPDIplusPCE.csv", head = TRUE, sep=",")
  
  profraw1 <- read.csv("ProfitsAfterTax.csv", head = TRUE, sep=",")
  profraw2 <- read.csv("ProfitsBeforeTax.csv", head = TRUE, sep=",")
  
  uraw1 <- read.csv("Capacity1_Utilization_Manuf.csv", head = TRUE, sep=",")
  uraw2 <- read.csv("Capacity2_Utilization_Manuf_exc_computer.csv", head = TRUE, sep=",")
  uraw3 <- read.csv("Capacity3_UtilizationManufNAICS.csv", head = TRUE, sep=",")

#All FinAsset (including UNIDENTIFIED)
  fininvraw <- read.csv("FinInv.csv", skip = 1,head = TRUE, sep=",")
#ONLY Identified Financial assets
  finInvIndeed <- read.csv("PURGED_FININV.csv", skip = 2,head = TRUE, sep=",")
#UnIdentified Financial assets
  intanginv <- read.csv("IntangibleInv.csv", skip = 1,head = TRUE, sep=",")
  
  
  DebtToNw <- read.csv("Z1_NFCBusiness_creditMarket_Debt_asPercentageof_NetWorth.csv",
                       head = TRUE, sep=",")
  DebtToEq <- read.csv("Z1_NFCBusiness_CreditMarketDebtAsPercentageOfMarketValueOfCorpEquities.csv",
                       head = TRUE, sep=",")
  #
# Make Time Series of data
  inv1 <- ts(invraw1$K160071A027NBEA, start = c(1951,1),end = c(2016,4),frequency = 4)
  inv2 <- ts(invraw2$FPI, start = c(1947,1),frequency = 4)
  inv3 <- ts(invraw3$FPIC1, start = c(1999,1),frequency = 4)
  inv4 <- ts(invraw4$GPDI, start = c(1947,1),frequency = 4)
  inv5 <- ts(invraw5$GPDIC1, start = c(1947,1),end = c(2016,4),frequency = 4)
  inv6 <- ts(invraw6$NINV, start = c(1951,1),frequency = 4)
  
  profit1 <- ts(profraw1$NFCPATAX, start = c(1947,1),end = c(2016,4),frequency = 4)
  profit2 <- ts(profraw2$A464RC1Q027SBEA, start = c(1947,1),frequency = 4)
  
  capu1 <- ts(uraw1$CAPUTLB00004SQ, start = c(1948,1),end = c(2016,4),frequency = 4)
  capu2 <- ts(uraw2$CAPUTLX4HTK2SQ, start = c(1967,1),frequency = 4)
  capu3 <- ts(uraw3$CUMFN, start = c(1972,1),frequency = 4)

#2 alternative FinInv :
  #1st. is Total Fin Asset from Z1
    TotFinInv <- ts(fininvraw$fininv, start = c(1951,4),end = c(2016,4),frequency = 4)
  #2nd. is financialy "pure" Fin Assets
    FinInv <- ts(finInvIndeed$FININDEED, start = c(1951,4),
               end = c(2015,1),frequency = 4)
  
#INTANIBLE INVESTMENT SERIES
  IntInv <- ts(intanginv$intinv, start = c(1951,4),
               end = c(2015,1),frequency = 4)
  
FinInvHistRatio <- ts(fininvraw$hfininvratio, start = c(1951,4),end = c(2016,4),frequency = 4)
FinInvRatio <- ts(fininvraw$fininvratio, start = c(1951,4),frequency = 4)
AssetTot <- ts(fininvraw$totinv, start = c(1951,4),frequency = 4)

dbtnw <- ts(DebtToNw$NCBCMDPNWMV, start = c(1951,4),end = c(2016,4),frequency = 4)
dbteq <- ts(DebtToEq$NCBCMDPMVCE, start = c(1951,4),frequency = 4)

#
# PLOT data
  old.par <- par(mfrow=c(1,1))
  pdf("C5_Graph.pdf")
  #
  par(mfrow=c(3,2))
  ts.plot(inv1)
  ts.plot(inv2)
  ts.plot(inv3)
  ts.plot(inv4)
  ts.plot(inv5)
  lines(TotFinInv)
  lines(FinInv)
  ts.plot(inv6)
  par(mfrow=c(3,1))
  ts.plot(capu1)
  ts.plot(capu2)
  # ts.plot(capu3)
  ts.plot(FinInv)
  ts.plot(FinInvRatio)
  ts.plot(FinInvHistRatio)
  ts.plot(AssetTot)
  par(mfrow=c(2,1))
  ts.plot(profit1)
  ts.plot(profit2)
  ts.plot(dbtnw)
  ts.plot(dbteq)
  # restore par
  par(old.par)
  dev.off()
  #

# Stationarity ------------------------------------------------------------

#1- ADF:  Ho=non-stat.  H1= diff-stat.
#2-KPSS:  Ho=stat.

# LEVELS
  adf.test(inv5)
  adf.test(capu1)
  adf.test(profit1)
  adf.test(FinInv)
  adf.test(FinInvRatio)
  adf.test(FinInvHistRatio)
  adf.test(dbtnw)
  adf.test(dbteq)
  # capu1 I(0)

# 1st. Diff
  adf.test(diff(inv5))
  adf.test(diff(profit1))
  adf.test(diff(FinInv))
  adf.test(diff(FinInvRatio))
  adf.test(diff(FinInvHistRatio))
  adf.test(diff(dbtnw))
  adf.test(diff(dbteq))
  # all I(1)

#Create LIST of 5 variables (i-u-r-Fi-D)
  #1st. ALTERNATIVE --->  (i,r,Fi) = NOMINAL level
    #eqlist<- ts.intersect(inv5,capu1,profit1,FinInv,dbtnw)
    #After Peter's comment, I change 3 variables (Inv, Profit, FinInv)
  #2nd ALTERNATIVE --->  (i,r,Fi) = LOGARITHM
    #eqlist<- ts.intersect(log(inv5),capu1,log(profit1),
    #log(FinInv),dbtnw)
  #3rd ALTERNATIVE --->  (i,r,Fi) = RATIO of Total NFC Assets
    eqlist<- ts.intersect((inv5/AssetTot),capu1,(profit1/AssetTot),
                          (FinInv/AssetTot),dbtnw)
  #4th ALTERNATIVE --->  (i,r,Fi,Ii) = RATIO of Total NFC Assets
    data_list<- ts.intersect((inv5/AssetTot),capu1,(profit1/AssetTot),
                        (FinInv/AssetTot),(IntInv/AssetTot),dbtnw)
  # ++ ALTERNATIVE --->  FinInvRatio
  #eqlist<- ts.intersect(inv5,capu1,profit1,FinInvRatio,dbtnw)
  # ++ ALTERNATIVE --->  FinInvHistRatio
  #eqlist<- ts.intersect(inv5,capu1,profit1,FinInvHistRatio,dbtnw)

#Create LIST of 5X5 variables (i-u-r-Fi-D)x(level-diff-lagLevel-2xlagDiff)
  difeqlist <- diff(eqlist)
  lageqlist <- lag(eqlist, k=-1)
  ld1eqlist <- lag(difeqlist, k=-1)
  ld2eqlist <- lag(difeqlist, k=-2)
  #ld3eqlist <- lag(difeqlist, k=-3)
  #ld4eqlist <- lag(difeqlist, k=-4)
  ecmeq <- ts.intersect(eqlist, difeqlist, lageqlist,ld1eqlist,ld2eqlist)

#Create a simpler list of i - u - r - Fi - Ii - D  (no Lag no Diff)
  
  
# SPLIT SAMPLE ACCORDING TO FIN°
ecmeqante <- window(ecmeq,start=1951,end=1984)
ecmeqpost <- window(ecmeq,start=1984,end=2016)
ecmeqbtwn <- window(ecmeq,start=1984,end=2008)


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

# Bilateral COINT ---------------------------------------------------------


# 1952-2016 *****
# LOOPING coint btw varibles
for(i in 1:4){
  j<-i
  while(j<5){
    j<-j+1
    johansen <- ca.jo(cbind(ecmeq[,i],ecmeq[,(j)])) #,ecdet="const",type="trace")
    print(summary(johansen))
    cat("Coint ",i,"-",j,"   **************************")
    wait<-readline("OK ?")
  }
  cat("\n")
}

# Split sample FOR FINANCIALIZATION ACCOUNT

# 1952-1985 *****
for(i in 1:4){
  j<-i
  while(j<5){
    j<-j+1
    johansen <- ca.jo(cbind(ecmeqante[,i],ecmeqante[,(j)])) #,ecdet="const",type="trace")
    print(summary(johansen))
    cat("Coint ",i,"-",j,"   **************************")
    wait<-readline("OK ?")
  }
  cat("\n")
}

# 1985-2016 *****
#pdf("COINT-Post_85-2016.pdf")
for(i in 1:4){
  j<-i
  while(j<5){
    j<-j+1
    johansen <- ca.jo(cbind(ecmeqpost[,i],ecmeqpost[,(j)])) #,ecdet="const",type="trace")
    print(summary(johansen))
    cat("Coint ",i,"-",j,"   **************************")
    wait<-readline("OK ?")
  }
  cat("\n")
}
#dev.off()


# 1985-2007 *****
for(i in 1:4){
  j<-i
  while(j<5){
    j<-j+1
    johansen <- ca.jo(cbind(ecmeqbtwn[,i],ecmeqbtwn[,(j)])) #,ecdet="const",type="trace")
    print(summary(johansen))
    cat("Coint ",i,"-",j,"   **************************")
    wait<-readline("OK ?")
  }
  cat("\n")
}




# TEST LOOP IN coint
for(i in 1:4){
  j<-i
  while(j<5){
    j<-j+1
    johansen <- ca.jo(cbind(ecmeq[,i],ecmeq[,(j)])) #,ecdet="const",type="trace")
    print(summary(johansen))
    cat("Coint ",i,"-",j,"   **************************")
    wait<-readline("OK ?")
  }
  cat("\n")
}


# Lag selection -----------------------------------------------------------

# 2 methods (ARDL Chapter p.54)
# 1- Informat° Criteria
# 2- iid residuals

# 1- Schwarz (Bayes) Criterion
VARselect((ecmeq[,6]),lag.max = 8, type = "both",
          exogen = cbind(ecmeq[,7],ecmeq[,8],ecmeq[,9],
                         ecmeq[,11], ecmeq[,12],
                         ecmeq[,13], ecmeq[,14]))
# diff(capu)t + diff(profit)t + diff(fininv)t
# min(SC=7.398314) : 2 lags for  diff(inv)      7.316285

VARselect((ecmeq[,6]),lag.max = 7, type = "both",
          exogen = cbind(ecmeq[,11], ecmeq[,12],ecmeq[,13], ecmeq[,14],
                         ecmeq[,7],ecmeq[,8], ecmeq[,9],
                         ecmeq[,16]))
# diff(capu)t
# min(SC=7.455427) :  1 lag for  diff(inv)

VARselect((ecmeq[,6]),lag.max = 8, type = "both",
          exogen = cbind(ecmeq[,8],
                         ecmeq[,11], ecmeq[,12],
                         ecmeq[,13], ecmeq[,14]))
# diff(profit)t
# min(SC=7.615878) : 2 lags for  diff(inv)

VARselect((ecmeq[,6]),lag.max = 8, type = "both",
          exogen = cbind(ecmeq[,9],
                         ecmeq[,11], ecmeq[,12],
                         ecmeq[,13], ecmeq[,14]))
# diff(fininv)t
# min(SC=7.750497) : 2 lags for  diff(inv)

VARselect((ecmeq[,6]),lag.max = 8, type = "both",
          exogen = cbind(ecmeq[,7],ecmeq[,8],
                         ecmeq[,11], ecmeq[,12],
                         ecmeq[,13], ecmeq[,14]))
# diff(capu)t + diff(profit)t
# min(SC=7.380591) : 2 lags for  diff(inv)

VARselect((ecmeq[,6]),lag.max = 8, type = "both",
          exogen = cbind(ecmeq[,7],ecmeq[,9],
                         ecmeq[,11], ecmeq[,12],
                         ecmeq[,13], ecmeq[,14]))
# diff(capu)t + diff(fininv)t
# min(SC=7.473296) : 2 lags for  diff(inv)

VARselect((ecmeq[,6]),lag.max = 8, type = "both",
          exogen = cbind(ecmeq[,8],ecmeq[,9],
                         ecmeq[,11], ecmeq[,12],
                         ecmeq[,13], ecmeq[,14]))
# diff(profit)t + diff(fininv)t
# min(SC=7.632944) : 2 lags for  diff(inv)



# -- round 2 --------------------------------------------------------------



VARselect((ecmeq[,6]),lag.max = 8, type = "both",
          exogen = cbind(ecmeq[,7],ecmeq[,8],ecmeq[,9],
                         ecmeq[,17],ecmeq[,18],ecmeq[,19],
                         ecmeq[,11], ecmeq[,12], ecmeq[,13], ecmeq[,14]))
## diff(I)t-1 + diff(I)t-2
# diff(capu)t + diff(profit)t + diff(fininv)t
# diff(capu)t-1 + diff(profit)t-1 + diff(fininv)t-1
# min(SC=7.431313 ) : 2 lags for  diff(inv)

VARselect((ecmeq[,6]),lag.max = 8, type = "both",
          exogen = cbind(ecmeq[,7],ecmeq[,8],
                         ecmeq[,17],ecmeq[,18],
                         ecmeq[,11], ecmeq[,12], ecmeq[,13], ecmeq[,14]))
## diff(I)t-1 + diff(I)t-2
# diff(capu)t + diff(profit)t
# diff(capu)t-1 + diff(profit)t-1
# min(SC=7.396523 ) : 2 lags for  diff(inv)


VARselect((ecmeq[,6]),lag.max = 8, type = "both",
          exogen = cbind(ecmeq[,7],ecmeq[,8],ecmeq[,9],
                         ecmeq[,17],
                         ecmeq[,11], ecmeq[,12], ecmeq[,13], ecmeq[,14]))
## diff(I)t-1 + diff(I)t-2
## diff(capu)t + diff(profit)t + diff(fininv)t
# diff(capu)t-1
# min(SC=7.415717 ) : 2 lags for  diff(inv)

VARselect((ecmeq[,6]),lag.max = 8, type = "both",
          exogen = cbind(ecmeq[,7],ecmeq[,8],
                         ecmeq[,17],
                         ecmeq[,11], ecmeq[,12], ecmeq[,13], ecmeq[,14]))
## diff(I)t-1 + diff(I)t-2
## diff(capu)t + diff(profit)t
# diff(capu)t-1
# min(SC=7.399642 ) : 2 lags for  diff(inv)

VARselect((ecmeq[,6]),lag.max = 8, type = "both",
          exogen = cbind(ecmeq[,7],ecmeq[,8],ecmeq[,9],
                         ecmeq[,18],
                         ecmeq[,11], ecmeq[,12], ecmeq[,13], ecmeq[,14]))
## diff(I)t-1 + diff(I)t-2
## diff(capu)t + diff(profit)t + diff(fininv)t
# diff(profit)t-1
# min(SC=7.390325 ) : 2 lags for  diff(inv)                 7.309134

VARselect((ecmeq[,6]),lag.max = 8, type = "both",
          exogen = cbind(ecmeq[,7],ecmeq[,8],
                         ecmeq[,18],
                         ecmeq[,11], ecmeq[,12], ecmeq[,13], ecmeq[,14]))
## diff(I)t-1 + diff(I)t-2
## diff(capu)t + diff(profit)t
# diff(profit)t-1
# min(SC=7.374593 ) : 2 lags for  diff(inv)


VARselect((ecmeq[,6]),lag.max = 8, type = "both",
          exogen = cbind(ecmeq[,7],ecmeq[,8],ecmeq[,9],
                         ecmeq[,19],
                         ecmeq[,11], ecmeq[,12], ecmeq[,13], ecmeq[,14]))
## diff(I)t-1 + diff(I)t-2
## diff(capu)t + diff(profit)t + diff(fininv)t
# diff(capu)t-1 + diff(profit)t-1 + diff(fininv)t-1
# min(SC=7.412899 ) : 2 lags for  diff(inv)

VARselect((ecmeq[,6]),lag.max = 8, type = "both",
          exogen = cbind(ecmeq[,9],
                         ecmeq[,19],
                         ecmeq[,11], ecmeq[,12], ecmeq[,13], ecmeq[,14]))
## diff(I)t-1 + diff(I)t-2
# diff(fininv)t
# diff(fininv)t-1
# min(SC= ) : 2 lags for  diff(inv)



VARselect((ecmeq[,6]),lag.max = 8, type = "both",
          exogen = cbind(ecmeq[,7],ecmeq[,8],ecmeq[,9],
                         ecmeq[,17],ecmeq[,18],
                         ecmeq[,11], ecmeq[,12], ecmeq[,13], ecmeq[,14]))
## diff(I)t-1 + diff(I)t-2
## diff(capu)t + diff(profit)t + diff(fininv)t
# diff(capu)t-1 + diff(profit)t-1
# min(SC=7.412397 ) : 2 lags for  diff(inv)


VARselect((ecmeq[,6]),lag.max = 8, type = "both",
          exogen = cbind(ecmeq[,7],ecmeq[,8],ecmeq[,9],
                         ecmeq[,17],ecmeq[,19],
                         ecmeq[,11], ecmeq[,12], ecmeq[,13], ecmeq[,14]))
## diff(I)t-1 + diff(I)t-2
## diff(capu)t + diff(profit)t + diff(fininv)t
# diff(capu)t-1 + diff(fininv)t-1
# min(SC=7.434984 ) : 2 lags for  diff(inv)


VARselect((ecmeq[,6]),lag.max = 8, type = "both",
          exogen = cbind(ecmeq[,7],ecmeq[,8],ecmeq[,9],
                         ecmeq[,18],ecmeq[,19],
                         ecmeq[,11], ecmeq[,12], ecmeq[,13], ecmeq[,14]))
## diff(I)t-1 + diff(I)t-2
## diff(capu)t + diff(profit)t + diff(fininv)t
# diff(profit)t-1 + diff(fininv)t-1
# min(SC=7.409239 ) : 2 lags for  diff(inv)

VARselect((ecmeq[,6]),lag.max = 8, type = "both",
          exogen = cbind(ecmeq[,8], ecmeq[,9],
                         ecmeq[,19],
                         ecmeq[,11], ecmeq[,12], ecmeq[,13], ecmeq[,14]))
## diff(I)t-1 + diff(I)t-2
# diff(fininv)t
# diff(fininv)t-1
# min(SC=7.645901 ) : 2 lags for  diff(inv)

VARselect((ecmeq[,6]),lag.max = 8, type = "both",
          exogen = cbind(ecmeq[,17],ecmeq[,18],ecmeq[,19],
                         ecmeq[,11], ecmeq[,12], ecmeq[,13], ecmeq[,14]))
## diff(I)t-1 + diff(I)t-2
# diff(capu)t-1 + diff(profit)t-1 + diff(fininv)t-1
# min(SC=7.720005 ) : 2 lags for  diff(inv)

VARselect((ecmeq[,6]),lag.max = 8, type = "both",
          exogen = cbind(ecmeq[,17],ecmeq[,18],
                         ecmeq[,11], ecmeq[,12], ecmeq[,13], ecmeq[,14]))
## diff(I)t-1 + diff(I)t-2
# diff(capu)t-1 + diff(profit)t-1
# min(SC= ) : 2 lags for  diff(inv)

VARselect((ecmeq[,6]),lag.max = 8, type = "both",
          exogen = cbind(ecmeq[,17],ecmeq[,19],
                         ecmeq[,11], ecmeq[,12], ecmeq[,13], ecmeq[,14]))
## diff(I)t-1 + diff(I)t-2
# diff(capu)t-1 + diff(fininv)t-1
# min(SC=7.741029 ) : 2 lags for  diff(inv)

VARselect((ecmeq[,6]),lag.max = 8, type = "both",
          exogen = cbind(ecmeq[,18],ecmeq[,19],
                         ecmeq[,11], ecmeq[,12], ecmeq[,13], ecmeq[,14]))
## diff(I)t-1 + diff(I)t-2
# diff(profit)t-1 + diff(fininv)t-1
# min(SC= ) : 2 lags for  diff(inv)


# -- round 3 --------------------------------------------------------------

VARselect((ecmeq[,6]),lag.max = 8, type = "both",
          exogen = cbind(ecmeq[,7],ecmeq[,8],
                         ecmeq[,17],ecmeq[,18],
                         ecmeq[,22],ecmeq[,23],
                         ecmeq[,11],
                         ecmeq[,13], ecmeq[,14]))

VARselect((ecmeq[,6]),lag.max = 8, type = "both",
          exogen = cbind(ecmeq[,7],ecmeq[,8],
                         ecmeq[,18],
                         ecmeq[,23],
                         ecmeq[,11], ecmeq[,13], ecmeq[,14]))
## diff(I)t-1
## diff(capu)t + diff(profit)t
# diff(profit)t-1  +  diff(profit)t-2
# min(SC=7.361737 ) : 1 lags for  diff(inv)


# i.i.d residuals ---------------------------------------------------------

## 1st. ALTERNATIVE : FinInvRATIO ###

# diff(capu)t + diff(profit)t + diff(fininv)t
# min(SC=7.398314) : 2 lags for  diff(inv)

### ARDL(1,1,0,1) in unrestricted form (equation #4 from Dave Gile's Blog)
      ## with lag(0) for exogen variables
            model4.1 <- lm((ecmeqbtwn[,6]) ~ ecmeqbtwn[,11] + ecmeqbtwn[,12]+ ecmeqbtwn[,13] +ecmeqbtwn[,14] +
                                                    ecmeqbtwn[,7] + ecmeqbtwn[,8] + ecmeqbtwn[,9] +
                                       ecmeqbtwn[,16] + ecmeqbtwn[,17]+ ecmeqbtwn[,19])
      ## withOUT lag(0) for exogen variables
            model4.2 <- lm((ecmeqbtwn[,6]) ~ ecmeqbtwn[,11] + ecmeqbtwn[,12]+ ecmeqbtwn[,13] +ecmeqbtwn[,14] +
                                       ecmeqbtwn[,16] + ecmeqbtwn[,17]+ ecmeqbtwn[,19])

      ## test both models' residual autocorr :
            bgtest(model4.1)
            bgtest(model4.2)
            
            
            
# diff(capu)t + diff(profit)t
# min(SC=7.380591) : 2 lags for  diff(inv)

model3 <- lm((ecmeq[,6]) ~ ecmeq[,16] + ecmeq[,21]+
               ecmeq[,7] + ecmeq[,8]+
               ecmeq[,17] + ecmeq[,18]+
               ecmeq[,11] + ecmeq[,12] + ecmeq[,13] + ecmeq[,14])
## diff(I)t-1 + diff(I)t-2
# diff(capu)t + diff(profit)t
# diff(capu)t-1 + diff(profit)t-1
# min(SC=7.396523 ) : 2 lags for  diff(inv)


model4 <- lm((ecmeq[,6]) ~ ecmeq[,16] + ecmeq[,21]+
               ecmeq[,7] + ecmeq[,8]+
               ecmeq[,17]+
               ecmeq[,11] + ecmeq[,12] + ecmeq[,13] + ecmeq[,14])
## diff(I)t-1 + diff(I)t-2
## diff(capu)t + diff(profit)t
# diff(capu)t-1
# min(SC=7.399642 ) : 2 lags for  diff(inv)

model5 <- lm((ecmeq[,6]) ~ ecmeq[,16] + ecmeq[,21]+
               ecmeq[,7] + ecmeq[,8]+
               ecmeq[,18]+
               ecmeq[,11]+ ecmeq[,12] + ecmeq[,13] + ecmeq[,14])
## diff(I)t-1 + diff(I)t-2
## diff(capu)t + diff(profit)t
# diff(profit)t-1
# min(SC=7.374593 ) : 2 lags for  diff(inv)


model6 <- lm((ecmeq[,6]) ~ ecmeq[,16] + ecmeq[,21]+
               ecmeq[,7] + ecmeq[,8]+
               ecmeq[,18]+
               ecmeq[,23]+
               ecmeq[,11] + ecmeq[,12] + ecmeq[,13] + ecmeq[,14])
## diff(I)t-1
## diff(capu)t + diff(profit)t
# diff(profit)t-1  +  diff(profit)t-2
# min(SC=7.361737 ) : 1 lags for  diff(inv)


## 2nd. Alternative  FININV  ##

#SC:7.3091
model1 <- lm((ecmeq[,6]) ~ ecmeq[,16] + ecmeq[,21]+
               ecmeq[,7] + ecmeq[,8] + ecmeq[,9]+
               ecmeq[,18]+
               ecmeq[,11] + ecmeq[,12]+ ecmeq[,13] + ecmeq[,14])

#SC:7.3092
model2 <- lm((ecmeq[,6]) ~ ecmeq[,16]+
               ecmeq[,7] + ecmeq[,8] + ecmeq[,9]+
               ecmeq[,18]+
               ecmeq[,11] + ecmeq[,12]+ ecmeq[,13] + ecmeq[,14])

#SC:7.3165
model3 <- lm((ecmeq[,6]) ~ ecmeq[,16]+
               ecmeq[,7] + ecmeq[,8] + ecmeq[,9]+
               ecmeq[,11] + ecmeq[,12]+ ecmeq[,13] + ecmeq[,14])

#SC:7.3271
model4 <- lm((ecmeq[,6]) ~ ecmeq[,16]+
               ecmeq[,7] + ecmeq[,8] + ecmeq[,9]+
               ecmeq[,18] + ecmeq[,19]+
               ecmeq[,11] + ecmeq[,12]+ ecmeq[,13] + ecmeq[,14])

#SC:7.3275
model5 <- lm((ecmeq[,6]) ~ ecmeq[,16] + ecmeq[,21]+
               ecmeq[,7] + ecmeq[,8] + ecmeq[,9]+
               ecmeq[,18] + ecmeq[,19]+
               ecmeq[,11] + ecmeq[,12]+ ecmeq[,13] + ecmeq[,14])


# p-value of Lung-Box t-stat Ho: independance of residuals
Box.test(model1$residuals, type="Ljung-Box",lag = 7)
Box.test(model2$residuals,lag = 8, type="Ljung-Box",fitdf=1+1)
Box.test(model3$residuals,lag = 8, type="Ljung-Box",fitdf=1+1)
Box.test(model4$residuals,lag = 8, type="Ljung-Box",fitdf=1+1)
Box.test(model5$residuals,lag = 8, type="Ljung-Box",fitdf=1+1)
Box.test(model6$residuals,lag = 8, type="Ljung-Box",fitdf=1+1)

bgtest(model1)
bgtest(model2)
bgtest(model3)
bgtest(model4)
bgtest(model5)
bgtest(model6)

# For FinInvRATIO --> MODEL 2,5,6 HAVE WELL BAHAVED RESIDUALS
# For   FinInv  --> MODELS N° 1,5 HAVE WELL BAHAVED RESIDUALS

# Stability: see the roots
#roots(model3)


# Bound tests -------------------------------------------------------------

# Bound tests (all theta = 0 , coef of non-constrained coint regression)

# 1st. Alternative FinInvRatio
waldtest(model2,"ecmeq[, 11]", "ecmeq[, 12]",
         "ecmeq[, 13]", "ecmeq[, 14]")
waldtest(model5,"ecmeq[, 11]", "ecmeq[, 12]",
         "ecmeq[, 13]", "ecmeq[, 14]")
waldtest(model6,"ecmeq[, 11]", "ecmeq[, 12]",
         "ecmeq[, 13]", "ecmeq[, 14]")
## All t-stat fall beneath LOWER BOUND of NOCOINT ####

# 2nd. Alternative FinInv
waldtest(model1,"ecmeq[, 11]", "ecmeq[, 12]",
         "ecmeq[, 13]", "ecmeq[, 14]")
waldtest(model5,"ecmeq[, 11]", "ecmeq[, 12]",
         "ecmeq[, 13]", "ecmeq[, 14]")
## All F-stat are above UPPER BOUND of COINT (1% & 2.5%) ####
Summary(model1)


# Ardl Package ------------------------------------------------------------


# Data sets arangement

#### 1952 - 2016

#Create dataframe for compatibility with ARDL PACKAGE Funct?
  c5dat <- data.frame(inv = (ecmeq[,1]), u = ecmeq[,2],
                      r=(ecmeq[,3]), fi = (ecmeq[,4]), d = ecmeq[,5])
  ardl_data <- data.frame(inv = (data_list[,1]), u = data_list[,2],
                      r=(data_list[,3]), fi = (data_list[,4]),
                      ii = data_list[,5], d = data_list[,6])

#1/2 ALTERN.
  #cdat<-ts(c5dat,start=c(1952,3), end=c(2016,4), frequency=4)
  cdat<-ts(ardl_data,start=c(1952,3), end=c(2016,4), frequency=4)

# WATCHOUT : use alternatively lvldate according to sample PERIODs
  #52-2016
    lvldata <- cdat
  #52 - 2007
    lvldata <- window(cdat, start = c(1952,3), end=c(2008,1), frequency=4)
  #52 - 1987
    lvldata <- window(cdat, start = c(1952,3), end=c(1987,1), frequency=4)
  #1985 - 2016
    lvldata <- window(cdat, start = c(1987,1), end=c(2016,4), frequency=4)
  #1984 - 2008
    lvldata <- window(cdat, start = c(1984,1), end=c(2008,1), frequency=4)

# Write lvldata in CSV 
write.csv(lvldata, file = "MyData.csv")

# LagSel 1 ----------------------------------------------------------------

#1st. Alternative : i~u.r.fi
  Alt1select1 <- ardl::auto.ardl(inv~u+r+fi, data=lvldata, ymax=2,
                                 xmax=c(4,4,4),case=1,verbose = T,ic = "aic")
  Alt1select1 <- ardl::auto.ardl(inv~u+r+fi, data=lvldata, ymax=4,
                                 xmax=c(4,4,4),case=3,verbose = T,ic = "aic")
  Alt1select1 <- ardl::auto.ardl(inv~u+r+fi, data=lvldata, ymax=4,
                                 xmax=c(4,4,4),case=5,verbose = T,ic = "aic")
#2nd. ALTERN. : + intangibles
  Alt1select1 <- ardl::auto.ardl(inv~u+r+fi+ii, data=lvldata, ymax=2,
                                 xmax=c(4,4,4,4),case=1,verbose = T,ic = "aic")
  Alt1select1 <- ardl::auto.ardl(inv~u+r+fi+ii, data=lvldata, ymax=4,
                                 xmax=c(4,4,4,4),case=3,verbose = T,ic = "aic")
  Alt1select1 <- ardl::auto.ardl(inv~u+r+fi+ii, data=lvldata, ymax=4,
                                 xmax=c(4,4,4,4),case=5,verbose = T,ic = "aic")
  #BEST for INTANGIBLES is ARDL(1,1,0,1,1)
  
  
#1952-2016#Best model is inv ~ +1 + L(inv, 1) + L(inv, 2) + L(inv, 3) +
# L(inv, 4) + u + L(u, 1) + r + L(r, 1) +
# L(r, 2) + fi + L(fi, 1)
# chosen by BIC = 2588.131
#1952-2007#Best model is inv ~ +1 + L(inv, 1) + L(inv, 2) + L(inv, 3) +
# L(inv, 4) + u + L(u, 1) + r + L(r, 1) +
# L(r, 2) + L(r, 3) + fi
# chosen by bic = 1256.457
#1952-1985#Best model is inv ~ +1 + L(inv, 1) + L(inv, 2) + L(inv, 3) + L(inv, 4) + L(inv,      5) + L(inv, 6) + L(inv, 7) + L(inv, 8) + L(inv, 9) + L(inv,      10) + L(inv, 11) + L(inv, 12) + L(inv, 13) + L(inv, 14)
# +u + L(u, 1) + r + L(r, 1) + L(r, 2) + L(r, 3) + fi
# chosen by bic = 1200.469
#1985-2016#Best model is inv ~ +1 + L(inv, 1) + L(inv, 2) + L(inv, 3) + L(inv, 4)
# + u + L(u, 1) + r + L(r, 1) + fi + L(fi, 1)
# chosen by bic = 1241.917
#1984-2007#Best model with TotalAsset RATIOs is case 1:
# inv ~ -1 + L(inv, 1) + u + L(u, 1) + r + fi
# chosen by bic = -806.7769
#Best model with LOG & PURE is case 5 inv ~ -1 + L(inv, 1)
#+ u + L(u, 1) + r + fi chosen by bic = -557.8029


# LagSel 2 ----------------------------------------------------------------

# 2nd. Alternative : i~u.r.fi+DEBT

c5select1 <- ardl::auto.ardl(inv~u+r+fi+d, data=lvldata, ymax=4,
                             xmax=c(4,4,4,4),case=1,verbose = T)
c5select3 <- ardl::auto.ardl(inv~u+r+fi+d, data=lvldata, ymax=4,
                             xmax=c(4,4,4,4),case=3,verbose = T)
c5select5 <- ardl::auto.ardl(inv~u+r+fi+d, data=lvldata, ymax=4,
                             xmax=c(4,4,4,4),case=5,verbose = T)

#1952-2016
# with FININV= Total Assets : case 1 Best1 model is inv ~ -1 + L(inv, 1) + L(inv, 2) + L(inv, 3) + L(inv, 4) + u +      L(u, 1) + r + L(r, 1) + L(r, 2) + fi + L(fi, 1) + d chosen by bic = 2582.938
# with FININV= PURE fin Assets : case 1 Best model is ,
# inv ~ -1 + L(inv, 1) + L(inv, 2) + L(inv, 3) + L(inv, 4)+
# + u + L(u, 1) + L(u, 2) + r + L(r, 1) + L(r, 2)+
# + L(r, 3)+ L(r, 4) + fi + L(fi, 1) + d chosen by bic = 2694.912
# with Log-variable: Best model is case 5
# inv ~ +1 + trend(inv) + L(inv, 1) + u + L(u, 1)
# + r + fi + d + L(d, 1) chosen by bic = -1105.235

#1952-2007# TODO
#1952-1985#
# case 5 Best model is inv ~ +1 + trend(inv) + L(inv, 1) + L(inv, 2) + L(inv, 3) + L(inv,      4) + u + L(u, 1) + r + fi + d + L(d, 1) chosen by bic = 1232.507
#1987-2016##
# with log & PURE FinInv: Best model is case 1
# inv ~ -1 + L(inv, 1) + u + L(u, 1) + r + fi + d + L(d, 1) chosen by bic = -558.1435
#1985-2007#
# case 1 Best model is inv ~ -1 + L(inv, 1) + L(inv, 2) + L(inv, 3) + L(inv, 4) + u +      L(u, 1) + r + fi + d chosen by bic = 813.4317


# i.i.d Ardl --------------------------------------------------------------

retards<-c(1,0,0,0)
retards<-c(1,1,0,0) #
retards<-c(1,1,1,0)

retards<-c(1,1,0,1) ###

retards<-c(1,1,0,2)
retards<-c(1,1,2,0)
retards<-c(2,0,0,0)
retards<-c(2,1,0,0)
retards<-c(2,1,1,0)
retards<-c(2,1,0,1)
retards<-c(1,1,1,1)
retards<-c(1,1,1,2)
retards<-c(2,1,1,1)


cas<- 1

mod <- ardl( inv~u+r+fi, data=lvldata, ylag=retards[1], xlag=retards[2:4], case=cas, quiet=FALSE )
summary(mod)

Box.test(mod$residuals,lag = 8, type="Ljung-Box",fitdf=sum(retards))

qqnorm(mod$residuals)
qqline(mod$residuals)  
bgtest(mod$residuals)

boxplot(mod$residuals)
hist(mod$residuals)
shapiro.test(ArdlRatioAlt1.0$residuals) #Royston (1995) to be adequate for p.value < 0.1.

## Estimation ##

# Est° 1 ------------------------------------------------------------------

# 1st. Alternative : i~u.r.fi

#1952-2016#
cmacroa<-ardl::ardl(inv~u+r+fi, data=lvldata, ylag=2,
                    xlag=c(8,3,1), case=5)
#1952-2007#
cmacrob<-ardl::ardl(inv~u+r+fi, data=lvldata, ylag=4,
                    xlag=c(1,3,0))
#1952-1985#
cmacroc<-ardl::ardl(inv~u+r+fi, data=lvldata, ylag=14,
                    xlag=c(1,3,0))
#1987-2016#
cmacrod<-ardl::ardl(inv~u+r+fi, data=lvldata, ylag=4,
                    xlag=c(1,1,1))
#1984-2008#
ArdlRatioAlt1<-ardl::ardl(inv ~ -1+u+r+fi+ii, data=lvldata, ylag=1,
                          xlag=c(1,0,1,1), case = 1)
### test for alternative specification ARDL(1,1,0,1) :
ArdlRatioAlt1.0<-ardl::ardl(inv ~ -1+u+r+fi, data=lvldata, ylag=1,
                            xlag=c(1,0,1), case = 3)
#IN FINE: when restricting the period for 1984-2008
# (to account for identified structural breaks)
# ARDL (Inv,u,r,FinIn) prooves LRR at 5%
# FinInv has SIGNIFICANT negative LR impact on Accum?

# Est° 2 ------------------------------------------------------------------
# 2nd. Alternative : i~u.r.fi+DEBT

#1952-2016 - TOTAL FIN ASSETS --> COINT 5%
#Case 1
cmacroa1<-ardl::ardl(inv~u+r+fi+d, data=lvldata, ylag=4,
                     xlag=c(1,2,1,0), case=1)
#1952-2016 - PURE FIN ASSTES --> existence of LRR at 5%
#Case 1
cmacroa1pur<-ardl::ardl(inv~u+r+fi+d, data=lvldata, ylag=4,
                        xlag=c(2,4,1,0), case=1)
#1952-2016 - LOG and PURE FIN ASSTES --> NO LRR
#Case 5
cmacroa1purlog<-ardl::ardl(inv~u+r+fi+d, data=lvldata, ylag=1,
                           xlag=c(1,0,0,1), case=5)
#1952-2007 - TODO
cmacrobb<-ardl::ardl(inv~u+r+fi+d, data=lvldata, ylag=4,
                     xlag=c(1,1,1,0))
#1952-1985 - COINT 5%
cmacroc5<-ardl::ardl(inv~u+r+fi+d, data=lvldata, ylag=4,
                     xlag=c(1,0,0,1), case=5)
#1987-2016 - LOG & PURE FinInv
cmacrodlog<-ardl::ardl(inv~u+r+fi+d, data=lvldata, ylag=1,
                       xlag=c(1,0,0,1), case=1)
#1984-2007 - COINT 10%
cmacroelog<-ardl::ardl(inv~u+r+fi+d, data=lvldata, ylag=1,
                       xlag=c(1,0,0,0), case=3)

# WALD Test ---------------------------------------------------------------

######## WALD TEST OK -->  long run relationship btw i~u.r.fi+DEBT ###
ardl::bounds.test(ArdlRatioAlt1)
ardl::coint(ArdlRatioAlt1)
