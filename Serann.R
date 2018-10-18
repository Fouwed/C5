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




