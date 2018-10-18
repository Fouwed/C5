# INIT ####
setwd("C:/Users/Ferdi/Documents/R/C5")
setwd("C:/Users/fouwe/Documents/R/C5")

library(tseries)
library(vars)
library(lmtest)
library(urca)
library(ardl)
library(outliers)

##### V.E.C.M    ####
    data_vecm1 <- ts.intersect(LogRgdp, log(IntInv+FinInv),
                           dbtot/(ProInv+IntInv+FinInv),log(ProInv),log(inv5))    # 2-Etha (REMEMBER AND CHANGE Fii to ETHA)    
    data_vecm1 <- ts.intersect(LogRgdp,log(IntInv+FinInv), 
                           log(dbtot),log(ProInv),log(inv5))  # 1-demand/supply seting relation (D=debt ; S=TotInv)
    #data_vecm1 <- ts.intersect(LogRgdp, ((FinInv+IntInv)/gdpts),
    #                       dbtnw,log(inv5))    # 2-Etha (REMEMBER AND CHANGE Fii to ETHA)
data_list_w <- window(data_vecm1,start=c(1981,1), end=c(2015,4), frequency=4)
data_list_w <- window(data_vecm1,start=c(1980,1), end=c(2008,1), frequency=4)
## OUTLIERS
data_list_w[76,2]<-mean(data_list_w[,2])
##
vecm_data <- data.frame(gdp = (data_list_w[,1]), fii = data_list_w[,2],
                        d=(data_list_w[,3]), inv = data_list_w[,4])
plot(data_list_w, nc=2)
## stationnary ####
adf.test(vecm_data[,"gdp"])
adf.test(vecm_data[,"fii"])
adf.test(vecm_data[,"d"])
adf.test(vecm_data[,"inv"])

adf.test(diff(vecm_data[,"gdp"]))
adf.test(diff(vecm_data[,"fii"]))
adf.test(diff(vecm_data[,"d"]))
adf.test(diff(vecm_data[,"inv"]))
# VAR estimat° (p=1, 2 & 7) ####
XOP=c("both","trend","const","none")
L<- 2
VARselect(vecm_data,lag.max = 8, type = XOP[L])
p1<-VAR(vecm_data, p=2, type = XOP[L])
p2<-VAR(vecm_data, p=3, type = XOP[L])
p7<-VAR(vecm_data, p=6, type = XOP[L])
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


#

##### A.R.D.L   ####
data_list<- ts.intersect(log(ProInv+IntInv+FinInv), (capu1),
                         ((profit1/(ProInv+IntInv+FinInv))),
                         (dbtnw),((FinInv+IntInv)/(ProInv+IntInv+FinInv)),
                         log(IntInv), log(FinInv), LogRgdp, log(inv5),log(dbtot),
                         dbtot/(ProInv+IntInv+FinInv))
data_list<- ts.intersect(log(ProInv+IntInv+FinInv), (capu1),
                         ((profit1/AssetTot)),
                         (dbtnw),((FinInv+IntInv)/AssetTot),
                         log(IntInv), log(FinInv), LogRgdp, log(inv5))
data_list_w <- window(data_list,start=c(1984,1), end=c(2015,1), frequency=4)

ardl_data <- data.frame(gtot = (data_list_w[,1]), u = data_list_w[,2],
                        r=(data_list_w[,3]), dtonw = data_list_w[,4],
                        etha = data_list_w[,5],ii = data_list_w[,6], 
                        fi = data_list_w[,7],gdp = data_list_w[,8],
                        inv = data_list_w[,9], lgd = data_list_w[,10],
                        d = data_list_w[,11])
#
# u-r ####
Alt1select1 <- ardl::auto.ardl(gtot~u+r, data=ardl_data, ymax=8,
                               xmax=c(5,8),case=3,verbose = T,ic = "aic")
Mod_sos<-ardl::ardl(gtot ~ u + r, data=ardl_data, ylag=2,
                    xlag=c(0,1), case = 3)
#
# u-r_d ####
Alt1select1 <- ardl::auto.ardl(gtot~u+r+d, data=ardl_data, ymax=18,
                               xmax=c(8,8,8),case=(1),verbose = T,ic = "aic")
Mod_sos<-ardl::ardl(gtot ~ u + r + d , data=ardl_data, ylag=8,
                    xlag=c(4,8,8), case = 3)
#
# r_d ####
Alt1select1 <- ardl::auto.ardl(gtot~r+d, data=ardl_data, ymax=8,
                               xmax=c(8,8),case=(1),verbose = T,ic = "aic")
Mod_sos<-ardl::ardl(gtot ~ r + d , data=ardl_data, ylag=5,
                    xlag=c(1,4), case = 3)
#
# u-r-etha ####
Alt1select1 <- ardl::auto.ardl(gtot~u+r+etha, data=ardl_data, ymax=8,
                               xmax=c(5,8,8),case=1,verbose = T,ic = "bic")
Mod_sos<-ardl::ardl(gtot ~ u + r + etha , data=ardl_data, ylag=2,
                    xlag=c(0,0,2), case = 1)
#
# r-d-etha ####
Alt1select1 <- ardl::auto.ardl(gtot~r+d+etha, data=ardl_data, ymax=8,
                               xmax=c(5,8,8),case=5,verbose = T,ic = "aic")
Mod_sos<-ardl::ardl(gtot ~ r + d + etha , data=ardl_data, ylag=16,
                    xlag=c(1,0,1), case = 5)
#
# u-r-d-etha ####
Alt1select1 <- ardl::auto.ardl(gtot~u+r+d+etha, data=ardl_data, ymax=8,
                               xmax=c(2,8,8,8),case=3,verbose = T,ic = "bic")
Mod_sos<-ardl::ardl(gtot ~ u + r + d + etha, data=ardl_data, ylag=4,
                    xlag=c(4,2,6,4), case = 3)
# Estim° ####
bounds.test(Mod_sos)
coint(Mod_sos)
Box.test(Mod_sos$residuals,lag = 9, type="Ljung-Box",fitdf=4) # I.I.D TESTS  #Ho:INDEPENDANT
shapiro.test(Mod_sos$residuals) #Ho:nORMALITY
car::ncvTest(Mod_sos) #Ho:constant error variance
#



# ++ ALTERNATIVE --->  cROISSANCE = Fii = f(u,r,etha)####
data_list<- ts.intersect(log(ProInv+IntInv+FinInv),
                         (capu1),log((profit1/(ProInv+IntInv+FinInv))),log(dbtnw),
                         ((FinInv+IntInv)/(ProInv+IntInv+FinInv)),
                         log(IntInv), log(FinInv), LogRgdp, log(inv5))
                         
data_list_w <- window(data_list,start=c(1984,1), end=c(2008,1), frequency=4)
data_list_w <- window(data_list,start=c(1958,1), end=c(2015,4), frequency=4)
ardl_data <- data.frame(gtot = (data_list_w[,1]), u = data_list_w[,2], 
                        r=(data_list_w[,3]), d = data_list_w[,4],
                        etha = data_list_w[,5],
                        ii = data_list_w[,6], fi = data_list_w[,7], 
                        gdp = data_list_w[,8],
                        inv = data_list_w[,9])
Alt1select1 <- ardl::auto.ardl(gtot~u+r, data=ardl_data, ymax=8,
                               xmax=c(5,8),case=1,verbose = T,ic = "aic")
Mod_sos<-ardl::ardl(gtot ~ u + r, data=ardl_data, ylag=2,
                    xlag=c(2,1), case = 1)

Alt1select1 <- ardl::auto.ardl(gtot~u+r+d, data=ardl_data, ymax=8,
                               xmax=c(5,8,8),case=1,verbose = T,ic = "aic")
Mod_sos<-ardl::ardl(gtot ~ u + r + d, data=ardl_data, ylag=3,
                    xlag=c(2,1,2), case = 1)
Alt1select1 <- ardl::auto.ardl(gtot~u+r+d+etha, data=ardl_data, ymax=8,
                               xmax=c(5,8,8,8),case=1,verbose = T,ic = "aic")
Mod_sos<-ardl::ardl(gtot ~ u + r + d + etha, data=ardl_data, ylag=2,
                    xlag=c(2,1,2,2), case = 1)
bounds.test(Mod_sos)
coint(Mod_sos)

# I.I.D TESTS
Box.test(Mod_sos$residuals,lag = 9, type="Ljung-Box",fitdf=4)
#Ho:INDEPENDANT 

shapiro.test(Mod_sos$residuals) #Royston (1995) to be adequate for p.value < 0.1.
#Ho:nORMALITY

car::ncvTest(Mod_sos)
#Ho:constant error variance




