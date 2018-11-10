## DEBT ##
  # EFP process
    #type= MOSUM
      efpMo_d <- efp(diff(dnw) ~ 1, type = "Rec-MOSUM",h=0.145)#1980's break whithin 0.05-->0.07
      # 0.053 --> 3 years
      plot(efpMo_d)
      abline(v=1999.5, col="grey50")
      abline(v=1985.0, col="grey50")
      ###RESULTS: BREAK in 1985 then 1999 for dnw   #
    
    #type= ME
      efpCu_d <- efp(diff(dnw) ~ 1, type = "ME")
      # 0.053 --> 3 years
      plot(efpCu_d)
      abline(v=1999.5, col="grey50")
      abline(v=1985.0, col="grey50")
      ###RESULTS: BREAK in 1985 then 1999 for dnw   #

  #Fstat
    #p.vavule of F-stat
      fs.debt <- Fstats(diff(dnw) ~ 1)
      sctest(fs.debt, type = "supF",asymptotic = T)
      sctest(fs.debt, type = "aveF",asymptotic = T)
      sctest(fs.debt, type = "expF")
    
    #BIC of many breakpoints
      bp.debt1 <- breakpoints(diff(dnw) ~ 1)
      summary(bp.debt1)
      x <- c(570.4, 567.3, 564.1, 566.9, 569.6, 579.8)
      plot(c(0,1,2,3,4,5), x, xlab="Number of break points", ylab="BIC", type="l")
      points(c(0,1,2,3,4,5), x,type = "p")
    
    # Fitted models
      fs.debt2 <- Fstats(diff(dnw) ~ 1)
      breakpoints(fs.debt2)
      bp.debt2 <- breakpoints(diff(dnw) ~ 1,breaks = 2)
      summary(bp.debt2)
      fmdnw0 <- lm(diff(dnw) ~ 1)
      fmdnwf <- lm(diff(dnw) ~ breakfactor(bp.debt2))
      plot(diff(dnw))
      lines(ts(fitted(fmdnw0), start=c(1951)), col = 3)
      lines(ts(fitted(fmdnwf), start = c(1951,4), frequency = 4), col = 4)
      lines(bp.debt2)
      
    
#Stats
    sctest(efpMo_d)    
    sctest(efpCu_d)
    sctest(fs.debt2)
    
# ZIVOT & ANDREWS test
    #RGDP - Level
      za.dnw <- ur.za(gdpts, lag= 9, model = "intercept")
      summary(za.dnw)
      plot(za.dnw)
      
      za.dnw <- ur.za(gdpts, lag= 9, model = "trend")
      summary(za.dnw)
      plot(za.dnw)
      # result: non-signif BP in 1980:50
      
      za.dnw <- ur.za(gdpts, lag= 9, model = "both")
      summary(za.dnw)
      plot(za.dnw)
    
    
    #LOG-RGDP
      za.dnw <- ur.za(log(gdpts), lag= 9, model = "intercept")
      summary(za.dnw)
      plot(za.dnw)
      
      za.dnw <- ur.za(log(gdpts), lag= 11, model = "trend")
      summary(za.dnw)
      plot(za.dnw)
      # result: non-signif BP in 1998:50
      
      za.dnw <- ur.za(log(gdpts), lag= 9, model = "both")
      summary(za.dnw)
      plot(za.dnw)

#BREAK in the cointegration 
      library(ts)
data_coint <- ts.intersect(gdpts, dnw, diff(gdpts),diff(dnw))
ci_dat <- data.frame(g = (data_coint[,1]), d = data_coint[,2],
                     dg= data_coint[,3], dd= data_coint[,4])
#ci_dat <- window(ci_dat2, start= c(1952, 1)) #, end= c(2016,4),frequency = 4)

coint.res <- residuals(lm(g ~ d, data = ci_dat))
coint.res <- lag(ts(coint.res, start = c(1953, 1), freq = 4), k = -1)

data_coint <- ts.intersect(gdpts, dnw, diff(gdpts),diff(dnw),coint.res)
ci_dat <- data.frame(g = (data_coint[,1]), d = data_coint[,2],
                     dg= data_coint[,3], dd= data_coint[,4],
                     cir= data_coint[,5])

#ci_dat <- cbind(ci_dat, coint.res)
#ci_dat2 <- cbind(ci_dat2, diff(ci_dat2[,"g"]), coint.res)

ecm.model <- dg ~ cir + dd

#EFP
  ocus <- efp(ecm.model, type = "OLS-CUSUM", data = ci_dat)
  me <- efp(ecm.model, type = "ME", data = ci_dat, h = 0.2)
  
  bound.ocus <- boundary(ocus, alpha = 0.01)
  plot(ocus, boundary = FALSE)
  lines(bound.ocus, col = 4)
  lines(-bound.ocus, col = 4)
  
  plot(me, alpha = 0.05)
  plot(me, functional = NULL)
  
  plot(ocus, functional = "meanL2")
  
  sctest(ocus)

#F-stat tests
  fs <- Fstats(ecm.model, from = c(1955, 1),
               to = c(1990, 1), data = ci_dat)
  plot(fs, alpha = 0.01)
  plot(fs, aveF=T, alpha = 0.01)


  
# HISTORICAL COST  Debt to NetWorth
  
  
  dthnw <- read.csv("dnw_HIST.csv",head = TRUE, sep=",")
  dnwH <- ts(dthnw$NCBCMDPNWHC, start = c(1951,4),frequency = 4)
  ts.plot(dnwH)
  
  ## DEBT ##
  # EFP process
  #type= MOSUM
  efpMo_d <- efp(diff(dnwH) ~ 1, type = "Rec-MOSUM",h=0.145)#1980's break whithin 0.05-->0.07
  # 0.053 --> 3 years
  plot(efpMo_d)
  abline(v=1999.5, col="grey50")
  abline(v=1985.0, col="grey50")
  ###RESULTS: BREAK in 1985 then 1999 for dnwH   #
  
  #type= ME
  efpCu_d <- efp(diff(dnwH) ~ 1, type = "ME")
  # 0.053 --> 3 years
  plot(efpCu_d)
  abline(v=1999.5, col="grey50")
  abline(v=1985.0, col="grey50")
  ###RESULTS: BREAK in 1985 then 1999 for dnwH   #
  
  #Fstat
  #p.vavule of F-stat
  fs.debt <- Fstats(diff(dnwH) ~ 1)
  sctest(fs.debt, type = "supF",asymptotic = T)
  sctest(fs.debt, type = "aveF",asymptotic = T)
  sctest(fs.debt, type = "expF")
  
  #BIC of many breakpoints
  bp.debt1 <- breakpoints(diff(dnwH) ~ 1)
  summary(bp.debt1)
  x <- c(769.6, 763.0, 767.2, 773.2, 783.3, 794.2)
  plot(c(0,1,2,3,4,5), x, xlab="Number of break points", ylab="BIC", type="l")
  points(c(0,1,2,3,4,5), x,type = "p")
  
  # Fitted models
  fs.debt2 <- Fstats(diff(dnwH) ~ 1)
  breakpoints(fs.debt2)
  bp.debt2 <- breakpoints(diff(dnwH) ~ 1,breaks = 2)
  summary(bp.debt2)
  fmdnwH0 <- lm(diff(dnwH) ~ 1)
  fmdnwHf <- lm(diff(dnwH) ~ breakfactor(bp.debt2))
  plot(diff(dnwH))
  lines(ts(fitted(fmdnwH0), start=c(1951)), col = 3)
  lines(ts(fitted(fmdnwHf), start = c(1951,4), frequency = 4), col = 4)
  lines(bp.debt2)
  
  
  #Stats
  sctest(efpMo_d)    
  sctest(efpCu_d)
  sctest(fs.debt2)
  
  # ZIVOT & ANDREWS test
  #RGDP - Level
  za.dnwH <- ur.za(dnwH, lag= 9, model = "intercept")
  summary(za.dnwH)
  plot(za.dnwH)
  
  za.dnwH <- ur.za(dnwH, lag= 9, model = "trend")
  summary(za.dnwH)
  plot(za.dnwH)
  # result: non-signif BP in 1980:50
  
  za.dnwH <- ur.za(dnwH, lag= 9, model = "both")
  summary(za.dnwH)
  plot(za.dnwH)
  
  
  #LOG-RGDP
  za.dnwH <- ur.za((dnwH), lag= 9, model = "intercept")
  summary(za.dnwH)
  plot(za.dnwH)
  
  za.dnwH <- ur.za((dnwH), lag= 11, model = "trend")
  summary(za.dnwH)
  plot(za.dnwH)
  # result: non-signif BP in 1998:50
  
  za.dnwH <- ur.za((dnwH), lag= 9, model = "both")
  summary(za.dnwH)
  plot(za.dnwH)
  
  #BREAK in the cointegration 
  library(ts)
  data_coint <- ts.intersect(gdpts, dnwH, diff(gdpts),diff(dnwH))
  ci_dat <- data.frame(g = (data_coint[,1]), d = data_coint[,2],
                       dg= data_coint[,3], dd= data_coint[,4])
  #ci_dat <- window(ci_dat2, start= c(1952, 1)) #, end= c(2016,4),frequency = 4)
  
  coint.res <- residuals(lm(g ~ d, data = ci_dat))
  coint.res <- lag(ts(coint.res, start = c(1953, 1), freq = 4), k = -1)
  
  data_coint <- ts.intersect(gdpts, dnwH, diff(gdpts),diff(dnwH),coint.res)
  ci_dat <- data.frame(g = (data_coint[,1]), d = data_coint[,2],
                       dg= data_coint[,3], dd= data_coint[,4],
                       cir= data_coint[,5])
  
  #ci_dat <- cbind(ci_dat, coint.res)
  #ci_dat2 <- cbind(ci_dat2, diff(ci_dat2[,"g"]), coint.res)
  
  ecm.model <- dg ~ cir + dd
  
  #EFP
  ocus <- efp(ecm.model, type = "OLS-CUSUM", data = ci_dat)
  me <- efp(ecm.model, type = "ME", data = ci_dat, h = 0.2)
  
  bound.ocus <- boundary(ocus, alpha = 0.01)
  plot(ocus, boundary = FALSE)
  lines(bound.ocus, col = 4)
  lines(-bound.ocus, col = 4)
  
  plot(me, alpha = 0.05)
  plot(me, functional = NULL)
  
  plot(ocus, functional = "meanL2")
  
  sctest(ocus)
  
  #F-stat tests
  fs <- Fstats(ecm.model, from = c(1975, 1),
               to = c(2005, 1), data = ci_dat)
  plot(fs, alpha = 0.01)
  plot(fs, aveF=T, alpha = 0.01)
  
  
  

