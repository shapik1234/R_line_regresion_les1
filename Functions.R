#
require("corrplot")
require("doBy")
require("car")
require("gvlma")
require("nortest")
require("MASS")
require("leaps")
require("bootstrap")
require("ggplot2")
require("ISwR")
require("ggplot2")
require("outliers")
require("stats")
require("lmtest")
require("forecast")
require("stats")

# Opening dates
#source("Functions.r")

options(digits = 3)
o.diam <- read.delim(file.choose(),
                     header = TRUE)
lapply(o.diam[,2:4],FUN = factor)
#
 ggplot(data=o.diam,aes(x=carat,y=price))+
  geom_point(col="darkgrey")+
  geom_smooth(col="blue")
#
ggplot(o.diam,aes(x=carat,y=price,color=certification))+
  geom_point()+geom_smooth()
ggplot(o.diam,aes(x=carat,y=price,color=colour))+
  geom_point()+geom_smooth()
ggplot(o.diam,aes(x=carat,y=price,color=clarity))+
  geom_point()+geom_smooth()
#
normality3.extend(o.diam$price)
normality3.extend(o.diam$carat)
envelopes(o.diam$price)
##############################   Log-congruence  ############

o.diam$logcar <- log(o.diam$carat)
o.diam$logpr <- log(o.diam$price)
#
ggplot(data=o.diam,aes(x=logcar,y=logpr))+
  geom_point(col="darkgrey")+
  geom_smooth(col="blue")
#########
normality3.extend(o.diam$logpr)
normality3.extend(o.diam$logcar)
#########
fit <- lm(logpr~certification+logcar,data=o.diam)
summary(fit)
#
display.resid(fit)
envelopes(fit$residuals)

normality3.extend(fit$residuals)
durbinWatsonTest(fit)
#
Box.test(fit$residuals,lag=10,type="Ljung-Box")
bgtest(logpr~certification+logcar,data=o.diam,order=1,type="Chisq")
acf(fit$residuals,type = "correlation")
#
crPlots(fit)
ncvTest(fit)
spreadLevelPlot(fit)
outlierTest(fit)
grubbs.test(o.diam$logpr,type=11)
o.diam <- o.diam[-c(130,220),]
#
#  fit.gvlma <- gvlma(logpr~certification+logcar,data=o.diam)
#  summary(fit.gvlma)
#
#stepAIC(lm(logpr~colour+clarity+certification+logcar-1,data=o.diam))
sqrt(vif(fit))>2
########################
ggplot(data=o.diam,aes(x=carat,y=price))+
  geom_point(col="darkgreen")+
  geom_smooth(col="blue")+
#  geom_line(aes(x=carat,y=exp(fit$fitted.values)),col="red")+
  geom_smooth(aes(x=carat,y=exp(fit$fitted.values)),col="red")
#
###############################################################
#
fit.nls <- nls(price~a*carat^k,data=o.diam,start=list(a=5,k=1),na.action = na.omit)
summary(fit.nls)
#
ggplot(data=o.diam,aes(x=carat,y=price))+
  geom_point(col="darkgreen")+
  geom_smooth(col="blue")+
  geom_line(aes(x=carat,y=predict(fit.nls)),col="red",lwd=1.25,linetype=2)
