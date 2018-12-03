require(stats)
require(datasets)
require(ggplot2)
require(nortest)
require(car)
require(corrplot)
require(readxl)
require(MASS)
require(lmtest)
#source 
source("Functions.R")
#####################
#pr1
#####################
set.seed(1)
n = 100
x <- runif(n)
#plotnost
y1 <- x + rnorm(n, sd=.1)
full.pack <- rbind(x,y1)
fit1 <- lm(y1 ~ x)
#display |_|_| two column for graphics
par(mfrow=c(1,2))
plot(x,y1,pch = 21, col = "black", bg = "lightblue", cex = .9)
abline(fit1)
plot(x,resid(fit1),pch = 20, col = "black", bg = "lightblue", cex = .9)
abline(h=0)
#остатки расспределены равномерно что говорит о правильной модели 
# тепрь пример не правильной модели
#pr2
y2 <- log(x) + rnorm(n, sd=.1)
full.pack2 <- rbind(x,y2)
fit2 <- lm(y2 ~ x)
plot(x,y2,pch = 21, col = "black", bg = "lightblue", cex = .9)
abline(fit2)
plot(x,resid(fit2),pch = 20, col = "black", bg = "lightblue", cex = .9)
abline(h=0)
# вывод по самой модели - можно сказать что она линейна 
#но смотря на остатки - вывод 1  - они имеют зависимость (паттерн) модель не верна
#pr3
y3 <- x + rnorm(n, sd=.001*x)
full.pack3 <- rbind(x,y3)
fit3 <- lm(y3 ~ x)
plot(x,y3,pch = 21, col = "black", bg = "lightblue", cex = .9)
abline(fit3)
plot(x,resid(fit3),pch = 20, col = "black", bg = "lightblue", cex = .9)
abline(h=0)
#линейная модель с такими раздутыми остатками не корректна 
#
#
#построим график квантилей остатков квантилей которые можно бы ожидать при условии 
#что остатки нормально расспределены
#нормальные остатки 
qqnorm(resid(fit1))
qqline(resid(fit1))
#не нормальные 
qqnorm(resid(fit2))
qqline(resid(fit2))
#
#
ggQQ = function(lm) {
  # extract standardized residuals from the fit
  d <- data.frame(std.resid = rstandard(lm))
  # calculate 1Q/4Q line
  y <- quantile(d$std.resid[!is.na(d$std.resid)], c(0.25, 0.75))
  x <- qnorm(c(0.25, 0.75))
  slope <- diff(y)/diff(x)
  int <- y[1L] - slope * x[1L]
  
  p <- ggplot(data=d, aes(sample=std.resid)) +
    stat_qq(shape=1, size=3) +           # open circles
    labs(title="Normal Q-Q",             # plot title
         x="Theoretical Quantiles",      # x-axis label
         y="Standardized Residuals") +   # y-axis label
    geom_abline(slope = slope, intercept = int, linetype="dashed")  # dashed reference line
  return(p)
}
# зависимость стандартных квантилей остатков и текущих двет нам функции
#qqnorm(resid(fit1))
#qqline(resid(fit1)) - они должны сходиться 
# ggQQ - своя функция отрисовки зависимости этих квантелей через пп==ггплот
#
ggQQ(fit1)
ggQQ(fit2)
#############
#############
#работа с выбросами 
#############
#############
x4 <- c(9, x) # добавили выброс
y4 <- c(3, x + rnorm(n, sd=.1))
full.pack4 <- rbind(x4,y4)
fit4 <- lm(y4 ~ x4)
par(mfrow = c(1,1))
plot(x4,y4,pch = 21, col = "black", bg = "lightblue", cex = .9)
abline(fit4)
# имеем выброс который может исказить результаты
round(dfbetas(fit4), 3)
round(hatvalues(fit4),3)
# как видим 1 член имеет гараздо большее влияние на нашу модель
