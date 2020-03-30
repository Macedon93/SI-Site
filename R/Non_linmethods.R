#Non Linear methods
library(ISLR)
attach(Wage)
#Polynomial Regression & Step functions 
fit=lm(wage~poly(age,4),data = Wage)
coef(summary(fit))
#use lm() function, in order to predit wage using 
# a fourth degree polynomial in age. 
#lets fit a poly regression with polynomials 1-4(raw). 
fit2=lm(wage~poly(age,4,raw = T), data = Wage)
coef(summary(fit2))
#another way of fitting this polynomial 
fit2a=lm(wage~age+I(age^2)+I(age^3)+I(age^4),data = Wage)
coef(fit2a)
#another more elegant way of fitting this polynomial
fit2b=lm(wage~cbind(age,age^2,age^3,age^4),data = Wage)
# we now create a grid of values for age at which we want predictions. 
agelims=range(age)
age.grid=seq(from=agelims[1],to=agelims[2])
preds=predict(fit,newdata = list(age=age.grid),se=TRUE)
se.bands=cbind(preds$fit+2*preds$se.fit-2*preds$se.fit)
#lets plot and add the 4th degree polynomial fit. 
par(mfrow =c(1,2) ,mar=c(4.5 ,4.5 ,1 ,1) ,oma=c(0,0,4,0))
plot(age ,wage ,xlim=agelims ,cex =.5, col =" darkgrey ")
title (" Degree -4 Polynomial ",outer =T)
lines(age.grid ,preds$fit ,lwd =2, col =" blue")
matlines (age.grid ,se.bands ,lwd =1, col =" blue",lty =3)
#We use the ANOVA function, to perform hypoth test that a simpler model is sufficient compared to a 
#more complex model. fit.1 must be nested for anova to work. 
fit.1= lm(wage~age ,data=Wage)
fit.2= lm(wage~poly(age ,2) ,data=Wage)
fit.3= lm(wage~poly(age ,3) ,data=Wage)
fit.4= lm(wage~poly(age ,4) ,data=Wage)
fit.5= lm(wage~poly(age ,5) ,data=Wage)
anova(fit.1, fit.2, fit.3, fit.4, fit.5)
#p values for linear fit is essentialy 0 indicating a bad fit. Degree 5 poly pvalue is 0.37. 
#this anova analysis shows us that wither a cubic or quartic polyfit provides a reasonable fit. 

# since poly() creates orthogonal plynomials we could have also done:
coef(summary (fit.5))
# these classical methods are best replaced by finding the polynomial degree 
# by using cross validation. 

#-----------------------------------#
#Splines
#-----------------------------------#
#lets fit wage to age using a regression spline. 
library(splines)
fit=lm(wage~bs(age,knots=c(25,40,60)),data = Wage)
pred=predict(fit,newdata = list(age=age.grid),se=T)
lines(age.grid ,pred$fit ,lwd =2)
lines(age.grid ,pred$fit+2* pred$se ,lty =" dashed ")
lines(age.grid ,pred$fit-2* pred$se ,lty =" dashed ")

# Instead of prespecified knots, lets fit a narutal spline. 
fit2=lm(wage~ns(age ,df =4) ,data=Wage)
pred2=predict (fit2 ,newdata =list(age=age.grid),se=T)
lines(age.grid , pred2$fit ,col ="red",lwd =2)
 plot(age ,wage ,xlim=agelims ,cex =.5, col =" darkgrey ")
 title (" Smoothing Spline ")
 fit=smooth.spline (age ,wage ,df =16)
 fit2=smooth.spline (age ,wage ,cv=TRUE)
 fit2$df
 lines(fit ,col ="red ",lwd =2)
 lines(fit2 ,col =" blue",lwd =2)
 legend (" topright ",legend =c("16 DF " ,"6.8 DF"),
         col=c("red "," blue "),lty =1, lwd =2, cex =.8)
 
 #heres a quick example of local regression using the same dataset. 
 plot(age ,wage ,xlim=agelims ,cex =.5, col =" darkgrey ")
 title (" Local Regression ")
 fit=loess (wage~age ,span =.2, data=Wage)
 fit2=loess(wage~age ,span =.5, data=Wage)
 lines(age.grid ,predict (fit ,data.frame(age=age.grid)),
         col ="red ",lwd =2)
 lines(age.grid ,predict (fit2 ,data.frame(age=age.grid)),
         col =" blue",lwd =2)
 legend (" topright ",legend =c("Span =0.2" ," Span =0.5") ,
           col=c("red "," blue "),lty =1, lwd =2, cex =.8)
 #GAM's
gam1=lm(wage~ns(year,4)+ns(age,5)+education,data=Wage)
library(gam)
install.packages("gam")
library(gam)
gam.m3=gam(wage~s(year,4)+s(age,5)+education, data = Wage)
par(mfrow=c(1,3))
plot(gam.m3, se=TRUE, col="blue")
summary(gam.m3)

