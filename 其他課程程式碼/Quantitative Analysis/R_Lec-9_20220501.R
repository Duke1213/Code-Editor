###############################################################################
##                  R LECTURE 9: Moving Beyond Linearity                     ##
##            	 Quantitative Analysis - 2019 Spring  5/13   		     	    	 ##
###############################################################################
#
#
###############################################################################
##                                Author		                                 ##
##            	            ??‹å?ƒç¿° Spencer Wang   		     	                 ##
###############################################################################

rm(list=ls(all=T))

library(ISLR)
attach(Wage)

####################################################################
# Section 1:  Polynomial Regression and Step Function              #
####################################################################

#####################
# polynomial function

fit = lm(wage~poly(age,4), data =  Wage) #This function would return a matrix whose coliumns are a basis of orthogonal polynomials.
coef(summary(fit))

fit2 = lm(wage~poly(age,4,raw=T), data=Wage) #This is regressing wage on age, age^2, age^3, age^4
coef(summary(fit2))

fit2a = lm(wage~cbind(age, age^2, age^3, age^4), data=Wage) #This is identical to fit2.
coef(summary(fit2a))
# Note that fit and fit2 do not differ in a meaningful way - the fitted values obtain for both model are the same.

##plot
agelims = range(age)
age.grid = seq(from = agelims[1], to = agelims[2])
preds = predict(fit, newdata = list(age=age.grid),se=T) # Here we obtain the predictions form age = 18 to 80.
se.bands = cbind(preds$fit + 2*preds$se.fit,  preds$fit - 2*preds$se.fit ) # Here are the estimated 95% confidence interval.

plot(age,wage,xlim=agelims,cex=0.8,col="darkgrey", main="Degree-4 Polynomial", xlab="Age", ylab="Wage")
lines(age.grid,preds$fit,lwd=3,col="blue")
matlines(age.grid,se.bands,lwd=1.5,col="blue",lty=3)



###############
# Step function

fit=lm(wage~cut(age,breaks = 5),data=Wage)  # "breaks" represents the number of intervals into which x is to be cut.
                                            # The cutpoint is simply = "breaks - 1", and in this case 4. The range of the data is divided into 5 pieces of equal length.
table(cut(age,5))  # Here are the intervals and the number of observations that falls into them.
coef(summary(fit)) # The coefficients

##plot
agelims = range(age)
age.grid = seq(from = agelims[1], to = agelims[2])
preds = predict(fit, newdata = list(age=age.grid),se=T)
se.bands = cbind(preds$fit + 2*preds$se.fit,preds$fit - 2*preds$se.fit )

plot(age,wage,xlim=agelims,cex=0.8,col="darkgrey", main="Step Function with 4 Cutpoints", xlab="Age", ylab="Wage")
lines(age.grid,preds$fit,lwd=3,col="blue")
matlines(age.grid,se.bands,lwd=1.5,col="blue",lty=3)

## Cross Validation
library(boot)
cv.error=rep(0,10)
for(i in 1:10){
  set.seed(i)
  ageCut <- cut(Wage$age,i+1)
  data = data.frame(ageCut,Wage$wage)
  step.fit=glm(Wage.wage~ageCut, data=data)
  cv.error[i] = cv.glm(data, step.fit, K=10)$delta[2]
}
plot(seq(1:10),cv.error[1:10])
min(cv.error)
which.min(cv.error)


####################################################################
# Section 2:  Splines                                              #
####################################################################
library(splines)
library(ISLR)


###############
# cubic splines

agelims = range(age)
age.grid = seq(from = agelims[1], to = agelims[2]) # this is a grid from 18 to 80.

fit=lm(wage~bs(age,knots=c(25,40,60),degree = 3), data=Wage) # Here we prespecified knots at age 25, 40, and 60. This produces a spline with 6 basis function. 
                                                             # Note that "degree" represent the degree of the piecewise polynomial and the default is 3 for cubic splines.
pred=predict(fit,newdata=list(age=age.grid),se=T) # These are the fitted values for each age (from 18 to 80).
plot(age,wage,col="gray")
lines(age.grid,pred$fit,lwd=2) # The fitted line
lines(age.grid,pred$fit+2*pred$se,lty="dashed")
lines(age.grid,pred$fit-2*pred$se,lty="dashed")

# We can also use the "df" option to produce a spline with knots at uniform quantiles of the data.
dim(bs(age,knots=c(25,40,60))) # Here are the pre-specified knots
dim(bs(age,df=6)) # Note that this is equal to "dim(bs(age,df=6,degree = 3))" 
                  # and the code will choose "df - degree" knots, in our case here, knots = 6-3 = 3, and the location of the knots are chosen at uniform quantiles of the data.
attr(bs(age,df=6),"knots") # here we choose age = 33.75, 42, 51 as our three knots, which correspond to the 25%, 50%, and 75% quantile of age.
attr(bs(age,knots=c(25,40,60)),"knots")

#################
# natural splines

# To fit a natural spline, we use the "ns()" function.
fit2=lm(wage~ns(age,df=4),data=Wage) # Here we fit a natural spline with 4 degrees of freedom (which is a natural cubic spline with three knot)
                                     # The code will choose "df - 1" knots, in our case, knots = 4-1 = 3
attr(ns(age,df=4),"knots") # The location for each knot.

pred2=predict(fit2,newdata=list(age=age.grid),se=T)
lines(age.grid, pred2$fit,col="red",lwd=2)
lines(age.grid,pred2$fit+2*pred2$se,lty="dashed",col="red")
lines(age.grid,pred2$fit-2*pred2$se,lty="dashed",col="red")
# Here we can see that, compare to ordinary spline, natural spline has smaller variance near the boundaries.


####################################################################
# Section 3:  Local regression                                     #
####################################################################


# To fit a local regression, we use the "loess()" function.
fit=loess(wage~age,span=.2,data=Wage, degree =2)   # "span=.2" means that each nighborhood consists of 20% of the observation
                                                   # "degree = 2" is the defualt, meaning that a quadratic local regression is fit.
fit2=loess(wage~age,span=.5,data=Wage)  # "span=.5" means that each nighborhood consists of 50% of the observation

# plot
plot(age,wage,xlim=agelims,cex=.5,col="darkgrey")
title("Local Regression")
lines(age.grid,predict(fit,data.frame(age=age.grid)),col="red",lwd=2)
lines(age.grid,predict(fit2,data.frame(age=age.grid)),col="blue",lwd=2)
legend("topright",legend=c("Span=0.2","Span=0.5"),col=c("red","blue"),lty=1,lwd=2,cex=.8)
# Recall that the larger the span, the smoother (less flexible) of the fit.




####################################################################
# Section 4:  GAMs                                                 #
####################################################################
rm(list=ls(all=T))

library(ISLR)
library(gam)
data(Wage)
summary(Wage)

## rotatable 3D plots
library(ggplot2)
library(plotly)

# We now fit "wage" using natural spline functions of "year" with 3 knots, local regression with span=50% for "age".
gam.3d=gam(wage~ns(year,4)+lo(age,span=0.5),data=Wage) 

plot_ly(x=Wage$year, y=Wage$age, z=predict(gam.3d), type="scatter3d", mode="markers", color=predict(gam.3d))
plot_ly(x=Wage$year, y=Wage$age, z=predict(gam.3d), type="mesh3d")



################
# natural spline

# We now fit "wage" using natural spline functions of "year" and "age" with 3 knots respectively, while treating "education" as it is.
gam.natual=lm(wage~ns(year,4)+ns(age,4)+education,data=Wage)
par(mfrow=c(1,3))
plot.Gam(gam.natual,  se=TRUE, col="red", lwd=2 ,ylab = "f(x)") 
#Note that we have to use plot.Gam() instead of plot().

#Also note that the folloing codes will yield the same result
gam.natual=gam(wage~ns(year,4)+ns(age,4)+education,data=Wage)
par(mfrow=c(1,3))
plot(gam.natual,  se=TRUE, col="red")

#The summary function produces a summary of the gam fit
summary(gam.natual)
#Note that the tests in the "Anova for Nonparametric Effects" section can be interpreted as test of the null hypothesis of a linear relationship instead of a nonlinear relationship.

#We can make predictions (fitted values) using the "predict()" function
preds=predict(gam.natual,data=Wage)


######################################
# local regression and mixing strategy

# We now fit "wage" using natural spline functions of "year" with 3 knots, local regression with span=50% for "age", while treating "education" as it is.
gam.lo=gam(wage~ns(year,4)+lo(age,span=0.5)+education,data=Wage)
par(mfrow=c(1,3))
plot.Gam(gam.lo, se=TRUE, col="green", lwd=2 ,ylab = "f(x)")








