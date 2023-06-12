###############################################################################
##                           HW 7 Solutions           			    		         ##
###############################################################################

##############
# Question 1 #
##############

#####
#(a)# 
#####
rm(list=ls(all=T))
library(ISLR)
data(Auto)

set.seed(1)
train = sample(392,196)

lm.fit = lm(mpg~horsepower, data=Auto,subset=train)
lm.fit2 =lm(mpg~horsepower + weight, data=Auto,subset=train)
lm.fit3 =lm(mpg~horsepower + weight + acceleration, data=Auto,subset=train)

mean((predict(lm.fit,Auto)-Auto$mpg)[-train]^2)
mean((predict(lm.fit2,Auto)-Auto$mpg)[-train]^2)
mean((predict(lm.fit3,Auto)-Auto$mpg)[-train]^2)

#Best model = Model 2
#Test MSE = 16.50505

#####
#(b)# 
#####
library("lmtest")
library(boot)
glm.fit = glm(mpg~horsepower, data=Auto)
glm.fit2 = glm(mpg~horsepower + weight, data=Auto)
glm.fit3 = glm(mpg~horsepower + weight + acceleration, data=Auto)

LOOCV1 = cv.glm(Auto,glm.fit)
LOOCV2 = cv.glm(Auto,glm.fit2)
LOOCV3 = cv.glm(Auto,glm.fit3)

c(LOOCV1$delta[1],LOOCV2$delta[1],LOOCV3$delta[1]) 

#Best model = Model 2
#Test MSE = 18.11295

#####
#(c)# 
#####
set.seed(1) 
kFCV1 = cv.glm(Auto,glm.fit,K=10)
kFCV2 = cv.glm(Auto,glm.fit2,K=10)
kFCV3 = cv.glm(Auto,glm.fit3,K=10)

c(kFCV1$delta[1],kFCV2$delta[1],kFCV3$delta[1])

#Best model = Model 2
#Test MSE = 18.18335

##############
# Question 2 #
##############

#####
#(a)# 
#####
library("lmtest")

result=NULL
for (i in 1:1000) {
  set.seed(i)
  index= sample(392,392,replace = T)
  a=lm(mpg~horsepower,data = Auto,subset = index)
  result=c(result,coeftest(a)[2,1])
}
sd(result)

#Paired Bootstrap estimation = 0.007306607

#####
#(b)# 
#####
result=NULL
a=lm(mpg~horsepower,data = Auto)

for (i in 1:1000) {
  set.seed(i)
  index= sample(392,392,replace = T)
  Y=a$fitted.values + a$residuals[index]
  result=c(result,coeftest(lm(Y~horsepower,data = Auto))[2,1])
}
sd(result)

#Residual Bootstrap estimation = 0.006641035




