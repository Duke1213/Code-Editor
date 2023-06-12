###############################################################################
##                          Final Exam Answer Sheet                          ##
###############################################################################

##############
# Question 1 # +10
##############

set.seed(1)
X = rnorm(100,10,1)
set.seed(2)
Y = rnorm(100,10,1)

result=NULL
for (i in 1:10000) {
  set.seed(i)
  index= sample(100,100,replace = T)
  x_bar = mean(X[index])
  y_bar = mean(Y[index])
  result=c(result,(x_bar^5)/(x_bar + y_bar)^2)
}
sd(result)^2

## Estimated variance of hat.z = _104.251__


##############
# Question 2 #
##############
library(glmnet)
#####
# a # +5
#####
ans = 0; 
for(rep in 1:100){
  set.seed(rep)
  X = cbind(rnorm(55), rnorm(55)); beta = rbind(1, 1)
  Y = X %*% beta + cbind(rnorm(55)) + 2
  Y = as.numeric(Y) # Just in case
  for(i in 3:25){
    # set.seed(i)
    X = cbind(X, rnorm(55)) # noise variables produced!
  }
  grid = seq(0,2,length = 100)
  lasso_fit = glmnet(X,Y,alpha = 1, lambda = grid) 
  lasso_kfold = cv.glmnet(X,Y,alpha=1, nfolds=10, lambda = grid)
  opt_lambda = lasso_kfold$lambda.min
  lasso_fit = glmnet(X,Y,alpha = 1, lambda = opt_lambda) 
  if(coef(lasso_fit)[2] > 0){ans = ans + 1}
}
ans/100


#LASSO selection rate for x1 = _100__%

#####
# b # +5
#####
library(leaps); 
ans = 0
for(rep in 1:100){
  set.seed(rep)
X = cbind(rnorm(55), rnorm(55)); beta = rbind(1, 1)
Y = X %*% beta + cbind(rnorm(55)) + 2
Y = as.numeric(Y) 
for(i in 3:25){
  # set.seed(i)
  X = cbind(X, rnorm(55)) 
}
dat = as.data.frame(cbind(Y, X))
backward.fit = regsubsets(Y~.,dat,nvmax=16,method = "backward")
Names = names(coef(backward.fit, which.min(summary(backward.fit)$bic)))
if('V2' %in% Names ){
  ans = ans + 1
}
}
ans = ans/100
#Backward selection rate for x1 = __100__%


#####
# c # +5
#####

## OLS
truth = 1
coef(lm(Y~., dat))['V2'] - 1 # bias
(summary(lm(Y~., dat))$coefficients[2,2])^2 # variance
0.03677035 + 0.2582137^2 # MSE

## Backward
backward.fit = regsubsets(Y~.,dat,nvmax=16,method = "backward")
summary(backward.fit)
abs(coef(backward.fit,2)['V2'] - truth) # bias
vcov(backward.fit,2) # variance
0.04483674^2 + 0.0121015561

## Lasso
lasso_fit = glmnet(X,Y,alpha = 1, lambda = opt_lambda)
coef(lasso_fit)[2] - truth #bias


#Bias for LASSO = __0.2380543__
#Bias for Backward = __0.04483674___
#Bias for OLS = __0.2582137__


#Variance for LASSO = ______ 
#Variance for Backward = __0.0121015561___ 
#Variance for OLS = __0.03677035__


#MSE for LASSO =   
#MSE for Backward = 0.01411189
#MSE for OLS = 0.1034447



##############
# Question 3 #
##############
library(boot)
library(ISLR)
attach(Wage)
library(splines)
#####
# a # +5
#####
ns.fit=glm(wage~ns(age,df=4,Boundary.knots = c(20, 60), knots = c(30, 40, 50) ),data=Wage)
sum(ns.fit$residuals^2)

## RSS for this model = _4776304__

#####
# b # +5
#####
local=loess(wage~age,span=.5,data=Wage)
sum(local$residuals^2)


## RSS for this model = ___4760377___

#####
# c # +5
#####
library(gam)
gam.3d=gam(wage~ns(year,2)+lo(age,span=0.2),data=Wage) 
sum(gam.3d$residuals^2)

## RSS for this model = __4731633____


##############
# Question 4 #
##############
library(kmed)
data(heart)

heart$class = as.factor(ifelse(heart$class==0,0,1))
heart$sex = as.factor(ifelse(heart$sex=='TRUE',1,0))
heart$fbs = as.factor(ifelse(heart$fbs=='TRUE',1,0))
heart$exang = as.factor(ifelse(heart$exang=='TRUE',1,0))


#####
# a # +5
#####
library(tree)
tree_heart=tree(class~.,heart, split = "gini", mincut = 5) 
set.seed(1)
cv_heart=cv.tree(tree_heart, FUN=prune.misclass ,K = 10) 
plot(cv_heart)
## The optimal number of nodes = __5___

#####
# b # +10
#####
library(randomForest)
library(dplyr); library(ggplot2)
set.seed(1)
rf_heart = randomForest(class~.,data=heart,mtry=2,importance=TRUE)
e1 = mean(rf_heart$err.rate[,'OOB'])
rf_heart = randomForest(class~.,data=heart,mtry=4,importance=TRUE)
e2 = mean(rf_heart$err.rate[,'OOB'])
rf_heart = randomForest(class~.,data=heart,mtry=8,importance=TRUE)
e3 = mean(rf_heart$err.rate[,'OOB'])
rf_heart = randomForest(class~.,data=heart,mtry=13,importance=TRUE)
e4 = mean(rf_heart$err.rate[,'OOB'])
tibble(m = c(2, 4, 8, 13), 
           OOB_err = c(e1, e2, e3, e4)) %>%
  ggplot(aes(m, OOB_err)) + geom_line() + geom_point() +
  theme_bw() +
  labs(title = 'Ramdon Forest OOB errors with different m')




##############
# Question 5 #
##############
library(MASS)
data(Boston)


#####
# a # +5
#####
grid = seq(0,1,length = 100)
X = as.matrix(Boston)[, 1:13]
Y = Boston$medv
lasso_fit = glmnet(X,Y,alpha = 1, lambda = grid) 
set.seed(1)
lasso.kfold = cv.glmnet(X,Y,alpha=1, nfolds=10, lambda = grid)
lasso.kfold$lambda.min 
## Optimal lambda = __0.03030303____

#####
# b # +5
#####
lasso_fit = glmnet(X,Y,alpha = 1, lambda = 0.02020202) 
coef(lasso_fit)
set.seed(1)
index = sample(nrow(Boston), 0.5*nrow(Boston))
train = Boston[index, ]; valid = Boston[-index, ]

ols = lm(medv ~ crim + zn + chas + nox + rm + dis + rad + tax + ptratio + black + lstat, 
         data = train)
mean((predict(ols, valid) - valid$medv)^2)

## test MSE = __26.85842____

#####
# c # +5
#####
maxs <- apply(Boston, 2, max)
mins <- apply(Boston, 2, min)
scaled <- as.data.frame(scale(Boston, center = mins, scale = maxs - mins)) 
set.seed(1)
train_ <- scaled[index,]
test_ <- scaled[-index,]

library(neuralnet)
n <- names(train_)
f <- as.formula(paste("medv ~", paste(n[!n %in% "medv"], collapse = " + "))) 
nn <- neuralnet(formula=f,data=train_,hidden=c(5,4,3,2),linear.output=T)

pr.nn <- compute(x = nn, covariate = test_[,1:13]) 
pr.nn_ <- pr.nn$net.result*( max(Boston$medv)-min(Boston$medv) ) + min(Boston$medv) 
MSE.nn <- sum((valid$medv - pr.nn_)^2)/nrow(valid)
MSE.nn

## test MSE = ___18.22477___