###############################################################################
##                   R LECTURE 6: Resampling Methods  		      		         ##
##            	 Quantitative Analysis - 2021 Spring  4/26   		     	    	 ##
###############################################################################
#
#
###############################################################################
##                                Author		                                 ##
##            	               Spencer Wang       		     	                 ##
##            	              Jui-Chung Yang       		     	                 ##
###############################################################################

rm(list=ls(all=T))

####################################################################
# Section 1:  Validation set approach                              #
####################################################################

# Before we begin, let's first introduce the function "set.seed()". This code set a "seed" for the R's random 
# number generator and control the randomness. Everyone can obtain the exact same results after using this function.

rnorm(1) # Here we can see that every time you run this code (take a random sample from "N(0,1)"), a different number shows up.

set.seed(1) # But now if we set a seed "1" in R, then everytime we run these code together, the "ramdon sample" we get will be the same.
rnorm(1)

set.seed(10000) 
rnorm(1)


## Now we examine the Traffic example in our lecture, and see how the results are obtained.

Traffic= read.csv("traffic.csv") #please load the data from the place you store them.
attach(Traffic)

summary(lm(death ~ time))
summary(lm(death ~ auto_incr + time))
summary(lm(death ~ auto_incr + motor_incr + time))

nrow(Traffic) # we have 222 samples

set.seed(1)  # set a seed so that the way of splitting the data is exactly the same for everyone.
train = sample(222,111) # Here we "ramdomly" (using seed 1) select 111 samples from our data set to be our training set.
train

lm.fit= lm(death ~ time, subset = train) # We train our model with the training set.
summary(lm.fit)
mean((predict(lm.fit, Traffic) - death)[-train]^2) # Observe this code carefully, here we calculate the testing MSE of lm.fit
                                                # using our trained model and the previously unused validation set. 

lm.fit2 = lm(death ~ auto_incr + time, subset = train) # Here we calculate the testing MSE for lm.fit2.
summary(lm.fit2)
mean((predict(lm.fit2, Traffic) - death)[-train]^2)

lm.fit3 = lm(death ~ auto_incr + motor_incr + time, subset = train) # Here we calculate the testing MSE for lm.fit3.
summary(lm.fit3)
mean((predict(lm.fit3, Traffic) - death)[-train]^2)


## Now we repeat the above procedure 10 times, each with a different random seed, to demonstrate the "randomness" in splitting the data.

result=NULL
for (i in 1:10)
{
  set.seed(i)
  train = sample(222,111)
  lm.fit = lm(death ~ time, subset = train)
  a = mean((predict(lm.fit, Traffic) - death)[-train]^2)
  
  lm.fit2 = lm(death ~ auto_incr + time, subset = train)
  b = mean((predict(lm.fit2, Traffic) - death)[-train]^2)
  
  lm.fit3 = lm(death ~ auto_incr + motor_incr + time, subset = train)
  c = mean((predict(lm.fit3, Traffic) - death)[-train]^2)
  
  result = cbind(result, as.matrix(c(a, b, c)))
}
result # See how the results differ from one ramdom split to another.



####################################################################
# Section 2:  LOOCV                                                #
####################################################################


library(boot) # The package "boot" is a package that can perform LOOCV easily through the function "cv.glm()".

lm.fit = glm(death ~ time) # Recall that in R lecture 4, we use "glm()" function to perform logistic regression 
                         # by passing in the "family = binomial(link = "probit")" argument. Note that if we use 
                         # "glm()" function without passing the "family" aregument, then it's identical to the 
                         # usual "lm()" function. The reason we use "glm()" is because it can be used together with ?@
                         # the function "cv.glm()" which will help us perform LOOCV with ease.
lm.fit2 = glm(death ~ auto_incr + time)
lm.fit3 = glm(death ~ auto_incr + motor_incr + time)

LOOCV = cv.glm(Traffic, lm.fit) # cv.glm() gives the LOOCV results.
LOOCV2 = cv.glm(Traffic, lm.fit2)
LOOCV3 = cv.glm(Traffic, lm.fit3)

# The following code display the LOOCV estimated tesing MSE for the three different model.
c(LOOCV$delta[1], LOOCV2$delta[1], LOOCV3$delta[1]) 



####################################################################
# Section 3:  k-Fold Cross Validation                              #
####################################################################

library(boot)
lm.fit = glm(death ~ time)
lm.fit2 = glm(death ~ auto_incr + time)
lm.fit3 = glm(death ~ auto_incr + motor_incr + time)

set.seed(2) # We set a seed before performing k-fold CV.
kFCV = cv.glm(Traffic, lm.fit, K = 10)   # The code for performing k-fold CV, note that it is almost the same as LOOCV, except for an additional argument "K=10".
kFCV2 = cv.glm(Traffic, lm.fit2, K = 10)
kFCV3 = cv.glm(Traffic, lm.fit3, K = 10)

# The following code display the k-fold CV estimated testing MSE for the three different model.
c(kFCV$delta[1], kFCV2$delta[1], kFCV3$delta[1])


## Now we repeat the k-fold CV 10 times, each with a different random seed, to demonstrate the "randomness" in splitting the data.
result = NULL

for (i in 1:10)
{
  set.seed(i)
  a = cv.glm(Traffic, lm.fit, K = 10)$delta[1]
  b = cv.glm(Traffic, lm.fit2, K = 10)$delta[1]
  c = cv.glm(Traffic, lm.fit3, K = 10)$delta[1]
  
  result = cbind(result, as.matrix(c(a, b, c)))
}
result




####################################################################
# Section 4:  Bootstrap                                            #
####################################################################

######################################################
# Estimating the Accuracy of a Statistic of Interest #
######################################################
library(MASS)

## Constructing our sample
Sigma = matrix(c(1, 0.5, 0.5, 1.25), nrow = 2, ncol = 2)
mu = c(0, 0)

set.seed(2)
Stock = mvrnorm(n = 100, mu, Sigma) # Here we simulate 100 data from a binomial normal distribution with mean = mu, var-cov matrix = Sigma. 

## Obtaing the optimal fraction of investment
alpha = function(data, index){
  X = data[index, 1]
  Y = data[index, 2]
  return((var(Y) - cov(X,Y)) / (var(X) + var(Y) - 2 * cov(X, Y)))
} # This function returns the estimated "best investment portion".

hat_alpha = alpha(Stock, 1:100)
hat_alpha # This data set indicates that we should invest 61% in X and 39% in Y to minimize our risk.

## We now use bootstrap to estimate the variance of "hat_alpha".

result_boot = NULL

for(i in 1:1000){
  result_boot = c(result_boot, alpha(Stock, sample(100, 100, replace = T)))
}
mean(result_boot) # which is very close to "hat_alpha", as it should.
sd(result_boot)   # The Bootstrap estimation of "Var(hat_alpha)".


## We now use the in-build R code to perform bootstrap.

library(boot)
boot(Stock, alpha, R = 1000) # Bootstrap with the data "Stock", function "alpha", and 1000 replication. 
# The "original" indicates the value for "hat_alpha", and the "std. error" stands for the bootstrap estimation of "SD(hat_alpha)".



########################################################
# Estimating the Accuracy of a Linear Regression Model #
########################################################

library("lmtest")
library("sandwich")
library(boot)

lm.fit2 = lm(death ~ auto_incr + time)
summary(lm.fit2)

coeftest(lm.fit2) # iid case 
coeftest(lm.fit2, vcov = vcovHAC(lm.fit2)) # Ecker-White


##Paired Bootstrap with built-in code
Bootstrap = function(data, index){
  return(coef(lm(death ~ auto_incr + time, data = data, subset = index)))
}
set.seed(1)
boot(Traffic, Bootstrap, 1000) # Paired Bootstrap estimation of SD(beta_hat)
                             # SD(beta1) = 0.0001814633
                             # SD(beta2) = 0.0257054885

##Paired Bootstrap with self-written code
beta1 = NULL
beta2 = NULL
for (i in 1:1000) {
  set.seed(i)
  index = sample(nrow(Traffic), nrow(Traffic), replace = T)
  a = lm(death ~ auto_incr + time, subset = index)
  beta1 = c(beta1, coeftest(a)[2,1])
  beta2 = c(beta2, coeftest(a)[3,1])
}
sd(beta1) # SD(beta1) = 0.0001904233
sd(beta2) # SD(beta2) = 0.02565488

