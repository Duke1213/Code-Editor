###############################################################################
##                           HW 9 Answer Sheet         			    		         ##
##            	        Name :__劉德駿____   NTU ID:__b07801013______  	    	     	    	 ##
###############################################################################
rm(list=ls(all=T))
library(ISLR)
attach(Wage)
library(splines)
library(boot)
library(tidyverse)
##############
# Question 1 #
##############
mse = 9999; # best_model = paste0("model_", 0)
for(i in 1:5){
  mse_0 = rep(0, 50)
  for(iter in 1:50){
    set.seed(iter)
    index = sample(nrow(Wage), 0.98*nrow(Wage))
    train = Wage[index, ]; test = Wage[-index, ]
    model_0 = lm(wage~poly(age,i), data =  train)
    fitted_v = predict(model_0, newdata = tibble(age = test$age))
    mse_0[iter] = mean((fitted_v - test$wage)^2)
  }
  if(mean(mse_0) < mse){
    mse = mean(mse_0)
    cat("The best model is polynomial with degree ", i, " with MSE ", 
        mean(mse_0), " after 50-fold cv.", "\n")
  }
}

# optimal degree = 4

##############
# Question 2 #
##############

#####
#(a)# 
#####
mse = 9999
for(knot in 1:5){
  mse_0 = rep(0, 10)
  for(iter in 1:10){
    set.seed(iter)
    index = sample(nrow(Wage), 0.9*nrow(Wage))
    train = Wage[index, ]; test = Wage[-index, ]
    model_0 = lm(wage ~ bs(age,df = knot + 3, degree = 3), data=train)
    fitted_v = predict(model_0, newdata = tibble(age = test$age))
    mse_0[iter] = mean((fitted_v - test$wage)^2)
  }
  if(mean(mse_0) < mse){
    mse = mean(mse_0)
    cat("The best model is cubic spline with ", knot, " knots,", " with MSE ", 
        mean(mse_0), " after 10-fold cv.", "\n")
  }
}
attr(bs(age,df = 2 + 3, degree = 3), "knot")

# number of optimal interior knots = 2

#####
#(b)# 
#####
knot = 2; # set.seed(2)
index = sample(nrow(Wage), 0.9*nrow(Wage))
train = Wage[index, ]; test = Wage[-index, ]
model_0 = lm(wage ~ bs(age,df = knot + 3, degree = 3), data=train)
fitted_v = predict(model_0, newdata = tibble(age = test$age), se = T)

tibble(Age = test$age,
       Predict = unlist(fitted_v[1]),
       Wage = test$wage, 
       confi_upper = unlist(fitted_v[1]) + 2*unlist(fitted_v[2]), 
       confi_lower = unlist(fitted_v[1]) - 2*unlist(fitted_v[2])) %>%
  arrange(Age) %>%
  ggplot() + aes(Age, Wage) + geom_jitter(colour = "grey", alpha = 0.6) +
  theme_bw() +
  geom_line(aes(Age, Predict), colour = "blue") + 
  geom_line(aes(Age, confi_upper), linetype = "dotted") +
  geom_line(aes(Age, confi_lower), linetype = "dotted") +
  labs(title = "2 knots Cubic Spline with fitted value and Se", 
       subtitle = "Testing Sets with 10% original samples")


##############
# Question 3 #
##############

#####
#(a)# 
#####
candy = list(c(quantile(age, .05), quantile(age, .5), quantile(age, .95)),
             c(quantile(age, .1), quantile(age, .5), quantile(age, .9)),
             c(quantile(age, .15), quantile(age, .5), quantile(age, .85)),
             c(quantile(age, .2), quantile(age, .5), quantile(age, .8)))
mse = 9999
for(knot in candy){
  mse_0 = rep(0, 10)
  for(iter in 1:10){
    set.seed(iter)
    index = sample(nrow(Wage), 0.9*nrow(Wage))
    train = Wage[index, ]; test = Wage[-index, ]
    model_0 = lm(wage~ns(age,df=4, knots = unlist(knot)),data=train)
    fitted_v = predict(model_0, newdata = tibble(age = test$age))
    mse_0[iter] = mean((fitted_v - test$wage)^2)
  }
  if(mean(mse_0) < mse){
    mse = mean(mse_0)
    cat("The best model is natural cubic spline with knots ", names(unlist(knot)), " with MSE ", 
        mean(mse_0), " after 10-fold cv.", "\n")
  }
}
quantile(age, c(.05, .95))
# Optimal boundary quantiles = 5%, 95%
# Location of the boundary knots = [24, 61]

#####
#(b)# 
#####
knot = candy[1]
index = sample(nrow(Wage), 0.9*nrow(Wage))
train = Wage[index, ]; test = Wage[-index, ]
model_0 = lm(wage~ns(age,df=4, knots = unlist(knot)),data=train)
fitted_v = predict(model_0, newdata = tibble(age = test$age), se = T)

model_cube = lm(wage ~ bs(age,df = 2 + 3, degree = 3), data=train)
fitted_v_cube = predict(model_cube, newdata = tibble(age = test$age), se = T)

tibble(Age = test$age,
       Predict = unlist(fitted_v[1]),
       Wage = test$wage, 
       confi_upper = unlist(fitted_v[1]) + 2*unlist(fitted_v[2]), 
       confi_lower = unlist(fitted_v[1]) - 2*unlist(fitted_v[2]),
       Predict_cube = unlist(fitted_v_cube[1])) %>%
  arrange(Age) %>%
  ggplot() + aes(Age, Wage) + geom_jitter(colour = "grey", alpha = 0.6) +
  theme_bw() +
  geom_line(aes(Age, Predict), colour = "red") + 
  geom_line(aes(Age, Predict_cube), colour = "blue") +
  geom_line(aes(Age, confi_upper), linetype = "dotted") +
  geom_line(aes(Age, confi_lower), linetype = "dotted") +
  labs(title = "3 knots Natural Cubic Spline with fitted value and Se", 
       subtitle = "Compared with 2 knots cubic spline")


