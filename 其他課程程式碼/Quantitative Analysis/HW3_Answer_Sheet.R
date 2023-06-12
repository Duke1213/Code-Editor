###############################################################################
##                           HW 3 Answer Sheet         			    		         ##
##            	        Name :______   NTU ID:________  	    	     	    	 ##
###############################################################################

rm(list=ls(all=T))
data(mtcars)  # load the data set "mtcars" from R
mtcars        # use help(mtcars) to get help from the definition of this dataset in R
attach(mtcars)

##############
# Question 1 #
##############
rm(list=ls(all=T))


#(a)#
n = 32; k = 4;
y = mtcars$drat
X = cbind(1, mtcars$wt, mtcars$hp, mtcars$qsec, mtcars$vs)
beta_hat = solve(t(X) %*% X) %*% t(X) %*% y

R = matrix(0, 1, 5); R[1, 2] = 1;
sigma_hat = sum((y-(X %*% beta_hat))^2)/(n-k-1)
# 這邊手算的sigma_hat的數字怪怪的，故在下一步驟直接用sigma()取代
model = lm(drat ~ wt + hp + qsec + vs, data = mtcars)
t_statistic = R %*% beta_hat / (sigma(model) * sqrt(R %*% solve(t(X)%*%X) %*% t(R)))

#(b)#
coef(summary(model))[, "t value"]

## 以上兩題算出來的結果類似

##############
# Question 2 #
##############
rm(list=ls(all=T))


#(a)#
q = 2; n = 32; k = 4;
restricted_model = lm(drat ~ qsec + vs, data = mtcars)
unrestricted_model = lm(drat ~ wt + hp + qsec + vs, data = mtcars)

R_ur = summary(unrestricted_model)$r.squared
R_r = summary(restricted_model)$r.squared

((R_ur - R_r)/q)/((1-R_ur)/(n-k-1))

#(b)#
y = mtcars$drat
X = cbind(1, mtcars$wt, mtcars$hp, mtcars$qsec, mtcars$vs)
beta_un = coef(summary(unrestricted_model))[, 'Estimate']
SSR_un = sum((y-X%*%beta_un)^2)

X = cbind(1, mtcars$qsec, mtcars$vs)
beta_r = coef(summary(restricted_model))[, 'Estimate']
SSR_r = sum((y-X%*%beta_r)^2)

((SSR_r - SSR_un)/q)/(SSR_un/(n-k-1))

#(c)# 
library(car)
linearHypothesis(unrestricted_model, c("wt=0", "hp=0"))

### 以上3題算出來的結果類似
