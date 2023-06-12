###############################################################################
##                           HW 2 Answer Sheet         			    		         ##
##            	        Name :__劉德駿_   NTU ID:__b07801013______  	    	     	    	 ##
###############################################################################

##############
# Question 1 #
##############
#(a)# 
X = cbind(rep(1,5), c(7,4,9,0,5), c(2,6,2,9,3), c(3,7,0,0,5))
y = c(6,2,4,2,1)
beta = (solve(t(X)%*%X)%*%t(X))%*%y

#(b)# 
x = c(1,0,4,3)
x %*% beta

##############
# Question 2 #
##############
rm(list=ls(all=T))
data(mtcars)  # load the data set "mtcars" from R
mtcars        # use help(mtcars) to get help from the definition of this dataset in R
attach(mtcars)

#(a)# 
mtcars["Camaro Z28", ]

#(b)#
cbind(rownames(mtcars), mtcars$wt)
#(c)#
library(tidyverse)
mtcars %>% filter(gear == 3)

#(d)# 
mtcars %>% filter(mpg > 10) %>%
  filter(cyl == 6) %>% filter(hp >= 90 & hp <= 110)

#(e)#
X = cbind(rep(1, nrow(mtcars)), mtcars$wt, mtcars$hp, mtcars$qsec, mtcars$vs)
y = mtcars$drat
(beta = (solve(t(X)%*%X)%*%t(X))%*%y)

#(f)#
lm(drat ~ wt+ hp+ qsec+ vs, data = mtcars)$coefficients

