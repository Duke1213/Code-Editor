###############################################################################
##                           HW 5 Answer Sheet         			    		         ##
##            	        Name :__劉德駿____   NTU ID:__b07801013______  	    	     	    	 ##
###############################################################################

install.packages("ivreg")
library(ivreg)
data("SchoolingReturns", package = "ivreg")
attach(SchoolingReturns)

##############
# Question 1 #
##############
#(a)# 
lm.stage1_x1 = lm(education~nearcollege + age + age^2 + 
                    ethnicity + smsa + south )
x1.hat = fitted.values(lm.stage1_x1)
lm.stage1_x2 = lm(experience~nearcollege + age + age^2 + 
                    ethnicity + smsa + south )
x2.hat = fitted.values(lm.stage1_x2)
lm.stage1_x3 = lm(experience^2~nearcollege + age + age^2 + 
                    ethnicity + smsa + south )
x3.hat = fitted.values(lm.stage1_x3)
#(b)# 
lm.stage2 = lm(log(wage) ~ x1.hat + x2.hat + x3.hat +
                 ethnicity + smsa + south)
lm.stage2$coefficients[2]
#(c)#
iv <- ivreg(log(wage) ~ education + poly(experience, 2) |
              nearcollege + poly(age, 2) |
              ethnicity + smsa + south , data = SchoolingReturns)
              
iv$coefficients[2] 

## 結果顯示c 小題的(b) 和 （c）的estimator的估計值幾乎一樣。

## 如果分開來跑的話，前兩個stage都必須放入外生變數作為解釋變數。感謝助教解答！

