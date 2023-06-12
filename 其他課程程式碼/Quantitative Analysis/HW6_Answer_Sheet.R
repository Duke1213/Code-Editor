###############################################################################
##                           HW 5 Answer Sheet         			    		         ##
##            	        Name :__劉德駿____   NTU ID:__b07801013______  	    	     	    	 ##
###############################################################################
library(AER)
data(HMDA)
HMDA$deny = as.numeric(HMDA$deny) - 1 
##############
# Question 1 #
##############
#(a)# 
denylogit <- glm(deny ~ hirat,family = binomial(link = "logit"), data = HMDA)
coeftest(denylogit, vcov. = vcovHC, type = "HC0")

#(b)# 
predict(denylogit, list(hirat = c(0.2, 0.8)), type = "response")

#(c)#
cat("同上")

#(d)# 
denylogit2 <- glm(deny ~ hirat + afam,family = binomial(link = "logit"), data = HMDA)

#(e)#
library(tidyverse)
predict(denylogit2, tibble(
  hirat = 0.2, 
  afam = as.factor(c("yes","no"))
), type = "response")

#(f)#
predict(denylogit2, tibble(
  hirat = 0.8, 
  afam = as.factor(c("yes","no"))
), type = "response")

