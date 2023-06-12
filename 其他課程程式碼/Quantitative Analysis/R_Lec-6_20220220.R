###############################################################################
##                   R LECTURE 6: Binary Choice Models              		     ##
##            	 Quantitative Analysis - 2022 Spring  3/28   		             ##
###############################################################################


###############################################################################
##                                Author		                                 ##
##            	            ??‹å?ƒç¿° Spencer Wang   		                	     ##
###############################################################################

rm(list=ls(all=T))

####################################################################
# Section 1:  The Linear Probability Model                         #
####################################################################
library(AER)
data(HMDA) #HMDA provides data that relate to mortgage applications in Boston in the year of 1990.
summary(HMDA)

#The variable we are interested in modelling is "deny", an indicator for whether an applicant??s mortgage application has 
#been accepted (deny = no) or denied (deny = yes). A regressor that ought to have power in explaining whether a mortgage 
#application has been denied is "pirat", the applicant??s payment to income ratio.

# convert 'deny' to numeric
HMDA$deny = as.numeric(HMDA$deny) - 1  #Note that as.numeric(HMDA$deny) will turn deny = no into "deny = 1" and deny = yes into "deny = 2", 
                                       #so using as.numeric(HMDA$deny)-1 we obtain the binary values 0 and 1.

###########################################
# estimate a simple linear probabilty model
denymod1 <- lm(deny ~ pirat, data = HMDA)
denymod1 #According to the estimated model, a payment-to-income ratio of 1
         #is associated with an expected probability of mortgage application denial of roughly 50%.

#######
# plot 
plot(x = HMDA$pirat,y = HMDA$deny,main = "Mortgage Application Denial and the Payment-to-Income Ratio",xlab = "P/I ratio",
     ylab = "Deny",pch = 20,ylim = c(-0.4, 1.4),cex.main = 0.8)

# add horizontal dashed lines and text
abline(h = 1, lty = 2, col = "darkred")
abline(h = 0, lty = 2, col = "darkred")
text(2.5, 0.9, cex = 0.8, "Mortgage denied")
text(2.5, -0.1, cex= 0.8, "Mortgage approved")

# add the estimated regression line
abline(denymod1, lwd = 1.8, col = "steelblue")



####################################################################
# Section 2:  Probit Regression                                    #
####################################################################

#In R, Probit models can be estimated using the function "glm()". 
#Using the argument "family" we specify that we want to use a Probit model.

###################################
# estimate a simple probit model
data(HMDA)
HMDA$deny = as.numeric(HMDA$deny) - 1
denyprobit <- glm(deny ~ pirat,family = binomial(link = "probit"),data = HMDA)

coeftest(denyprobit, vcov. = vcovHC, type = "HC0") # Note that the coefficient is significant. Also, It is essential to use robust standard errors since the  
                                                   # residuals here are always heteroskedastic.

#######
# plot 
plot(x = HMDA$pirat,y = HMDA$deny,main = "Probit Model of the Probability of Denial, Given P/I Ratio",
     xlab = "P/I ratio",ylab = "Deny",pch = 20,ylim = c(-0.4, 1.4),cex.main = 0.85)

# add horizontal dashed lines and text
abline(h = 1, lty = 2, col = "darkred")
abline(h = 0, lty = 2, col = "darkred")
text(2.5, 0.9, cex = 0.8, "Mortgage denied")
text(2.5, -0.1, cex= 0.8, "Mortgage approved")

# add estimated regression line
x <- seq(0, 3, 0.01)
y <- predict(denyprobit, list(pirat = x), type = "response")
lines(x, y, lwd = 1.5, col = "steelblue")

#The function is clearly nonlinear and flattens out for large and small values of P/I ratio.
#The functional form thus also ensures that the predicted conditional probabilities of a denial lie between 0 and 1.

#############
# Predictions

# compute predictions for P/I ratio = 0.3, 0.5
predict(denyprobit,newdata = data.frame("pirat" = c(0.3, 0.5)),type = "response")
#That is, for a person with P/I ration = 0.5, the probability of being reject is around 23.88%, which is
# very different from what linear probability model estimated.

###########################################
# estimate a multiple variable probit model

# We continue by using a multiple variable Probit model to estimate the effect of race on the probability of a mortgage application denial.
# Note that the variable "afam" equals 1 if the applicant is an African American and equals 0 otherwise. 

denyprobit2 <- glm(deny ~ pirat + afam,family = binomial(link = "probit"), data = HMDA)
coeftest(denyprobit2, vcov. = vcovHC, type = "HC0")

# How big is the estimated difference in denial probabilities between two hypothetical applicants with the same payments-to-income ratio?
# First, we compute predictions for P/I ratio = 0.3
predictions <- predict(denyprobit2,newdata = data.frame("afam" = c("no", "yes"),"pirat" = c(0.3, 0.3)),type = "response")
# We now compute the difference in probabilities
diff(predictions)
#In this case, the estimated difference in denial probabilities is about 15.78%.



####################################################################
# Section 3:  Logit Regression                                     #
####################################################################

#It is very straightforward to swich from Probit model to Logit in R

###############################
# estimate a simple Logit model
library(AER)
data(HMDA)
HMDA$deny = as.numeric(HMDA$deny) - 1

denylogit <- glm(deny ~ pirat,family = binomial(link = "logit"), data = HMDA)
coeftest(denylogit, vcov. = vcovHC, type = "HC0")


#######
# plot 

#The subsequent code chunk plot the two models together.
plot(x = HMDA$pirat, 
     y = HMDA$deny,
     main = "The Probability of Denial, Given P/I Ratio",
     xlab = "P/I ratio",
     ylab = "Deny",
     pch = 20,
     ylim = c(-0.4, 1.4),
     cex.main = 0.9)

# add horizontal dashed lines and text
abline(h = 1, lty = 2, col = "darkred")
abline(h = 0, lty = 2, col = "darkred")
text(2.5, 0.9, cex = 0.8, "Mortgage denied")
text(2.5, -0.1, cex= 0.8, "Mortgage approved")

# add estimated regression line of Probit and Logit models
x <- seq(0, 3, 0.01)
y_probit <- predict(denyprobit, list(pirat = x), type = "response")
y_logit <- predict(denylogit, list(pirat = x), type = "response")

lines(x, y_probit, lwd = 1.5, col = "steelblue")
lines(x, y_logit, lwd = 1.5, col = "black", lty = 2)

# add a legend
legend("topleft",
       horiz = TRUE,
       legend = c("Probit", "Logit"),
       col = c("steelblue", "black"), 
       lty = c(1, 2))


