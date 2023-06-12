###############################################################################
##                           HW 11 Solution            			    		         ##
###############################################################################
rm(list=ls(all=T))

##############
# Question 1 #
##############

#####
# a #
#####
library(ISLR)
data = Carseats
n <- names(data)
data = Carseats[n[!n %in% c("ShelveLoc","Urban","US")]]
set.seed(1234)
index <- sample(1:nrow(data),round(0.8*nrow(data)))
train <- data[index,]
test <- data[-index,]

lm.fit <- glm(Sales~., data=train)
pr.lm <- predict(lm.fit,test)
MSE.lm <- sum((pr.lm - test$Sales)^2)/nrow(test)

# MSE = 4.419823



#####
# b #
#####
maxs <- apply(data, 2, max)
mins <- apply(data, 2, min)
scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins)) 
set.seed(1234)
train_ <- scaled[index,]
test_ <- scaled[-index,]

library(neuralnet)
n <- names(train_)
f <- as.formula(paste("Sales ~", paste(n[!n %in% "Sales"], collapse = " + "))) 
nn <- neuralnet(formula=f,data=train_,hidden=c(5,3,2),linear.output=T)
windows()
plot(nn)

pr.nn <- compute(x = nn, covariate = test_[,2:8]) 
pr.nn_ <- pr.nn$net.result*( max(data$Sales)-min(data$Sales) ) + min(data$Sales) 
MSE.nn <- sum((test$Sales - pr.nn_)^2)/nrow(test)
MSE.nn
#MSE = 5.634624


#####
# c #
#####

print(paste(MSE.lm,MSE.nn))

# linear regression performs better



#####
# d #
#####
windows()
par(mfrow=c(1,2))
plot(test$Sales,pr.nn_,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='NN',pch=18,col='red', bty='n')
plot(test$Sales,pr.lm,col='blue',main='Real vs predicted lm',pch=18, cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='LM',pch=18,col='blue', bty='n', cex=.95)
