###############################################################################
##                           HW 10 Answer Sheet         			    		       ##
##            	        Name :___劉德駿___   NTU ID:__B07801013______  	    	     	    	 ##
###############################################################################
rm(list=ls(all=T))
library(tree)
library(ISLR)
attach(Carseats) 
library(randomForest)
library(MASS)
library(tidyverse)
library(gbm)
Boston

##############
# Question 1 #
##############


#####
# a #
#####
set.seed(1)
train = sample(1:nrow(Boston), nrow(Boston)/2)
tree.boston=tree(medv~.,Boston,subset=train)
set.seed(1)
cv.boston=cv.tree(tree.boston, K=100) 
plot(prune.tree(tree.boston,best=7))
text(prune.tree(tree.boston,best=7),pretty=0)

#####
# b #
#####
boost.boston_1 = gbm(medv~., data=Boston[train,], distribution="gaussian", n.trees=1000, interaction.depth=1, shrinkage = 0.1, bag.fraction = 1, cv.folds=10)
which.min(boost.boston_1$cv.error) 
boost.boston_2 = gbm(medv~., data=Boston[train,], distribution="gaussian", n.trees=1000, interaction.depth=2, shrinkage = 0.1, bag.fraction = 1, cv.folds=10)
which.min(boost.boston_2$cv.error) 
boost.boston_3 = gbm(medv~., data=Boston[train,], distribution="gaussian", n.trees=1000, interaction.depth=3, shrinkage = 0.1, bag.fraction = 1, cv.folds=10)
which.min(boost.boston_3$cv.error) 
boost.boston_4 = gbm(medv~., data=Boston[train,], distribution="gaussian", n.trees=1000, interaction.depth=4, shrinkage = 0.1, bag.fraction = 1, cv.folds=10)
which.min(boost.boston_4$cv.error) 
# The optimal tree for m=1 is : 998


# The optimal tree for m=2 is :928


# The optimal tree for m=3 is :669


# The optimal tree for m=4 is :335


#####
# c #
#####

boost.boston_1$cv.error[998]
boost.boston_2$cv.error[928]
boost.boston_3$cv.error[669]
boost.boston_4$cv.error[335]

# The smallest 10-fold cv error is the model with m = 4


##############
# Question 2 #
##############
rm(list=ls(all=T))

#####
# a #
#####
Boston = tibble(Boston) %>%
  mutate(High = as.factor(ifelse(medv<=22,"No","Yes"))) 

tree_boston = tree(High~.-medv,Boston, split = "gini", mincut = 5) 
set.seed(1)
cv_boston = cv.tree(tree_boston, FUN=prune.misclass ,K = 100) 

prune_boston = prune.misclass(tree_boston,best=6)
plot(prune_boston)
text(prune_boston,pretty=0)
#####
# b #
#####
rm(list=ls(all=T))
train = sample(nrow(Boston), 0.5*nrow(Boston))
set.seed(1)
bag_boston=randomForest(medv~., data=Boston, subset=train, mtry=(ncol(Boston)-1), ntree=500,importance=TRUE)
rf_boston=randomForest(medv~.,data=Boston,subset=train,mtry=3,importance=TRUE) 

set.seed(1)
bag_boston=randomForest(medv~., data=Boston, mtry=(ncol(Boston)-1))
set.seed(1)
rf_boston=randomForest(medv~., data=Boston, mtry=3) 

plot(bag_boston$mse, type = "l", xlab = "Number of Trees", ylab = "OOB error", cex.lab = 1.2, col="blue") # OOB error for bagging
a =length(rf_boston$mse)
lines(seq(from = 1,to = a, length =a), rf_boston$mse, col = "red") # OOB error for random forest
legend("topright",legend = c("Bagging", "Random Forest"),col = c("blue", "red"), lty = c(1, 1))


