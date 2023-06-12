###############################################################################
##                           HW 8 Answer Sheet         			    		         ##
##            	        Name :_劉德駿_____   NTU ID:_b07801013____  	    	     	    	 ##
###############################################################################
rm(list=ls(all=T))

# DGP_1 with 25 regressors
library(glmnet)
dev.new(height = 20, width = 20) ;par(mfrow=c(2,2), mar=c(4,2,4,2))

ans = 0; opt_lambda = 0;
for(rep in 1:100){
X = cbind(rnorm(55), rnorm(55)); beta = rbind(1, 1)
Y = X %*% beta + cbind(rnorm(55)) + 2
Y = as.numeric(Y) # Just in case
for(i in 3:25){
  # set.seed(i)
  X = cbind(X, rnorm(55)) # noise variables produced!
}
grid = seq(0,2,length = 100)
ridge.fit = glmnet(X,Y,alpha = 0, lambda = grid) 
ridge.kfold = cv.glmnet(X,Y,alpha=0, nfolds=10, lambda = grid)
opt_lambda = c(opt_lambda, ridge.kfold$lambda.min )
ans = ans + coef(ridge.fit)
}
ans = ans/100
opt_lambda = opt_lambda[-1]
opt_lambda = mean(opt_lambda)

plot(ridge.fit$lambda,ans[2,], col = "red", type = "l", ylim= c(-1.5, 1.5), 
     xlab = "lambda", ylab = ".", main = "DGP_1 with 25 regressors")
for(i in 3){
  lines(ridge.fit$lambda,ans[i,], col = "red")
}
for (i in 4:26){
  lines(ridge.fit$lambda,ans[i,])}
abline(v = opt_lambda, col="blue", lwd=1, lty=2)
text(x=1.2, y=1.2,label="Single Variables' avg coefs", cex = 0.5, col = "red")
text(x=1.2, y=-0.5,label="Noise Variables' avg coefs", cex = 0.5, col = "black")
text(x=opt_lambda, y=-1,label="Avg optimal lambda", cex = 0.5, col = "blue")

# DGP_1 with 50 regressors
ans2 = 0; opt_lambda = 0;
for(rep in 1:100){
  X = cbind(rnorm(55), rnorm(55)); beta = rbind(1, 1)
  Y = X %*% beta + cbind(rnorm(55)) + 2
  Y = as.numeric(Y) # Just in case
  for(i in 3:50){
    # set.seed(i)
    X = cbind(X, rnorm(55)) # noise variables produced!
  }
  grid = seq(0,2,length = 100)
  ridge.fit = glmnet(X,Y,alpha = 0, lambda = grid) 
  ridge.kfold = cv.glmnet(X,Y,alpha=0, nfolds=10, lambda = grid)
  opt_lambda = c(opt_lambda, ridge.kfold$lambda.min )
  ans2 = ans2 + coef(ridge.fit)
}
ans2 = ans2/100
opt_lambda = opt_lambda[-1]
opt_lambda = mean(opt_lambda)

plot(ridge.fit$lambda,ans2[2,], col = "red", type = "l", ylim= c(-1.5, 1.5), 
     xlab = "lambda", ylab = ".", main = "DGP_1 with 50 regressors")
for(i in 3){
  lines(ridge.fit$lambda,ans2[i,], col = "red")
}
for (i in 4:51){
  lines(ridge.fit$lambda,ans2[i,])}
abline(v = opt_lambda, col="blue", lwd=1, lty=2)
text(x=1.2, y=1.2,label="Single Variables' avg coefs", cex = 0.5, col = "red")
text(x=1.2, y=-0.5,label="Noise Variables' avg coefs", cex = 0.5, col = "black")
text(x=opt_lambda, y=-1,label="Avg optimal lambda", cex = 0.5, col = "blue")

# DGP_2 with 25 regressors

ans3 = 0; opt_lambda = 0;
for(rep in 1:100){
  X = cbind(rnorm(55), rnorm(55)); beta = rbind(1, 1)
  for(i in 3:20){
    X = cbind(X, rnorm(55)); beta = rbind(beta, 1)
  }
  # X = cbind(rnorm(55), rnorm(55)); beta = rbind(1, 1)
  Y = X %*% beta + cbind(rnorm(55)) + 2
  Y = as.numeric(Y) # Just in case
  for(i in 21:25){
    # set.seed(i)
    X = cbind(X, rnorm(55)) # noise variables produced!
  }
  grid = seq(0,2,length = 100)
  ridge.fit = glmnet(X,Y,alpha = 0, lambda = grid) 
  ridge.kfold = cv.glmnet(X,Y,alpha=0, nfolds=10, lambda = grid)
  opt_lambda = c(opt_lambda, ridge.kfold$lambda.min )
  ans3 = ans3 + coef(ridge.fit)
}
ans3 = ans3/100
opt_lambda = opt_lambda[-1]
opt_lambda = mean(opt_lambda)

plot(ridge.fit$lambda,ans3[2,], col = "red", type = "l", ylim= c(-1.5, 1.5), 
     xlab = "lambda", ylab = ".", main = "DGP_2 with 25 regressors")
for(i in 3:21){
  lines(ridge.fit$lambda,ans3[i,], col = "red")
}
for (i in 22:26){
  lines(ridge.fit$lambda,ans3[i,])}
abline(v = opt_lambda, col="blue", lwd=1, lty=2)
text(x=1.2, y=1.2,label="Single Variables' avg coefs", cex = 0.5, col = "red")
text(x=1.2, y=-0.5,label="Noise Variables' avg coefs", cex = 0.5, col = "black")
text(x=opt_lambda, y=-1,label="Avg optimal lambda", cex = 0.5, col = "blue")

# DGP_2 with 50 regressors

ans4 = 0; opt_lambda = 0;
for(rep in 1:100){
  X = cbind(rnorm(55), rnorm(55)); beta = rbind(1, 1)
  for(i in 3:20){
    X = cbind(X, rnorm(55)); beta = rbind(beta, 1)
  }
  # X = cbind(rnorm(55), rnorm(55)); beta = rbind(1, 1)
  Y = X %*% beta + cbind(rnorm(55)) + 2
  Y = as.numeric(Y) # Just in case
  for(i in 21:50){
    # set.seed(i)
    X = cbind(X, rnorm(55)) # noise variables produced!
  }
  grid = seq(0,2,length = 100)
  ridge.fit = glmnet(X,Y,alpha = 0, lambda = grid) 
  ridge.kfold = cv.glmnet(X,Y,alpha=0, nfolds=10, lambda = grid)
  opt_lambda = c(opt_lambda, ridge.kfold$lambda.min )
  ans4 = ans4 + coef(ridge.fit)
}
ans4 = ans4/100
opt_lambda = opt_lambda[-1]
opt_lambda = mean(opt_lambda)

plot(ridge.fit$lambda,ans4[2,], col = "red", type = "l", ylim= c(-1.5, 1.5), 
     xlab = "lambda", ylab = ".", main = "DGP_2 with 50 regressors")
for(i in 3:21){
  lines(ridge.fit$lambda,ans3[i,], col = "red")
}
for (i in 22:51){
  lines(ridge.fit$lambda,ans3[i,])}
abline(v = opt_lambda, col="blue", lwd=1, lty=2)
text(x=1.2, y=1.2,label="Single Variables' avg coefs", cex = 0.5, col = "red")
text(x=1.2, y=-0.5,label="Noise Variables' avg coefs", cex = 0.5, col = "black")
text(x=opt_lambda, y=-1,label="Avg optimal lambda", cex = 0.5, col = "blue")

