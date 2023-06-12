###############################################################################
##                           HW 4 Answer Sheet         			    		         ##
##            	        Name :______   NTU ID:________  	    	     	    	 ##
###############################################################################

# DGP1 : {y_i}~N(0,1)
# DGP2 : {y_i}~t(4)
# DGP3 : {y_i}~t(1)
# sample size n = {10,500}
# moment functions = {y, y^3, sin(y), cos(y)}
# Number of replications: B = 1000 times
# requirements:
# (1) For the total of 2*3*4=24 ways to construct M_N, plot their corresponding histogram.
# (2) Compute the empirical frequencies of the events: (M_N)^2 > 3:8414588 and (M_N)^2 > 6:6348966 for each simulation graph.
# (3) Add the Gaussian kernel density estimate (KDE) of M_N as well as the probability density function (PDF) of N(0,1) for each simulation graph.

###############################################################################
rm(list=ls(all=T))

# grDevices::windows(width=10,height=10) #open a new window and plot on it
# grDevices::dev.new()
# par(mfrow=c(6,4)) # doesn't work!!
dev.new(height = 20, width = 20) ;par(mfrow=c(6,4), mar=c(4,2,4,2))
# quartz(height = 5,width = 10, pointsize = 6.5)



### New attempt
n_rep = 1000; prob = rep(0, n_rep);
# N = 10; dgp = 1; moment=3;
for(N in c(10, 500, 1000)){
  for(dgp in c(1,2,3)){
    for(moment in c(1,2,3,4)){
for(i in 1:n_rep){
  y_i = cbind(rnorm(N, 0, 1), rt(N, 4), rt(N, 1))[, dgp]
  fi = cbind(y_i, y_i^3, sin(y_i), cos(y_i))[, moment]
  sigma_N = sqrt(sum((fi - mean(fi))^2)/N)
  prob[i] = sum(fi)/(sigma_N*sqrt(N))
}
name_dist = c("N(0,1)", "t(4)", "t(1)")
name_moment = c("y", "y^3", "sin(y)", "cos(y)")
hist(prob,
     main=paste("N =", N, name_dist[dgp], name_moment[moment]),
     xlab=paste("pr(>3.8) =", sum((prob^2) >= 3.8414588)/length(prob),
                "/ pr(>6.6) =",sum((prob^2) >= 6.6348966)/length(prob)),
     col="darkmagenta",
     freq=FALSE
)
lines(density(prob, kernel = "gaussian"), # density plot
      lwd = 2, # thickness of line
      col = "chocolate3")
lines(density(rnorm(n_rep, 0, 1)),lwd = 2, col = "blue")
}}}


## notes

## 試了2個小時後，發現開新視窗 & 分割成24個 widges在同一張圖是比較困難
## 的，請助教手動修改第48行程式碼的參數組合，即可觀察不同的圖。謝謝！
