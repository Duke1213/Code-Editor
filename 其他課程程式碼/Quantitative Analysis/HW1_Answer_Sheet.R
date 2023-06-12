# 1.
x = seq(1, 150)
x[which(x > 135 | x <= 5)]
x[which(x > 70 & x < 90)]
x[which(x %% 4 == 0 & x %% 5 ==0)]

# 2.a-d
X = rnorm(150000, 0, 1) 

mean(X); median(X); max(X); min(X); sd(X)^2

Y = sample(X, 5000, replace = F); mean(Y); sd(Y)^2

Z = sample(X, 5000, replace = T); mean(Z); sd(Z)^2
# e
quantile(X, 0.45); qnorm(0.45, 0, 1)
# f
library(tidyverse)
tibble(X = X) %>%
  ggplot(aes(X)) + geom_density()
a = pnorm(1.25 , 0, 1) - pnorm(-0.55, 0, 1)

# 3. 
X = cbind(rep(1, 6), seq(2, 12, 2), seq(1, 16, 3))
Y = cbind(seq(1, 6), seq(9, 4, -1))
Z = cbind(X[, 1] + Y[, 1], X[,2], X[,3] - 2*Y[,2])
