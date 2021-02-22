X <- matrix(c(2,8,6,8,12,9,9,10), ncol = 2) # data matrix
xbar <- matrix(colMeans(X)) # column means matrix
S <- cov(X) # Sample Covariance matrix
I <- inv(S) # inverse of S 
n <- nrow(X)
mu <- matrix(c(7,11), ncol = 1) # Hypothesis, mu vector

T2 <- n * (t(xbar - mu)) %*% I %*% (xbar - mu) # 13.6
Fstat <- 3*qf(0.05, df1 = 2, df2 = 2, lower.tail = FALSE)
# is T^2 > Fstat? If so, we reject the null hypothesis in favor of H1
T2 > Fstat
qt(0.05/4, 10, lower.tail = FALSE)
