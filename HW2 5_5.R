# Defining Pre-Requisites 
#-------------------------
x <- matrix(c(0.564, 0.603), ncol = 1) # X means
S <- matrix(c(0.0144, 0.0117, 0.0177, 0.0146), ncol = 2, nrow = 2 )
I <- matrix(c(203.018, -163.391, -163.391, 200.228), ncol = 2) # inverse of S 
mu <- matrix(c(0.55, 0.60), ncol = 1)
n <- 42 # observations 
p <- 2 # variables
#-------------------------
# construct a test of mu at 0.05. 

L <- n * t(x - mu) %*% I %*% (x - mu) # LHS 
f <-  (p*(n-1) / (n - p))*qf(0.05, df1 = p, df2 = n-p, lower.tail = FALSE)

# Test if L \leq f
L <= f # TRUE, we conclude by (5-18) mu is in the confidence region ellipsoid
