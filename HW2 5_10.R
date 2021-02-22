# Note: Bear data imported from excel 
install.packages("ellipse")
# Inputs -------------------------------------------------------------

test <- data.frame(X1_10_Bear_Data)
df <- X1_10_Bear_Data # restrict to just lengths
df <- data.frame(cbind(df$lngth2, df$lngth3, df$lngth4, df$lngth5))
M <- data.matrix(colMeans(df)) # column means of the lengths
S <- cov(df) # Sample Covariance matrix 
n <- nrow(df)
p <- ncol(df)
f <- (p*(n-1) / (n-p))*qf(0.05, df1 = p, df2 = n-p, lower.tail = FALSE)

# PART (A) -----------------------------------------------------------

# LNGTH2
M[1,1] - sqrt(f) * sqrt(S[1,1]/n) # 130.6851
M[1,1] + sqrt(f) * sqrt(S[1,1]/n) # 155.8863
# LNGHT3
M[2,1] - sqrt(f) * sqrt(S[2,2]/n) # 127.0216
M[2,1] + sqrt(f) * sqrt(S[2,2]/n) # 191.5498
# LNGTH4
M[3,1] - sqrt(f) * sqrt(S[3,3]/n) # 160.3082
M[3,1] + sqrt(f) * sqrt(S[3,3]/n) # 185.9776
# LNGTH5
M[4,1] - sqrt(f) * sqrt(S[3,3]/n) # 164.3082
M[4,1] + sqrt(f) * sqrt(S[3,3]/n) # 189.9776

# PART (B) ------------------------------------------------------------
# Note: using yearly increase in mean length

M2 <- data.matrix(cbind(df$X2 - df$X1, df$X3 - df$X2, df$X4 - df$X3))
M2M <- data.matrix(colMeans(M2))
S2 <- cov(M2)

# LNGHT3
M2M[1,1] - sqrt(f) * sqrt(S2[1,1]/n) # -21.22649
M2M[1,1] + sqrt(f) * sqrt(S2[1,1]/n) # 53.22649
# LNGTH4
M2M[2,1] - sqrt(f) * sqrt(S2[2,2]/n) # -22.73077
M2M[2,1] + sqrt(f) * sqrt(S2[2,2]/n) # 50.44505
# LNGTH5
M2M[3,1] - sqrt(f) * sqrt(S2[3,3]/n) # -20.65385
M2M[3,1] + sqrt(f) * sqrt(S2[3,3]/n) # 28.65385

# PART (C) ------------------------------------------------------------

M3 <- data.matrix(cbind(test$lngth3 - test$lngth2, test$lngth5 - test$lngth4))
M3M <- data.matrix(colMeans(M3))
S3 <- cov(M3)
I2 <- inv(S3)
E <- eigen(S3) # meant for ellipse axis

library(ellipse)
plot(ellipse(S3, centre = 0.585, t=f, npoints = 1000, type="l",xlim=c(-40,60),ylim=c(-40,40), 
     main="95% Confidence Ellipse for \nSuccessive Yearly Length Increases \nYear3-Year2 and Year5-Year4", xlab = "Year3-Year2",
     ylab = "Year5-Year4"))

# PART (D) ------------------------------------------------------------
# Bonferroni Confidence Intervals

criticalvalue <- qt(.05/(14), 6, lower.tail = F) # originally computed the incorrect t value

M[1,1] - criticalvalue * sqrt(S[1,1]/n) # 125.8629
M[1,1] + criticalvalue * sqrt(S[1,1]/n) # 160.7085

M[2,1] - criticalvalue * sqrt(S[2,2]/n) # 145.7472
M[2,1] + criticalvalue * sqrt(S[2,2]/n) # 170.8242

M[3,1] - criticalvalue * sqrt(S[3,3]/n) # 167.1359
M[3,1] + criticalvalue * sqrt(S[3,3]/n) # 179.1498

M[4,1] - criticalvalue * sqrt(S[4,4]/n) # 166.9950
M[4,1] + criticalvalue * sqrt(S[4,4]/n) # 187.3307

# PART (E) ------------------------------------------------------------

