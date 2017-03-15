X <- as.matrix( read.table("STRIKES.TSM", header=FALSE) )
mu <- mean(X)
plot(X)

#Fitting a linear model to the data by Least Squares
model <- lm(formula = X ~ seq(1:30) )
R <- model$residuals

#Fitting AR model to the data by Ordinary Least Squares
noise <- ar(x = R, aic = FALSE, order.max = 1, method = "ols", intercept = FALSE)
phi <- as.double(noise[2])
sigma_squared <- as.double(noise[3])

#Sample acvf
gammahat_0 <- sigma_squared/(1-phi^2)
gammahat_1 <- phi*gammahat_0

#mean corrected AR(1)
X <- mu + R
acf(X, type = c("covariance"), plot = FALSE, lag = 0)
acf(X, type = c("covariance"), plot = FALSE, lag = 1)
