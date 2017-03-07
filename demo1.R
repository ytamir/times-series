X <- as.matrix( read.table("SUNSPOTS.TSM", header=FALSE) )
plot( X )

#Fitting a linear model to the data by Least Squares
model <- lm(formula = X ~ seq(1:100) + seq(1:100)^2 )

R <- model$residuals
acf(R)

#Fitting AR model to the data by Ordinary Least Squares
noise <- ar(x = R, aic = FALSE, order.max = 1, method = "ols", intercept = FALSE)

phi <- as.double(noise[2])

acf(R[2:100] - phi*R[1:99])
