library(MASS)
library(glmnet)

# Task 1
dataframe = read.csv("tecator.csv", dec=',')
dataframe = dataframe[-215,]
protein = dataframe$Protein
moisture = dataframe$Moisture
fat = dataframe$Fat
channels = as.matrix(dataframe[1:100])

plot(protein, moisture)

# Task 2
# M1 = B0+B1*p+ε, where ε = N~(0, σ)
# M2 = B0+B1*p+B2*p²+ε, where ε=N~(0, σ)
# M3 = B0+B1*p+B2*p²+B3*p³+ε, where ε=N~(0, σ)
# etc.
# MSE criterion is appropriate to use since it minimizes the errors in our predicions (deviation of ε)

# Task 3
n = dim(dataframe)[1]
ids = sample(1:n, floor(n/2))

training = dataframe[ids,]
validation = dataframe[-ids,]

p = training$Protein
p2 = p^2
p3 = p^3
p4 = p^4
p5 = p^5
p6 = p^6
X1 = cbind(p)
X2 = cbind(p, p2)
X3 = cbind(p, p2, p3)
X4 = cbind(p, p2, p3, p4)
X5 = cbind(p, p2, p3, p4, p5)
X6 = cbind(p, p2, p3, p4, p5, p6)
#B = solve(t(X)%*%X)%*%t(X)%*%Y if I was to calculate it myself, did not work for higher polynomial terms of power
M1 = lm(Moisture ~ X1, data=training)
M2 = lm(Moisture ~ X2, data=training)
M3 = lm(Moisture ~ X3, data=training)
M4 = lm(Moisture ~ X4, data=training)
M5 = lm(Moisture ~ X5, data=training)
M6 = lm(Moisture ~ X6, data=training)

# Make the predictions

fitted_validation1 = predict(M1, validation)
fitted_validation2 = predict(M2, validation)
fitted_validation3 = predict(M3, validation)
fitted_validation4 = predict(M4, validation)
fitted_validation5 = predict(M5, validation)
fitted_validation6 = predict(M6, validation)
fitted_training1 = predict(M1)
fitted_training2 = predict(M2)
fitted_training3 = predict(M3)
fitted_training4 = predict(M4)
fitted_training5 = predict(M5)
fitted_training6 = predict(M6)

# Calculate the MSEs
# Validation MSEs
mse_v1 = mean((validation$Moisture-fitted_validation1)^2)
mse_v2 = mean((validation$Moisture-fitted_validation2)^2)
mse_v3 = mean((validation$Moisture-fitted_validation3)^2)
mse_v4 = mean((validation$Moisture-fitted_validation4)^2)
mse_v5 = mean((validation$Moisture-fitted_validation5)^2)
mse_v6 = mean((validation$Moisture-fitted_validation6)^2)

# Training MSEs
mse_t1 = mean((training$Moisture-fitted_training1)^2)
mse_t2 = mean((training$Moisture-fitted_training2)^2)
mse_t3 = mean((training$Moisture-fitted_training3)^2)
mse_t4 = mean((training$Moisture-fitted_training4)^2)
mse_t5 = mean((training$Moisture-fitted_training5)^2)
mse_t6 = mean((training$Moisture-fitted_training6)^2)

# Plot
mse_v = c(mse_v1, mse_v2, mse_v3, mse_v4, mse_v5, mse_v6)
mse_t = c(mse_t1, mse_t2, mse_t3, mse_t4, mse_t5, mse_t6)

plot(1:6, mse_v, type="l", col="green", ylim=c(0,180))
lines(1:6, mse_t, type="l", col="blue")

idx = !colnames(training) %in% c("Sample", "Fat", "Protein", "Moisture")
formulax = paste("Fat~", paste(colnames(training[,idx]), collapse = "+"))

#Task 4
fit = lm(formula(formulax), data=dataframe)
step = stepAIC(fit, direction="both", trace=FALSE)
step$anova
summary(step)

# Task 5
scaled_channels = scale(dataframe[,2:101])
scaled_fat = scale(dataframe[,102])
model0_ridge = glmnet(as.matrix(scaled_channels), scaled_fat, alpha=0, family="gaussian")
plot(model0_ridge, xvar="lambda")

# Task 6
model0_lasso = glmnet(as.matrix(scaled_channels), scaled_fat, alpha=1, family="gaussian")
plot(model0_lasso, xvar="lambda")

# Task 7
model0_lasso_cv = cv.glmnet(as.matrix(scaled_channels), scaled_fat, alpha=1, family="gaussian", lambda=seq(0,1,0.001))
plot(model0_lasso_cv, xvar="lambda", label=TRUE)
model0_lasso_cv$lambda.min





