# returns the loglikelihood value for given θ and vector X
loglikelihood = function(x, θ){
  return(length(x)*log(θ)-θ*sum(x))
}

# returns the max logilikelihood value for the given distribution estimation
max_loglikelihood = function(x){
  return(length(x)/sum(x))
}

# returns log value of the bayesian proportional probability for given θ, λ and vector X
log_bayesian = function(x, θ, λ){
  n = length(x)
  #return(n*log(θ)-θ*sum(x)+n*log(λ)-n*λ*θ)
  return(n*log(θ)-θ*sum(x)+log(λ)-λ*θ)
}

max_bayesian = function(x, λ){
  n = length(x)
  #return (n/(sum(x)+n*λ))
  return (n/(sum(x)+λ))
}
###########################################################################

# Task 1
# Data preparation
dataframe = read.csv("machines.csv", dec=',')
X = dataframe[1]$Length # X vector
X_6 = X[1:6] # X vector of first 6 values
θ = seq(from=0, to=20, by=0.025) # θs to test
m = length(θ) # Amount of θs to be tested
loglikelihood_n = numeric(m) # Empty vector for loglikelihood values with all x values
loglikelihood_6 = numeric(m) # Empty vector for loglikelihood values the first 6 values

# Task 2
# Calculate the loglikelihoods for different θ values with the given vectors
for(i in 1:m){
  loglikelihood_n[i] = loglikelihood(X, θ[i])
  loglikelihood_6[i] = loglikelihood(X_6, θ[i])
}

# Task 3
# Plot the loglikelihoods
plot(θ, loglikelihood_6, type="l", main="Dependence of Log-Likelihood and θ (n values)", xlab="θ", ylab="Log-Likelihood", col="green")
lines(θ, loglikelihood_n, col="blue")

# θ values for maximum loglikelihoods
θstar_n = max_loglikelihood(X)
θstar_6 = max_loglikelihood(X_6)

# Task 4
# Plot the Bayesian Model
λ = 10;
log_bayesian_model = numeric(m)

for(i in 1:m){
  log_bayesian_model[i] = log_bayesian(X, θ[i], λ)
}
plot(θ, log_bayesian_model, type="l", xlim=c(0,20), ylim=c(-500, 0), main="Bayesian model", xlab="θ", ylab="Log-Bayesian", col="green")
θstar_b = max_bayesian(X, λ)

# Task 5
# Compare new generated observations to original ones
set.seed(12345)
new_observations = rexp(n=50, rate = θstar_n)
#data.frame(X, new_observations)
hist(X, col="green", xlim=c(0,6), ylim=c(0,30), main="Original observations", xlab="x")
x11()
hist(new_observations, col="blue", breaks=12, xlim=c(0,6), ylim=c(0,30), main="New observations", xlab="x")












