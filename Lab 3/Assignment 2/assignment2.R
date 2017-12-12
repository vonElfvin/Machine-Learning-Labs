# Appendix 2

###############
# Assignment 2#
###############

# Libraries
library(neuralnet)
set.seed(12345)

# Setup
rads = runif(50, 0, 10)
data = data.frame(rads, sin=sin(rads))
n = dim(data)[1]
ids = sample(1:n,n/2)
training = data[ids,]
validation = data[-ids,]

# Functions

mse = function(obs, pred){
  return(mean((obs-pred)^2))
}

# Implementation
set.seed(12345)
weights.start = runif(50, -1, 1)
mse.training = numeric(10)
mse.validation = numeric(10)

# Compute MSEs for each data set with different threshholds
for(i in 1:10){
  nn = neuralnet(sin~rads, training, hidden=10, threshold=i/1000, startweights=weights.start)
  pred = compute(nn, validation$rads)
  mse.training[i] = mse(training$sin, nn$result.matrix[1])
  mse.validation[i] = mse(validation$sin, pred$net.result[1])
}
plot(mse.training, type="b", col="green", ylim=c(0.4,0.9))
points(mse.validation, type="b", col="red")


# Neural Network with best threshhold = 1
nn = neuralnet(sin~rads, training, hidden=10, threshold=1/1000, startweights=weights.start)
plot(nn)

# Prediction
prediction = prediction(nn)
plot(prediction(nn)$rep1, col="blue")
points(data, col="red")
