# Attachmed 2
library(tree)
library(boot)
dataframe = read.csv2("State.csv")

# Settings
data.sorted = dataframe[order(dataframe$MET),] # sorted data
n = dim(data.sorted)[1] # number of observations
tree.control = tree.control(n, minsize=8) # control

# Functions
get_tree = function(){
  return(tree(EX~MET, data=data.sorted, control=tree.control))
}

get_pruned_tree = function(tree, leaves){
  return(prune.tree(tree, best=leaves))
}

calculate_residuals = function(predictions, observations){
  return(observations-predictions)
}

get_residuals = function(mle){
  fitted0 = predict(mle, data.sorted)
  return(calculate_residuals(fitted0, data.sorted$EX))
}

fit_tree = function(in.data){
  return(tree(EX~MET, data=in.data, control=tree.control))
}

f1 = function(in.data, ind){
  data1 = in.data[ind,]
  tree1 = fit_tree(data1)
  pruned.tree1 = get_pruned_tree(tree1, 3)
  fitted1 = predict(pruned.tree1, newdata=data.sorted)
  return(fitted1)
}

f2 = function(in.data){
  tree2 = fit_tree(in.data)
  pruned.tree2 = get_pruned_tree(tree2, 3)
  fitted2 = predict(pruned.tree2, newdata=data.sorted)
  return(fitted)
}

rng = function(in.data, mle){
  data = data.frame(EX=in.data$EX, MET=in.data$MET)
  n=dim(data)[1]
  data$EX = rnorm(n, predict(mle, newdata=data), sd(get_residuals(mle)))
  return(data)
}

# Task 1
plot(data.sorted$MET, data.sorted$EX, ylim=c(0,450), xlim=c(0,100)) # Some sort of regressional model, 2nd polynomial

# Task 2

# Fit the model
fit.tree = get_tree()
set.seed(12354)
cv.fit.tree = cv.tree(fit.tree, FUN=prune.tree)
# Plot deviations
plot(cv.fit.tree$size, cv.fit.tree$dev, type="b", col="forestgreen") # 3 leaves seems to be optimal
plot(log(cv.fit.tree$k), cv.fit.tree$dev, type="b", col="forestgreen") # 4 leaves seems to be optimal

# Generate final tree, chosen 3 leaves
pruned.tree = get_pruned_tree(fit.tree, 3)

# Make predictions
pruned.fitted = predict(pruned.tree, newdata=data.sorted)

# Plot predictions and actual values
plot(data.sorted$MET, pruned.fitted, col="blue")
points(data.sorted$MET, data.sorted$EX, col="red")

# Plot the residuals
residuals = calculate_residuals(pruned.fitted, data.sorted$EX)
hist(residuals, main="Residuals", xlab="Residual value", col="forestgreen", xlim=c(-125, 125))

# Task 3 - TODO missing what is asked for
set.seed(12345)
w1 = boot(data.sorted, f1, R=50)
hist(w1$t)

# Task 4 - TODO missing what is asked for, fix boot
set.seed(12345)
mle = pruned.tree
w2 = boot(data.sorted, statistic=f2, R=50, mle=mle, ran.gen=rng, sim="parametric")
hist(w2$t)















