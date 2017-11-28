library(tree)
library(e1071)
library(MASS)

# Task 1
dataframe = read.csv("creditscoring.csv", dec=',')
n = dim(dataframe)[1]
set.seed(12345)
ids = sample(1:n, n) # sample random order of data

# Split the data into training/validation/training (50/25/25)
training = dataframe[ids[1:floor(n/2)],] # 50% of the data
validation = dataframe[ids[(floor(n/2)+1):floor(3*n/4)],] # 25% of data
test = dataframe[ids[(floor(3*n/4)+1):n],] # 25% of the data

# Task 2
fit_dt_dev = tree(good_bad~., data=training, split="deviance")
fit_dt_gini = tree(good_bad~., data=training, split="gini")

# plot(fit_dt_dev)
# text(fit_dt_dev, pretty=0)
# plot(fit_dt_gini)
# text(fit_dt_gini, pretty=0)

# Fit Test Data
fitted_test_dev = predict(fit_dt_dev, test, type="class")
fitted_test_gini = predict(fit_dt_gini, test, type="class")

# Confusion matrixes
cm_dev_test= table(fitted_test_dev, test[,20])
cm_gini_test = table(fitted_test_gini, test[,20])

# Missclassification rates
mcr_dev_test = 1-sum(diag(cm_dev_test))/sum(cm_dev_test)
mcr_gini_test = 1-sum(diag(cm_gini_test))/sum(cm_gini_test)
#mcr_dev_training = 0.2105 = 104 / 494
#mcr_gini_training = 0.2368 = 117 / 494
# mcr_dev < mcr_gini --> Will use Deviance as the measure of impurity

# Task 3
# Empty vectors of scores
train_score = rep(0,14)
validation_score = rep(0,14)
# Compute the deviance for the pruned tree's prediction on train set and on validation set, save in score vectors
for(i in 2:14){
  pruned_tree = prune.tree(fit_dt_dev, best=i)
  prediction = predict(pruned_tree, newdata=validation, type="tree")
  train_score[i] = deviance(pruned_tree)
  validation_score[i] = deviance(prediction)
}
# Plot the deviances for each score
plot(2:14, train_score[2:14], type="b", col="green", xlab="# of leaves", ylab="deviance", ylim=c(250, 600))
points(2:14, validation_score[2:14], type="b", col="blue")
legend("topright", legend=c("(green) Train Score", "(blue) Validation Score"), col=c("green","blue"))

# Optimal leaves = 4, depth=4, variables=savings, duration, history
pruned_tree = prune.tree(fit_dt_dev, best=4)
plot(pruned_tree)
text(pruned_tree, pretty=0)
# Predictions for pruned tree on TEST data
fitted_pruned_tree_test = predict(pruned_tree, newdata=test, type="class")
# Confusion matrix for pruned tree on the TEST data
cm_dev_test_prun = table(fitted_pruned_tree_test, test[,20])
# Missclassification rate for pruned tree on the TEST data
mcr_dev_test_prun = 1-sum(diag(cm_dev_test_prun)/sum(cm_dev_test_prun)) #0.248

# Task 4 - Naive Bayes
fit_naive_bayes =naiveBayes(good_bad~., data=training)
fitted_training_naive_bayes = predict(fit_naive_bayes, newdata=training, type="class")
fitted_test_naive_bayes = predict(fit_naive_bayes, newdata=test, type="class")

# Confusion matrixes
cm_naive_bayes_training = table(fitted_training_naive_bayes, training[,20])
cm_naive_bayes_test = table(fitted_test_naive_bayes, test[,20])

# Missclassification rates
mcr_naive_bayes_training = 1-sum(diag(cm_naive_bayes_training))/sum(cm_naive_bayes_training) # 0.3
mcr_naive_bayes_test = 1-sum(diag(cm_naive_bayes_test))/sum(cm_naive_bayes_test) # 0.3

# Task 5 - Loss Matrix
fitted_training_naive_bayes_prob = predict(fit_naive_bayes, newdata=training, type="raw")
fitted_test_naive_bayes_prob = predict(fit_naive_bayes, newdata=test, type="raw")

# Confusion matrixes
cm_naive_bayes_training_loss_matrix = table(fitted_training_naive_bayes_prob[,1]>10/11, training[,20])
cm_naive_bayes_test_loss_matrix = table(fitted_test_naive_bayes_prob[,1]>10/11, test[,20])

