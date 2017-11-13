library(kknn)

# Calculate knearest
knearest = function(data, k, newdata) {
  
  n1=dim(data)[1] #number of observations in training set
  n2=dim(newdata)[1] #number of observations in test set
  p=dim(data)[2] #number of variables observed in both sets (including whether it is spam or not)
  Prob=numeric(n2) #vector as long as test data, to be filled with probabilities for spam or not spam
  X=as.matrix(data[,-p]) # matrix of the training data excluding the spam/not spam
  Y=as.matrix(newdata[,-p]) # matrix is the test data excludign the spam/not spam
  
  # i
  X=X/matrix(sqrt(rowSums(X^2)), nrow=n1, ncol=p-1) #divide each Xi on row j by sqrt of the sum of the square of all Xij from current row j
  # ii
  Y=Y/matrix(sqrt(rowSums(Y^2)), nrow=n2, ncol=p-1) #same but for Y
  # iii
  C=X%*%t(Y)
  D=1-C
  print(D)
  for (i in 1:n2 ){
    nearest_neighbors_indexes = order(D[,i])[1:k] # indexes of the 5 nearest
    Prob[i] = sum(data[nearest_neighbors_indexes,p])/k # the probability of it being spam or not Ki/K
  }
  return(Prob)
}

# Returns a list with the vales for the ROC curve
ROC = function(Y, Yfit, p){
  m=length(p)
  TPR=numeric(m)
  FPR=numeric(m)
  for(i in 1:m){
    Ypred=ifelse(Yfit>p[i],1,0)
    t=table('pred'=Ypred, 'true'=Y)
    print(t)
    TPR[i]=t[1,1]/sum(t[,1])
    FPR[i]=t[1,2]/sum(t[,2])
  }
  return (list(TPR=TPR,FPR=FPR))
}

# Main program
dataframe = read.csv("spambase.csv", dec=',')
n = dim(dataframe)[1]
set.seed(12345)
id=sample(1:n, floor(n/2))
train = dataframe[id,]
test = dataframe[-id,]

# Prob
Prob_k5 = knearest(train, 5, test)
Prob_k1 = knearest(train, 1, test)
Prob_k5_kknn = kknn(Spam ~ .,train=train, test=test, k=5)

#confusion matrixes
cm_k5 = table(Prob_k5>0.5, test[,49])
cm_k1 = table(Prob_k1>0.5, test[,49])
cm_k5_kknn = table(Prob_k5_kknn$fitted.values>0.5, test[,49])

#missclassification rate
mcr_k5 = 1-sum(diag(cm_k5)/sum(cm_k5))
mcr_k1 = 1-sum(diag(cm_k1)/sum(cm_k1))
mcr_k5_kknn = 1-sum(diag(cm_k5_kknn)/sum(cm_k5_kknn))

# ROC and sensitivty
p_seq = seq(from=0.05, to=0.95, by=0.05)
#debugonce(ROC)
list_result = ROC(test[,49], Prob_k5, p_seq)
plot(x=list_result$FPR, y=list_result$TPR, xlab="FPR", ylab="TPR", xlim=c(0,1), ylim=c(0,1))

############## K = 5 with kknn ##############





