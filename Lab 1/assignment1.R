# Calculate knearest
knearest = function(data, k, newdata) {
  
  n1=dim(data)[1] #number of observations in training set
  n2=dim(newdata)[1] #number of observations in test set
  p=dim(data)[2] #number of variables observed in both sets (including whether it is spam or not)
  Prob=numeric(n2) #vector as long as test data, to be filled with probabilities for spam or not spam
  X=as.matrix(data[,-p])
  Y=as.matrix(newdata[,-p])
  
  # i
  X=X/matrix(sqrt(rowSums(X^2)), nrow=n1, ncol=p-1) #divide each Xi on row j by sqrt of the sum of the square of all Xij from current row j
  # ii
  Y=Y/matrix(sqrt(rowSums(Y^2)), nrow=n2, ncol=p-1) #same but for Y
  # iii
  print(dim(X))
  print(dim(Y))
  print(dim(t(Y)))
  C=X%*%t(Y)
  M = matrix(1, nrow=dim(C)[1], ncol=dim(C)[2])
  D=M-C
  print(dim(D))
  
  for (i in 1:n2 ){
  #MISSING: use the computed distance matrix to find 
    #which observations are the nearest neighbors to case #i
  #MISSING: derive probability value 'Prob[i]' by using the
    #target values of the nearest neighbors
    #Prob[i] = 
    nearest_neighbors_indexes = which(D[i,] %in% sort(D[i,])[1:k])
    for(j in 1:k){
      index = nearest_neighbors_indexes[j]
      Prob[i] = Prob[i] + data[index,p]/k
    }
  }
  return(Prob)
}

# Main program
dataframe = read.csv("spambase.csv", dec=',')
n = dim(dataframe)[1]
set.seed(12345)
id=sample(1:n, floor(n/2))
train = dataframe[id,]
test = dataframe[-id,]
Y = test[,49]

# Prob
Prob_k5 = knearest(train, 5, test)
Prob_k1 = knearest(train, 1, test)

#confusion matrixes
cm_k5 = table(Prob_k5>0.5, Y)
cm_k1 = table(Pred_k1>0.5, Y)

#missclassification rate
mcr_k5 = 1-sum(diag(cm_k5)/sum(cm_k5))
mcr_k1 = 1-sum(diag(cm_k1)/sum(cm_k1))

# ROC and sensitivty
Yfit = Prob_k5
p = matrix(seq(from=0.01, to=0.99, by=0.01), nrow=1, ncol=99)
list = ROC(Y, Yfit, p)
print(as.vector(list)$TPR)
plot(as.vector(list)$FPR, as.vector(list)$TPR, xlim=c(0,1), ylim=c(0,1))


# Print the ROC curve
ROC = function(Y, Yfit, p){
  m=length(p)
  TPR=numeric(m)
  FPR=numeric(m)
  print(Y)
  for(i in 1:m){
    t=table(Yfit>p[i], Y)
    print(t)
    TPR[i]=t[1,1]/(t[1,1]+t[1,2])
    FPR[i]=t[2,2]/(t[2,1]+t[2,2])
  }
  return (list(TPR=TPR,FPR=FPR))
}

