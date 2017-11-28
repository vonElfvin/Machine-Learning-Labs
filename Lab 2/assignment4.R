# Attachment 3
library(fastICA)
library(pls)

# Task 1
dataframe = read.csv2("NIRSpectra.csv")
data = dataframe
data$Viscosity = c()
pca = prcomp(data)
# Eigenvalues
λ = pca$sdev^2

# Variation proportions for the different eigenvalues
var_props = λ/sum(λ)*100
sprintf("%2.3f", var_props)
barplot(var_props[1:10], ylim=c(0,100), col="forestgreen", 
        main="Variation proportions for different eigenvalues", 
        xlab="λi", ylab="Varaible proportion") # The plot shows that 2 PCs should be extracted
sum(var_props[1:2]) # PC1 and PC2 count for 99.5957% of the variation

# Scores - There are at least 2 unusual diesiel fuels according to this plot
plot(pca$x[,1], pca$x[,2]) # 2 "strong" outliers, 5-7 "medium" outliers

# Task 2
U = res$rotation

# Tracing plots
plot(U[,1], main="Traceplot for PC1", ylim=c(-0.11,0.11))
plot(U[,2], main="Traceplot for PC2", ylim=c(-0.3, 0.3)) # the last few original feutures mainly explain this PC

# Task 3 - ICA
ica = fastICA(data, 2, alg.typ="parallel", fun="logcosh", alpha=1, method="R", row.norm=FALSE, maxit=200, tol=0.0001, verbose=TRUE)
Wtick = ica$K%*%ica$W
plot(Wtick[,1], main="Traceplot, W' column 1")
plot(Wtick[,2], main="Traceplot, W' column 2")

# Scores
plot(ica$S[,1], ica$S[,2]) # ICA, 

# Task 4
set.seed(12345)
pcr = pcr(Viscosity~., data=dataframe, validation="CV")
summary(pcr)
validationplot(pcr, val.type="MSEP", main="Dependence of MSEP and #components")
