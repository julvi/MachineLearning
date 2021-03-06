# exercise 6.1.2
rm(list=ls())
library(rpart)
library(cvTools)
source("setup.R")
# Load data
library(R.matlab)
dat <- readMat(file.path('Data','wine2.mat'))
X <- dat$X
N <- dat$N
attributeNames <- as.vector(unlist(dat$attributeNames))
M <- dat$M
y <- dat$y
C <- dat$C
classNames <- as.vector(unlist(dat$classNames))

# substitute spaces with dots to make handling of columns in data matrix easier
attributeNames <- gsub(' ', '.', attributeNames)

# Number of folds for crossvalidation
K = 100;

# Create k-fold crossvalidation partition
set.seed(1234) # for reproducibility
CV <- cvFolds(length(y), K=K)
# set up vectors that will store sizes of training and test sizes
CV$TrainSize <- c()
CV$TestSize <- c()

# Pruning levels
prune <- seq(from=0, to=0.05, length.out=10)

# Variable for classification error
Error_train = matrix(rep(NA, times=K*length(prune)), nrow=K)
Error_test = matrix(rep(NA, times=K*length(prune)), nrow=K)

for(k in 1:K){
    print(paste('Crossvalidation fold ', k, '/', K, sep=''))

    # Extract training and test set
    X_train <- X[CV$which!=k, ];
    y_train <- y[CV$which!=k];
    X_test <- X[CV$which==k, ];
    y_test <- y[CV$which==k];
    CV$TrainSize[k] <- length(y_train)
    CV$TestSize[k] <- length(y_test)

    Xdatframe_train <- data.frame(X_train)
colnames(Xdatframe_train) <- attributeNames
classassignments <- classNames[y_train+1]

# construct formula to fit automatically to avoid typing in each variable name
(fmla <- as.formula(paste("classassignments ~ ", paste(attributeNames, collapse= "+"))))

# fit classification tree
mytree <- rpart(fmla, data=Xdatframe_train,control=rpart.control(minsplit=100, minbucket=1, cp=0), parms=list(split='gini'), method="class")

Xdatframe_test <- data.frame(X_test)
colnames(Xdatframe_test) <- attributeNames

# Compute classification error
for(n in 1:length(prune)){ # For each pruning level
  mytree_pruned <- prune(mytree,prune[n])
predicted_classes_train<- classNames[predict(mytree_pruned, newdat=Xdatframe_train, type="vector")]
predicted_classes_test<- classNames[predict(mytree_pruned, newdat=Xdatframe_test, type="vector")]
    Error_train[k,n] = sum(classNames[y_train+1]!= predicted_classes_train)
    Error_test[k,n] = sum(classNames[y_test+1]!= predicted_classes_test)
}

  }


# Plot classification error
plot(c(min(prune), max(prune)), c(min(colSums(Error_train)/sum(CV$TrainSize), colSums(Error_test)/sum(CV$TestSize)), max(colSums(Error_train)/sum(CV$TrainSize), colSums(Error_test)/sum(CV$TestSize))), main='Wine decision tree: Holdout crossvalidation', xlab = 'Pruning level', ylab='Classification error', type="n")
points(prune, colSums(Error_train)/sum(CV$TrainSize), col="blue")
points(prune, colSums(Error_test)/sum(CV$TestSize), col="red");
legend('topleft', legend=c('Training error', 'Test error'), fill=c("blue", "red"));
