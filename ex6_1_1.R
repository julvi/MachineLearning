# exercise 6.1.1
rm(list=ls())
source("setup.R")
library(rpart)
install.packages('cvTools')
library(cvTools)
# Load data
library(R.matlab)
dat <- readMat(file.path('Data', 'wine2.mat'))
X <- dat$X
N <- dat$N
attributeNames <- as.vector(unlist(dat$attributeNames))
M <- dat$M
y <- dat$y
C <- dat$C
classNames <- as.vector(unlist(dat$classNames))

# substitute spaces with dots to make handling of columns in data matrix easier
attributeNames <- gsub(' ', '.', attributeNames)

# Create holdout crossvalidation partition
set.seed(1) # if you want reproducible results
CV <- cvFolds(length(y), K=2)
CV$TrainSize <- sum(CV$which!=1) # add sizes of training and test data to the list CV returned by cvFolds
CV$TestSize <- sum(CV$which!=2)

# Pruning levels
#prune = seq(from=0, to=0.05, length.out=10);
prune <- seq(from=0, to=1, length.out=10);

# Variable for classification error count
Error_train <- rep(NA, times=length(prune));
Error_test <- rep(NA, times=length(prune));

# Extract training and test set
X_train <- X[CV$which!=1, ];
y_train <- y[CV$which!=1];
X_test <- X[CV$which!=2, ];
y_test <- y[CV$which!=2];

# Fit classification tree to training set
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
    Error_train[n] = sum(classNames[y_train+1]!= predicted_classes_train)
    Error_test[n] = sum(classNames[y_test+1]!= predicted_classes_test)
}

# Plot classification error
plot(c(min(prune), max(prune)), c(min(Error_train/CV$TrainSize, Error_test/CV$TestSize), max(Error_train/CV$TrainSize, Error_test/CV$TestSize)), main='Wine decision tree: Holdout crossvalidation', xlab = 'Pruning level', ylab='Classification error', type="n")
points(prune, Error_train/CV$TrainSize, col="blue")
points(prune, Error_test/CV$TestSize, col="red");
legend('topleft', legend=c('Training error', 'Test error'), fill=c("blue", "red"));
