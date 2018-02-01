# exercise 7.1.3
rm(list=ls())
source('setup.R')
library(FNN)

# Load data
source("Scripts/ex4_1_1.R")

# K-nearest neighbors parameters
L = 40; # Maximum number of neighbors

res <- get.knn(X, k=L)
IDX <- res$nn.index

class_count <- array(rep(NA, times=N*C), dim=c(N,C))
Error <- array(rep(NA, times=N*L), dim=c(N,L))
for(ll in 1:L){ # For each number of neighbors
    
    # Count the number of observations in the neighbourhood belonging to each class
    for(c in 0:C-1){
        class_count[,c+1]=rowSums(cbind(rep(0, times=N), (matrix(as.vector(y)[IDX[,1:ll]], ncol=ll)==c)));
      }
    # Assign observations to the class with most observations
    y_est=max_idx(class_count)-1;
    Error[,ll] = sum(y!=y_est); 
  }

## Plot the classification error rate
plot(colSums(Error)/N*100, main='Error rate', xlab='Number of neighbors', ylab='Classification error rate (%)', type='l');
