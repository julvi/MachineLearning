# exercise 11.2.4
rm(list=ls())
source("setup.R")
library(FNN) # install.packages("FNN")
source(file.path('Scripts', 'ex11_1_1.R'))
graphics.off()

# Neighbors to use
K = 5;

# Find the k nearest neighbors
res <- get.knnx(data=X, query=X, k=K+1)
i <- res$nn.index
D <- res$nn.dist

# Outlier score
f = D[,K+1];

# Sort the outlier scores
sortres <- sort(f, index.return=TRUE, decreasing=TRUE)
y <- sortres$x
i <- sortres$ix

# Display the index of the lowest density data object
# The outlier should have index 1001
print(i[1]);

# Plot kernel density estimate outlier scores
barplot(y[1:20], main='Distance: Outlier score');


