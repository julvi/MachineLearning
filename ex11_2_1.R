# exercise 11.2.1
rm(list=ls())
source("setup.R")
library(sm) # install.packages("sm")
source(file.path('Scripts', 'ex11_1_1.R'))
graphics.off()

# add outlier
X=rbind(X,-10)
# Kernel width
w = 5;

# Outlier scores
# Compute kernel density estimate
f <- sm.density(X, h=w, model = "Normal", display='none', eval.points=X)
#f <- bkde(X, bandwidth=w)
#f = ksdensity(X, X, 'width', w);
#f <- density(X, bw=w)

# Sort the densities
res <- sort(f$estimate, index.return=TRUE)
y <- res$x
i <- res$ix

# Display the index of the lowest density data object
# The outlier should have index 1000 (the length of the vector i, i.e. be the last observation in the sorted vector of densities)
print(i[1]);

# Plot density estimate outlier scores
barplot(y[1:20], main='Outlier score')
