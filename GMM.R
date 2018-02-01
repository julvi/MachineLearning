####################################################################################
#K-means and hierarchical clustering
####################################################################################
#Use script 10_1_1


#Clear workspace ect:
rm(list=ls())
graphics.off() # close all open graphics windows


#----------------------
#Load the data into R
#----------------------
#Set working directory
setwd("~/Google Drive/PhD/Courses/Introducion to Machine Learning and Data Mining /dataset")
#setwd('/Users/lenesommer/DTU/Kandidat/F2014/IntroTilMachineLearning /Project2')
#source("~/Google Drive/PhD/Courses/Introducion to Machine Learning and Data Mining /ToolBox/02450Toolbox_R/Tools/setup.R")
library(mixtools) # install.packages("mixtools") # package that can be used to fit a gaussian mixture model. This package does allow random starts of the algorithm.
library(mclust)
source("~/Google Drive/PhD/Courses/Introducion to Machine Learning and Data Mining /ToolBox/02450Toolbox_R/Tools/categoric2numeric.R")
#source("~/Google Drive/PhD/Courses/Introducion to Machine Learning and Data Mining /ToolBox/02450Toolbox_R/Tools/clusterval.R")
#source("categoric2numeric.R")
source("~/Google Drive/PhD/Courses/Introducion to Machine Learning and Data Mining /ToolBox/02450Toolbox_R/Tools/clusterval.R")
source("clusterplot.R")

#Read data in
dat <- read.table("phageDS.complete25FEB.txt", header = T, as.is = T)
dim(dat)

#remove phage ID, categorical attributes and attributes related to the host
X <- dat[,-c(1,4,5,6,7,8)] 

#--------------------
#One out-of-k-coding
#--------------------
#First column 4
col4 = categoric2numeric(dat[,4])
X[11:19]<-col4[[1]]
familynames <- col4[[2]]
colnames(X)[11:19] <- familynames

#Then column 8
col7 = categoric2numeric(dat[,8])
X[20:28]<-col7[[1]]
predictedHost <- col7[[2]]
colnames(X)[20:28] <- predictedHost

#Remove outlier
X=X[-77,]
classlabels <- dat[-77,5] 
X <- data.frame(X)

y <- as.numeric(as.factor(classlabels))
y <- y-1

#standardize
X <- scale(X)
dattmp<-X[,11:28]*(1/sqrt(9))
X[,11:28] <- dattmp
attributeNamesC <- colnames(X)

#Define size
N <- 125
M <- 28
C <-8


# Number of clusters
K = 8; #minimum value is 2

# Fit model
model <- Mclust(data=X, G=K)
#model <- Mclust(data=Xdf, G=K) # using the mclust package
# model <- mvnormalmixEM(x=Xdf, k=K, maxit=100, epsilon=1e-2, verb=TRUE) # using the mixtools package. Defaults for maxit and epsilon are 500 and 1e-8, respectively. Avoid extreme running times by allowing fewer iterations and deeming convergence earlier by setting maxit and epsilon as done here. The argument verb=TRUE makes the method write output from the EM algorithm at each iteration. The argument verb is FALSE by default.

model$loglik
# Get clustering
i = model$classification # using the mclust package
#i = max_idx(model$posterior) # using the mixtools package

# Get cluster centers
Xc = t(model$parameters$mean) # using the mclust package
#Xc = matrix(unlist(model$mu), nrow=length(model$mu), byrow=TRUE) # using the mixtools package

## Plot results
# Plot clustering
#clusterplot(Xdf, y, i, Xc, main='GMM: Clustering');
clusterplot(X, y, i, Xc, main='GMM: Clustering');

###---------------------------------------------------
#Compute cluster validitiy
###---------------------------------------------------
#Use ex9_1_3 as inspiration 

# Allocate variables
Entropy = c()
Purity = c()
Rand = c()
Jaccard = c()

# Compute cluster validities
res <- clusterval(y, i)

Entropy <- res$Entropy
Purity <- res$Purity
Rand <- res$Rand
Jaccard <- res$Jaccard

textX = c("Entropy", "Purity", "Rand", "Jaccard")
plotErrors=c(Entropy, Purity, Rand, Jaccard)
plot(plotErrors, type="h", lwd=10, col="blue", main="Different error measures", xaxt = "n", ylab="", xlab="Method")
axis(1, at=1:4, labels=textX)
#NOTE: It does not look like the clusters are very pure!

