####################################################################################
#K-means and hierarchical clustering
####################################################################################
#Use script 9_3_2


#Clear workspace ect:
rm(list=ls())
graphics.off() # close all open graphics windows


#----------------------
#Load the data into R
#----------------------
#Set working directory
setwd("~/Google Drive/PhD/Courses/Introducion to Machine Learning and Data Mining /dataset")
#setwd('/Users/lenesommer/DTU/Kandidat/F2014/IntroTilMachineLearning /Project2')

source("~/Google Drive/PhD/Courses/Introducion to Machine Learning and Data Mining /ToolBox/02450Toolbox_R/Tools/categoric2numeric.R")
source("~/Google Drive/PhD/Courses/Introducion to Machine Learning and Data Mining /ToolBox/02450Toolbox_R/Tools/clusterval.R")
source("categoric2numeric.R")
source("clusterplot.R")
setwd("~/Desktop/IntroductionToMachineLearning")
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
#idxOutlier = X[,6]>4
#dat[77,]
# Finally we will remove these from the data set
#X = X[-which(idxOutlier),]


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

#Analyze your data by hierarchical clustering and try interpret the generated dendrogram.
#Use the cluster validity measures to evaluate how well the clusters reflect your 
#labeled information at one of the levels of the dendrogram.

## Hierarchical clustering
# Maximum number of clusters
Maxclust = 8;

# Compute hierarchical clustering
#hc <- hclust(dist(X), method="single")
hc <- hclust(dist(X), method="complete")
#hc <- hclust(dist(X), method="average")
#hc <- hclust(dist(Xdf), method="ward")
# Compute clustering by thresholding the dendrogram
i <- cutree(hc, k = Maxclust)
i <- as.matrix(i)
## Plot results

# Plot dendrogram
#plclust(hc)
plot(hc)

# Plot data
dev.new()

clusterplot(X, classlabels, i, main='Hierarchical')
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

