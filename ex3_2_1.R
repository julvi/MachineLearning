# image to use as query
i <- 1

# Similarity: 'SMC', 'Jaccard', 'ExtendedJaccard', 'Cosine', 'Correlation' 
SimilarityMeasure = 'Cosine';

library(R.matlab)
dat <- readMat(file.path("Data", "wildfaces_grayscale.mat"))
names(dat)
X = dat$X;
dimX = dim(X);
N <- dimX[1]; M <- dimX[2]
Xdat <- X
X <- matrix(Xdat[i,], ncol=dim(Xdat)[2])
noti <- 1:dim(Xdat)[1]
noti <- noti[-i]
Y <- Xdat[noti,]

source('setup.R')
install.packages("fdakma")
library(fdakma)
library(sos)
findFn("similarity", maxPages=2, sortby="MaxScore")
#           'SMC', 'smc'             : Simple Matching Coefficient
#           'Jaccard', 'jac'         : Jaccard coefficient 
#           'ExtendedJaccard', 'ext' : The Extended Jaccard coefficient
#           'Cosine', 'cos'          : Cosine Similarity
#           'Correlation', 'cor'     : Correlation coefficient
sim <- similarity(X,Y, "jac")
sim <- similarity(X,Y, "cos")

# Sort similarities
sort_result = sort(sim, decreasing=TRUE, index.return=TRUE);
val <- sort_result$x
j <- sort_result$ix
nj = length(j)
# plot five most similar and dissimilar images
npics=5
ndigits = 4
mostsim = j[1:npics]
leastsim = j[(nj-npics+1):nj]

queryImage <- Xdat[i,]
dim(queryImage) <- c(40,40)

dev.new(width=2, height=2)
image(t(queryImage[nrow(queryImage):1,]), main="Query image", col=gray((0:32)/32))

dev.new(width=2*npics, height=5)
layout(matrix(c(1:length(mostsim), (length(mostsim)+1):(2*length(mostsim))), 2, length(mostsim), byrow = FALSE))
for(d in 1:npics){
  similarImage <- Xdat[noti[mostsim[d]],]
  dim(similarImage) <- c(40,40)
  image(t(similarImage[nrow(similarImage):1,]), main=paste("Similarity:", format(val[d], digits=ndigits)), col=gray((0:32)/32))

  
  dissimilarImage <- Xdat[noti[leastsim[d]],]
  dim(dissimilarImage) <- c(40,40)
  
  image(t(dissimilarImage[nrow(dissimilarImage):1,]), main=paste("Similarity:", format(val[nj-d+1], digits=ndigits)), col=gray((0:32)/32))
}

