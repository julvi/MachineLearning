##################################################################
# Project 1                                                      #
##################################################################
rm(list=ls())

#----------------------
#Load the data into R
#----------------------
#Set working directory

#setwd("~/GoogleDrive/PhD/Courses/IntroduciontoMachineLearningandDataMining/dataset")
setwd("~/GoogleDrive/PhD/Courses/IntroductiontoMachineLearningandDataMining/dataset")
#Read data in
dat <- read.table("phageDS.complete25FEB.txt", header = T, as.is = T)
dim(dat)

#Extract class labels of observations
classlabels <- dat[1:126,5]

#Remove the fith column because it is with the class labels
datnew <- dat[,-5]
#Maybe remove the other columns not needed

#Extract attributes, i.e. names of attributes
attributes <- colnames(datnew)

#Extract the class names present in data
classNames <- unique(classlabels)

#Extract numeric class assignments
y <- as.numeric(as.factor(classlabels))
y <- y-1

##------------------------------------------------------------
#Plot

#Choose which attributes to plot
attX <- datnew[,2] #phage_genome_size
attY <- datnew[,3] #phage_GCcontent

#Make a simple plot

plot(attX, attY)

#Make it a more fancy plot:
plot(c(min(attX), max(attX)), c(min(attY), max(attY)), xlab="Phage genome size", ylab="phage GCcontent", main="Phage data", bty="L", type="n")

#Plot points in separate colors
cols <- colors()
for(i in sort(unique(y))){
  points(attX[y==i], attY[y==i], col=cols[(i+1)*10])
}

#Now a plot where there is room for the legend
plot(c(min(attX), max(attX)+150000), c(min(attY), max(attY)), xlab="Phage genome size", ylab="phage GCcontent", main="Phage data", bty="L", type="n")

#Plot points in separate colors
cols <- colors()
for(i in sort(unique(y))){
  points(attX[y==i], attY[y==i], col=cols[(i+1)*10])
}
#Turn the class names into independent charaters
classNew<-as.character(classNames)

#Get the order that classes were gone through and plotted in the for loop and add legend
sorted <- sort(classNew, index.return=TRUE)
legend(200500, 70, legend=classNames[sorted$ix], fill = cols[10*(1:8)],cex=0.7)

##-------------------------------------------------------------------------------------------

#--------------------------------------------------------------------------------------------
#Order the data
#--------------------------------------------------------------------------------------------

#First extract so that there is only numeric values
#datPC <- datnew[,c(2,3,8,9,10,11,12,13,14,15)] 
datPC <- datnew[,c(2,3,5,6,8,9,10,11,12,13,14,15)] 

#Make the 2 categorical attributes into something that we can use
source('categoric2numeric.R')

#First column 4
col4 = categoric2numeric(datnew[,4])
#datPC[11:19]<-col4[[1]]
datPC[13:21]<-col4[[1]]
familynames <- col4[[2]]
#colnames(datPC)[11:19] <- familynames
colnames(datPC)[13:21] <- familynames

#Then column 7
col7 = categoric2numeric(datnew[,7])
#datPC[20:28]<-col7[[1]]
datPC[22:30]<-col7[[1]]
familynames7 <- col7[[2]]
#colnames(datPC)[20:28] <- familynames7
colnames(datPC)[22:30] <- familynames7
#--------------------------------------------------------------------------------------------
#Summary statistics
#--------------------------------------------------------------------------------------------

datTMP <- datnew[,c(2,3,5,6,8,9,10,11,12,13,14,15)] 

#Calculate the mean, variance, median and range of the different variables.
#Before any scaling
mean_dat <- apply(datTMP, 2, mean)
var_dat <- apply(datTMP, 2, var)
median_dat <- apply(datTMP, 2, median)
range_dat <- diff(apply(datTMP, 2, range)) ## range returns the minimum and maximum of the vector
#After that the difference is taken between those.

#NOTE: comparing the mean and the median could give a hint whether there are any outliers.
#Looking at these values could also tell whether or not it is a good ide to standadize data.
#Since there is a big difference, it is good that we in the following scale.

#--------------------------------------------------------------------------------------------
#Standardize and lower the effect of the categorical variables
#--------------------------------------------------------------------------------------------
#we standardize our data
datStandard <- scale(datPC)
#check that the column mean is the close to 0
colMeans(datStandard)
#check that the sd is 1
sd(datStandard[,17])

#Now divide the columns for the categorical values with 1/sqrt(columns).
dattmp<-datStandard[,13:30]*(1/sqrt(9)) 
datStandard[,13:30] <- dattmp

#Make new attributes so they fit the new dataset
attributesSTD <- colnames(datStandard)

# Get the number of data objects, attributes, and classes
N = dim(datStandard)[1];
M = dim(datStandard)[2];
C = length(classNames);

#--------------------------------------------------------------------------------------------
#Remove outliers
#--------------------------------------------------------------------------------------------

#A boxplot of the attributes (standardized)
par(mar = c(8, 4.1, 2, 2.1))

par(mfrow=c(1,1))
#boxplot(datStandard, main="Boxplots of attribute values", las = 2)

boxplot(datStandard, xaxt = "n",  xlab = "")
#save the labels
labels <- colnames(datStandard)
labels[1] <- "Phage genome size"
labels[2] <- "Phage GC content"
labels[3] <- "Host genome size"
labels[4] <- "Host GC content"
labels[9] <- "Z score"
labels[11] <- "Unique 15mers in template"
labels[12] <- "Unique 15mers in query"
labels[15] <- "Unclassified phages"
labels[21] <- "Unclassified dsDNA phages"
labels[22] <- "Escherichia coli"
labels[24] <- "Lactococcus lactis"
labels[25] <- "Mycobacterium smegmatis"
labels[26] <- "Pseudomonas aeruginosa"
labels[27] <- "Staphylococcus aureus"
labels[29] <- "Prochlorococcus marinus"
labels[30] <- "Vibrio cholerae"

# x axis with ticks but without labels
axis(1, at = seq_along(labels), labels = FALSE)

# Plot x labs at default x position
text(x = seq_along(labels), y = par("usr")[3] - 0.5, srt = 45, adj = 1, labels = labels, xpd = TRUE, cex = 0.8)


#Here we can see that there are some outliers for attribute: 1,5,6,7,9,10

#Plot a histogram for those attributes to take a closer look
m = c(1,5,6,7,9,10 );
yvals <- c()
for(i in 1:6)
{
  res <- hist(datStandard[,m[i]],breaks=51, plot=FALSE);
  yvals <- c(yvals, res$counts)
}

par(mfrow=c(2,3))
for(i in 1:6)
{
  hist(datStandard[,m[i]],breaks=51, main=unlist(attributesSTD[m[i]]), ylim=c(min(yvals), max(yvals)))
}
#For sure number 6 has a outlier, it is hard to see the others. 
# It looks more like they are widely dirstributed.
#We remove the outlier from number 3 attribute

idxOutlier = datStandard[,8]>4

# Finally we will remove these from the data set
datMout = datStandard[-which(idxOutlier),]
yMout = y[-which(idxOutlier)]
N3 = N-sum(idxOutlier);
N-N3 #control how many observations are removed


#Now we look at the boxplot again
#par(mar = c(12, 4.1, 4.1, 2.1))
#par(mfrow=c(1,1))
boxplot(datMout, xaxt = "n",  xlab = "")
axis(1, at = seq_along(labels), labels = FALSE)
text(x = seq_along(labels), y = par("usr")[3] - 0.3, srt = 45, adj = 1, labels = labels, xpd = TRUE, cex = 0.8)
#Do not remove the categorical varible outliers - it is just an observation

#--------------------------------------------------------------------------------------------
#Check normallity
#--------------------------------------------------------------------------------------------

#A normal-QQ plot for the attributes:
par(mfrow=c(2,5))
for (i in 1:10){
  qqnorm(datMout[,i], main=unlist(attributesSTD[i]))
}
#A Normal QQ plot for each of the attributes
par(mfrow=c(1,1))
for (i in 1:10){
  qqnorm(datMout[,i], main=unlist(attributesSTD[i]))
}

#A histogram for the attributes
yvals <- c()
for(m in 1:10)
{
  res <- hist(datMout[,m], plot=FALSE);
  yvals = c(yvals, res$counts)
}
par(mfrow=c(1,1))
for(m in 1:10)
{
  hist(datMout[,m], main=unlist(attributesSTD[m]), ylim=c(min(yvals), max(yvals)), xlab='', col="blue");
}
#No need to plot histograms for catogoric data - it is not normal distributed anyway

#--------------------------------------------------------------------------------------------
#Similarities (correlation)
#--------------------------------------------------------------------------------------------

#Look at the relation between the attributes
pairs(datMout[,1:10])

#Look at the correlation between the attributes
cor_dat<-cor(datMout)
symnum(cor_dat)

#Look at a correlation plot
#install.packages("corrgram")
library(corrgram)

#Make it so that the size of the correlation is plotted below the diagonal
panel.cor <- function(x, y, digits=2, cex.cor)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  text(0.5, 0.25, paste("r=",round(r,2)))
}
labels <- c("frac_q","frac_d","Coverage","Z-score","Score","Expected value","Host genome size","Phage GC content","Host GC content","U. 15mers in t.","Phage genome size","U. 15mers in q.")
corrgram(datMout[,1:12], order=TRUE, lower.panel=panel.cor,
         upper.panel=panel.pts, text.panel=panel.txt,
         main="Correlation in the Phage Data Set",labels=labels, label.srt = 45, cex= 0.75)
 
#Other measures of similarities
source("similarity.R")

V <-matrix(nrow=10, ncol=10) 
for(j in 1:10){
  for (i in 1:10){
    V[i,j]<-similarity(t(datMout[,j]),t(datMout[,i]),'cor')
  }
}
#other methods jac, ext, cos, cal

#--------------------------------------------------------------------------------------------
#PCA
#--------------------------------------------------------------------------------------------
#Get the SVD decomposition of the standardized data and without outliers
svdres <- svd(datMout)

#Extract the singular values from the result list, svdres
singularvals <- svdres$d

#Calculate the variance explained by each PC - for the report.
pcvariance <- singularvals^2/sum(singularvals^2)

#Inspect what the different components explain
V <- svdres$v

#Inspect the first principal component
V[,1]
#This is explaining the principal directions of the considered PCA components. For the report.

par(mfrow=c(1,1))
#Plot the cumulative proportion of variance explained by the PCs
plot(cumsum(pcvariance), main="Data variance explained by PCs", xlab="Number of PCs included in variance sum", ylab="Proportion of variance explained", col="red",pch=19)

Z <- svdres$u%*%diag(svdres$d)

#Extract the two principal components i and j
i <- 1; j <- 3; # Do it for 1vs2 2vs3 3vs4 4vs5 5vs6
pcx <- Z[,i]
pcy <- Z[,j]

plot(c(min(pcx), max(pcx)), c(min(pcy), max(pcy)), xlab="PC 1", ylab="PC 2", main="Phage genome size", type="n", bty="L")
#Plot points for each principal componen in separate colors
cols <- colors()
for(i in sort(unique(yMout))){
  points(pcx[yMout==i], pcy[yMout==i], col=cols[(i+1)*10])
}
#NOTE: In the exercise is stated that we in the report should explain
#the data projected onto the considered principal components. I am unsure what is meant by that.
#I think that it is what we are doing when plotting the points for each principal component in
#seperate colors above.

#Now a plot where there is room for the legend
plot(c(min(pcx), max(pcx)+13), c(min(pcy), max(pcy)), xlab="PC 1", ylab="PC 2", type="n", bty="L")
#Plot points for each principal componen in separate colors
cols <- colors()
for(i in sort(unique(yMout))){
  points(pcx[yMout==i], pcy[yMout==i], col=cols[(i+1)*10])
}

#Get the order that classes were gone through and plotted in the for loop
classNew<-as.character(classNames)
sorted <- sort(classNew, index.return=TRUE)
#Add legend
legend(6.8,1, legend=classNames[sorted$ix], fill = cols[10*(1:8)], cex=0.7)


#NOTE: In the exercise is stated that we in the report should explain
#the data projected onto the considered principal components. I am unsure what is meant by that.
#I think that it is what we are doing when plotting the points for each principal component in
#seperate colors.

# Another way to do the PCA. 

#install.packages("ChemometricsWithR")
library(ChemometricsWithR)

phagePCA=PCA(datMout) #It is ok that there is a warning, it is not a mistake

par(mfrow=c(1,1))
scoreplot(phagePCA)
loadingplot(phagePCA, show.names = TRUE)
#biplot(phagePCA, score.col = phagePCA$X)
screeplot(phagePCA,type="percentage")
loadings(phagePCA)
#It gives thes same results as before so that is good.

#--------------------------------------------------------------------------------------------
###A boxplot for each attribute for each class
#--------------------------------------------------------------------------------------------
#The goal is to see if an attribute would be good to use to descriminate between classes

# Get the number of data objects, attributes, and classes
Nb = dim(datMout)[1];
Mb = dim(datMout)[2];
Cb = length(classNames);

yvalsb = c()
for(m in 0:(Cb-1))
{
  res <- boxplot(datMout[m==yMout,], plot=FALSE)
  yvals = rbind(yvalsb, res$stats)
}

par(mfrow=c(2,4))
for(m in 0:(Cb-1))
{
  boxplot(datMout[m==yMout,], main=paste("Boxplot for", classNames[m+1]), ylim=c(min(yvals), max(yvals)))
}
#Plot it in 8 different plots.
yvalsb = c()
for(m in 0:(Cb-1))
{
  res <- boxplot(datMout[m==yMout,], plot=FALSE)
  yvals = rbind(yvalsb, res$stats)
}

par(mfrow=c(1,1))
for(m in 0:(Cb-1))
{
  boxplot(datMout[m==yMout,], main=paste("Boxplot for", classNames[m+1]), ylim=c(min(yvals), max(yvals)))
}

#--------------------------------------------------------------------------------------------
###A 3d scatter plot of the data
#--------------------------------------------------------------------------------------------
#I don't think we should include it in the report, since we are having more than 3 attributes.
#If to be included, then find the 3 attributes that contribute the most to the first 2 PC

#A 3d plot (no rotation)
library(scatterplot3d)
par(mfrow=c(1,1))
cols <- c("blue", "green3", "red", "yellow","black","grey","magenta","pink")
cols <- cols[1:length(classNames)]
s3d <- scatterplot3d(datMout[,1:3], type="n")
for(c in 1:Cb){
  s3d$points3d(datMout[(c-1)==yMout,1:3],  col=cols[c])
}

#A 3d plot with rotation
library(rgl)
cols = rep("black", times=length(y))
cols[yMout==0] <- "blue"
cols[yMout==1] <- "green3"
cols[yMout==2] <- "red"
cols[yMout==3] <- "pink"
cols[yMout==4] <- "grey"
cols[yMout==5] <- "yellow"
cols[yMout==6] <- "grey"
cols[yMout==7] <- "magenta"
plot3d(datMout[,1:3], col=cols, size=3) 
