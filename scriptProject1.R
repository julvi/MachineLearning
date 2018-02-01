#############################################################################################
# Project 1                                                                                 #
#############################################################################################
#clear the work space
rm(list=ls())
#----------------------
#Load the data into R
#----------------------
#Set working directory
setwd("~/Google Drive/PhD/Courses/Introducion to Machine Learning and Data Mining /dataset")

#Read data in
dat <- read.table("phageDS.complete25FEB.txt", header = T, as.is = T)
#dat <- read.csv("phageDS.complete4.csv", sep=",", check.names=FALSE)
dim(dat)
#[1] 126  16


#######Remove the columns#######
#1: phage ID --> does not give any information it is different for each observation
#######Class or related to class attributes#######
#5: annotated host: our class labels
#7: host GC content: related to the host
#6: host genome size: related to the host
#######Categorcal attributes: we will convert them to numerical columns#######
#4: phage family: categorical attribute
#8: predicted host: categorical attribute

datnew <- dat[,c(-1,-4,-5,-6,-7,-8)]

#Chek that the dimensions of the data set is still ok
dim(datnew)
#[1] 126  10

###################################################################################################################
#Defining the class: annotated host
###################################################################################################################
#Extract class labels of observations. Jump over the first row (column names)
classlabels <- dat[1:126,5]

##Extract the class names present in data
classNames <- unique(classlabels)

#Extract numeric class assignments
y <- as.numeric(as.factor(classlabels))
y <- y-1

###################################################################################################################
#Plot Phage GC content according to the phage genome size and color differently different classes
###################################################################################################################
#Choose which attributes to plot
attX = datnew[,1]   #phage_genome_size
attY = datnew[,2]   #phage_GCcontent

#Make a simple plot
plot(attX, attY)

#Make it a more fancy plot:
#First assign titles and labels to the plot, and determine its size by giving the minimum
#and maximum values. Do not plot anything (the option type="n")
plot(c(min(attX), max(attX)), c(min(attY), max(attY)), xlab="Phage genome size", ylab="phage GCcontent", main="Phage data", bty="L", type="n")

# plot points for each sensor in separate colors
cols <- colors()
for(i in sort(unique(y))){
  points(attX[y==i], attY[y==i], col=cols[(i+1)*10])
}

#Now a plot where there is room for the legend
plot(c(min(attX), max(attX)+150000), c(min(attY), max(attY)), xlab="Phage genome size", ylab="phage GCcontent", main="Phage data", bty="L", type="n")

# plot points in separate colors
cols <- colors()
for(i in sort(unique(y))){
  points(attX[y==i], attY[y==i], col=cols[(i+1)*10])
}
#Turn the class names into independent charaters
classNew<-as.character(classNames)

#Get the order that classes were gone through and plotted in the for loop
sorted <- sort(classNew, index.return=TRUE)
#Add legend
legend(200500, 70, legend=classNames[sorted$ix], fill = cols[10*(1:8)],cex=0.7)
# NOTE: cex change the size of the text


###################################################################################################################
#Standardize the attributes
###################################################################################################################


#Make the 2 categorical attributes into something that we can use for PCA.
source('categoric2numeric.R')

#Convert the column of phage family to 9 columns of numeric values 
col4 = categoric2numeric(dat[,4])
#attach the numeric columns to datne
datnew[,11:19] <- col4[[1]]
#extract the familynames
familynames <- col4[[2]]
#assign the familynames as column names
colnames(datnew)[11:19] <- familynames

#Convert the column of predicted host to 9 columns of numeric values
col8 = categoric2numeric(dat[,8])
datnew[,20:28] <- col8[[1]]
predhost <- col8[[2]]
colnames(datnew)[20:28] <- predhost

#Extract the means of columns
means <- colMeans(datnew)

#Subtract the column means from each row. 
#Transpose result since apply returns a matrix corresponding to the transposed datfinal
datzeromean<- t(apply(datnew,1,'-',means))


#Extract the standard deviation of the columns
sd <-apply(datzeromean, 2, 'sd')

datStandard <- t(apply(datzeromean,1,'%/%',sd))


#Check that column means are now close to each other
colMeans(datStandard)

#Check that the colum SD are now close to one
apply(datStandard, 2, 'sd')

#weight the converted categorical columns by dividing the values by the square root of the number
#of converted columns per categorical column
datStandard[,11:19] <- datStandard[,11:19]*(1/sqrt(9))
datStandard[,20:28] <- datStandard[,20:28]*(1/sqrt(9))

###------------------------------------------------
#Check for outliers
###------------------------------------------------
attributes <- colnames(datStandard)
#A boxplot of the eight attributes (NON standardized)
par(mfrow=c(1,1))
boxplot(datnew, main="Boxplots of attribute values")

#A boxplot of the eight attributes (standardized)
par(mfrow=c(1,1))
boxplot(datStandard, main="Boxplots of attribute values")
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
  hist(datStandard[,m[i]],breaks=51, main=unlist(attributes[m[i]]), ylim=c(min(yvals), max(yvals)))
}
#For sure numer 3 has a outlier, it is hard to see the others. 
# It looks more like they are widely dirstributed.
#We remove the outlier from number 3 attribute

idxOutlier = datStandard[,6]>4

# Finally we will remove these from the data set
X = datStandard[-which(idxOutlier),]
yl = datStandard[-which(idxOutlier)]
N = N-sum(idxOutlier);

#Now we look at the boxplot again
par(mfrow=c(1,1))
boxplot(X, main="Boxplots of attribute values, removed 1 outlier")
#NOTE: now it looks better. I am not sure whether to remove the rest also, what do you think?

idxOutlierT = X[,1]>1.5 |X[,5]>1.5  | X[,7]>1.5 | X[,8]>1.5 | X[,9]>1.5
# Finally we will remove these from the data set
XT = X[-which(idxOutlierT),]
yT = y[-which(idxOutlierT)]
NT = N-sum(idxOutlierT);

#Now we look at the boxplot again
par(mfrow=c(1,1))
boxplot(XT, main="Boxplots of attribute values, with no outliers")
#NOTE: I am not sure what is wrong with numer 6 attribute, but the rest look ok

#----------------------------------------------------------
#Make PCA
#----------------------------------------------------------

#Get the SVD decomposition of the standardized data
svdres <- svd(datStandard)


#Extract the singular values from the result list, svdres; svdres$d is the diagonal matrix
singularvals <- svdres$d

#Calculate the variance explained by each PC - for the report.
pcvariance <- singularvals^2/sum(singularvals^2)

#Inspect what the different components explain (how much data variance each component explains). V is a 10*10 matrix attribute*component
V <- svdres$v

#Inspect the first principal component
V[,1]
#This is explaining the principal directions of the considered PCA components. For the report.

#Plot the cumulative proportion of variance explained by the PCs
plot(cumsum(pcvariance), main="Data variance explained by PCs", xlab="Number of PCs included in variance sum", ylab="Proportion of variance explained")

Z <- svdres$u%*%diag(svdres$d)

#Extract the two principal components i and j
i <- 1; j <- 2;
pcx <- Z[,i]
pcy <- Z[,j]

plot(c(min(pcx), max(pcx)), c(min(pcy), max(pcy)), xlab="PC 1", ylab="PC 2", main="Phage genome size", type="n", bty="L")

#Plot points for each principal componen in separate colors
cols <- colors()
for(i in sort(unique(y))){
  points(pcx[y==i], pcy[y==i], col=cols[(i+1)*10])
}
#NOTE: In the exercise is stated that we in the report should explain
#the data projected onto the considered principal components. I am unsure what is meant by that.
#I think that it is what we are doing when plotting the points for each principal component in
#seperate colors above.


#Now a plot where there is room for the legend
plot(c(min(pcx), max(pcx)+5), c(min(pcy), max(pcy)+3), xlab="PC 1", ylab="PC 2", main="Phage genome size", type="n", bty="L")

#Plot points for each principal componen in separate colors
cols <- colors()
for(i in sort(unique(y))){
  points(pcx[y==i], pcy[y==i], col=cols[(i+1)*10])
}

#Get the order that classes were gone through and plotted in the for loop
sorted <- sort(classNew, index.return=TRUE)
#Add legend
legend(4.5,9, legend=classNames[sorted$ix], fill = cols[10*(1:8)], cex=0.7)


#NOTE: In the exercise is stated that we in the report should explain
#the data projected onto the considered principal components. I am unsure what is meant by that.
#I think that it is what we are doing when plotting the points for each principal component in
#seperate colors.

#NOTE: Another way to do the PCA. 

install.packages("ChemometricsWithR")
library(ChemometricsWithR)

phagePCA=PCA(scale(datPCA))

par(mfrow=c(1,1))
scoreplot(phagePCA)
loadingplot(phagePCA, show.names = TRUE)
#biplot(phagePCA, score.col = phagePCA$X)
screeplot(phagePCA,type="percentage")
loadings(phagePCA)
#NOTE: This looks like what we have found when scaling that is good.

########-------------------------------------------------
#Summary statictics
########-------------------------------------------------

#Calculate the mean, variance, median and range of the different variables.
#Before any scaling
mean_dat <- apply(datPCA, 2, mean)
var_dat <- apply(datPCA, 2, var)
median_dat <- apply(datPCA, 2, median)
range_dat <- diff(apply(datPCA, 2, range)) ## range returns the minimum and maximum of the vector
#After that the difference is taken between those.

#NOTE: comparing the mean and the median could give a hint whether there are any outliers.
#Looking at these values could also tell whether or not it is a good ide to standadize data.
#Since there is a big difference, it is good that we scale.
########-------------------------------------------------
#Similiarities
########-------------------------------------------------

#Look at the relation between the attributes
pairs(datStandard[,1:10])

#Look at the correlation between the attributes
cor_dat<-cor(datStandard)
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

corrgram(datStandard[,1:10], order=TRUE, lower.panel=panel.cor,
         upper.panel=panel.pts, text.panel=panel.txt,
         main="Correlation in the Phage Data Set")

#Another correlation plot
library(gclus)
dta.r <- abs(cor(datPCA)) # get correlations
dta.col <- dmat.color(dta.r) # get colors
# reorder variables so those with highest correlation
# are closest to the diagonal
dta.o <- order.single(dta.r) 
cpairs(datPCA, dta.o, panel.colors=dta.col, gap=.5,
       main="Variables Ordered by Correlation" )
#The ones close to the diagonal has the highest correlation

#Other measures of similarities
source("similarity.R")
#how to make a plot with the similarities
#http://stackoverflow.com/questions/15271103/how-to-modify-this-correlation-matrix-plot

v1=numeric(length=10)
for (i in 1:10){
  v1[i]<-similarity(t(datPCA[,1]),t(datPCA[,i]),'cos')
}
v2=numeric(length=10)
for (i in 1:10){
  v2[i]<-similarity(t(datPCA[,2]),t(datPCA[,i]),'cos')
}
v3=numeric(length=10)
for (i in 1:10){
  v3[i]<-similarity(t(datPCA[,3]),t(datPCA[,i]),'cos')
}
v4=numeric(length=10)
for (i in 1:10){
  v4[i]<-similarity(t(datPCA[,4]),t(datPCA[,i]),'cos')
}
v5=numeric(length=10)
for (i in 1:10){
  v5[i]<-similarity(t(datPCA[,5]),t(datPCA[,i]),'cos')
}
v6=numeric(length=10)
for (i in 1:10){
  v6[i]<-similarity(t(datPCA[,6]),t(datPCA[,i]),'cos')
}
v7=numeric(length=10)
for (i in 1:10){
  v7[i]<-similarity(t(datPCA[,7]),t(datPCA[,i]),'cos')
}
v8=numeric(length=10)
for (i in 1:10){
  v8[i]<-similarity(t(datPCA[,8]),t(datPCA[,i]),'cos')
}
v9=numeric(length=10)
for (i in 1:10){
  v9[i]<-similarity(t(datPCA[,9]),t(datPCA[,i]),'cos')
}
v10=numeric(length=10)
for (i in 1:10){
  v10[i]<-similarity(t(datPCA[,10]),t(datPCA[,i]),'cos')
}

cos_together=c(v1,v2,v3,v4,v5,v6,v7,v8,v9,v10)
cos_together
#other methods jac, ext, cos, cal

###------------------------------------------------
#Check if the attributes are normally distributed
###------------------------------------------------
#Check for normallity by making a qq plot of all the attributes
par(mfrow=c(2,5))
for (i in 1:10){
  qqnorm(datPCA[,i])
}
#NOTE: Some of them look normally distributed

#And for the data with zero mean and 1 in SD:
par(mfrow=c(2,5))
for (i in 1:10){
  qqnorm(datStandard[,i])
}
#NOTE: I am not sure why it looks a bit strange

#Check by plotting a histogram
par(mfrow=c(2,5))
for (i in 1:10){
  hist(datPCA[,i],col=i)
}

par(mfrow=c(2,5))
for (i in 1:10){
  hist(datStandard[,i],col=i)
}
#NOTE: again very few look normally distributed



####################################################################
##A boxplot for each attribute for each class
####################################################################

# Get the number of data objects, attributes, and classes
N = dim(datnew)[1];
M = dim(datnew)[2];
C = length(classNames);

#A boxplot for each attribute for each class (NON standardized)
yvals = c()
for(m in 0:(C-1))
{
  res <- boxplot(datnew[m==y,], plot=FALSE)
  yvals = rbind(yvals, res$stats)
}

par(mfrow=c(2,4))
for(m in 0:(C-1))
{
  boxplot(datnew[m==y,], main=paste("Boxplot for", classNames[m+1]), ylim=c(min(yvals), max(yvals)))
}


########################################################
#Make a 3d plot of the data
########################################################
#NOTE: I don't think we should include this in the report, it does not make so much sense when
#we have so many dimensions - compared to only 4 in the Iris set.

library(scatterplot3d)
par(mfrow=c(1,1))
# note that if more than three classes are inspected
#then this vector of colors is not long enough. Thus more colors need to be added.
cols <- c("blue", "green3", "red", "yellow","black","grey","magenta","pink")
cols <- cols[1:length(classNames)]
s3d <- scatterplot3d(datPCA[,1:3], type="n")
for(c in 1:C){
  s3d$points3d(datPCA[(c-1)==y,1:3],  col=cols[c])
}
#legend("topright", legend=classNames, fill=unique(cols[match(y,as.numeric(classNames))]))


library(rgl)
# if more than three classes are inspected this code must also be modified accordingly
cols = rep("black", times=length(y))
cols[y==0] <- "blue"
cols[y==1] <- "green3"
cols[y==2] <- "red"
cols[y==3] <- "pink"
cols[y==4] <- "grey"
cols[y==5] <- "yellow"
cols[y==6] <- "grey"
cols[y==7] <- "magenta"
plot3d(datPCA[,1:3], col=cols, size=3) 



#Notes for the report:
# Saving a plot directly to a pgn-file:
#png("logprice_relations.png",width=800,height=600)

#How to save a table so that it can be included in the report
#library(xtable)
#capture.output(print(xtable(cor(saltdata)),type="html"),file="cortable.html")
