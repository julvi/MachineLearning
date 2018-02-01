####################
# exercise 2.2.1
####################
# read data into R
setwd("~/GoogleDrive/PhD/Courses/IntroductiontoMachineLearningandDataMining/ToolBox/02450Toolbox_R")
dat <- read.csv("./Data/nanonose.csv", sep=",", check.names=FALSE)
# extract class labels of observations
class_labels <- colnames(dat)
class_labels <- class_labels[-(1:2)]
# extract attributes, i.e. sensor names
attributes <- dat[3:10,1]

# remove first two rows and columns
datnew <- dat[-(1:2),-(1:2)]

# transpose data matrix
datfinal <- t(datnew)

# check that dimensions are as they should be (90 rows and 8 columns)
dim(datfinal)

# assign the class labels as row names and the attributes as column names
rownames(datfinal) <- class_labels
colnames(datfinal) <- attributes

# extract the class names present in data
class_names <- unique(class_labels)

# Extract numeric class assignments
y <- as.numeric(as.factor(class_labels))
y <- y-1

####################
# exercise 2.2.2
####################
# choose which sensors to plot
sensorx = datfinal[,1]
sensory = datfinal[,2]

# make simple plot
plot(sensorx, sensory)

# make more fancy plot
# first assign titles and labels to the plot, and determine its size by giving the minimum and maximum values of the sensors. Do not plot anything (the option type="n")
plot(c(min(sensorx), max(sensorx)), c(min(sensory), max(sensory)), xlab="Sensor A", ylab="Sensor B", main="NanoNose data", type="n")

# plot points for each sensor in separate colors
cols <- colors()
for(i in sort(unique(y))){
points(sensorx[y==i], sensory[y==i], col=cols[(i+1)*10])
}

# get the order that classes were gone through and plotted in in for loop
sorted <- sort(class_names, index.return=TRUE)
# add legend
legend("topright", legend=class_names[sorted$ix], fill = cols[10*(1:5)])


####################
# exercise 2.2.3
####################
# extract the means of columns
means <- colMeans(datfinal)

# subtract the column means from each row. Transpose result since apply returns a matrix corresponding to the transposed datfinal
datzeromean<- t(apply(datfinal,1,'-',means))

# check that column means are now zero (or are numerically different from zero by something on the order of 10^-14)
colMeans(datzeromean)

# get the SVD decomposition of the zero-mean data
svdres <- svd(datzeromean)

# extract the singular values from the result list, svdres
singularvals <- svdres$d

# calculate the variance explained by each PC
pcvariance <- singularvals^2/sum(singularvals^2)

# plot the cumulative proportion of variance explained by the PCs
plot(cumsum(pcvariance), main="Data variance explained by PCs", xlab="Number of PCs included in variance sum", ylab="Proportion of variance explained")

####################
# exercise 2.2.4
####################

Z <- svdres$u%*%diag(svdres$d)

# extract the two principal components i and j
i <- 1; j <- 2;
pcx <- Z[,i]
pcy <- Z[,j]

plot(c(min(pcx), max(pcx)), c(min(pcy), max(pcy)), xlab="PC A", ylab="PC B", main="NanoNose data", type="n")

# plot points for each pc in separate colors
cols <- colors()
for(i in sort(unique(y))){
points(pcx[y==i], pcy[y==i], col=cols[(i+1)*10])
}

# get the order that classes were gone through and plotted in in for loop
sorted <- sort(class_names, index.return=TRUE)
# add legend
legend("topright", legend=class_names[sorted$ix], fill = cols[10*(1:5)])

####################
# exercise 2.2.5
####################

#To get which of the original attributes does the second principal component
#mainly capture the variation of and what would cause an observation to have
#a large negative/positive projection onto the second principal component:
#The columns of V gives you the principal component directions
#The data is projected onto the second principal component by Y%*%V[,2]
V <- svdres$v

# inspect the second principal component
V[,2]
#Y%*%V[,2]
