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

# get the order that classes were gone through and plotted in for loop
sorted <- sort(class_names, index.return=TRUE)
# add legend
legend("topright", legend=class_names[sorted$ix], fill = cols[10*(1:5)])


####################
# exercise 2.2.3
####################
#calculate the mean for each column -> it creates a double precision vector. Each column is associated with its mean
means <- colMeans(datfinal)
#substract the mean to the data row-wise
datzeromean<- t(apply(datfinal,1,'-',means))
#have a look at the columns' means
colMeans(datzeromean)

#calculate the Singular value decomposition:
#Creates a list: 
#The first element $d is a double. vector of 1*8 elements (1 per each attribute) -> Diagonal matrix, the values on the main diagonal are shown
#The second element $u is a double. Matrix of 90*8 elements -> Orthonormal Matrix, Dataobjects*Components(projections of the vectors on the basis vectors)
#The third element $v is a double. Matrix of 8*8 elements ->Orhonormal Matrix, Attributes*Components(?)
svdres <- svd(datzeromean)

#take the entries of the Diagonal Matrix = Singular Values.
#These are sorted(largest first). Indicate how much variability is explainded by the corresponding component
singularvals <- svdres$d

#principal component variance: calculate the fraction of the variation in the data is explained by each principal component
pcvariance <- singularvals^2/sum(singularvals^2)
#sum the first 3 variation fractions
sum(pcvariance[1:3]) # -> by considering the first 3 principal components I keep 92.7% of the total information

#plot the cumulative sum of the principal component variance
plot(cumsum(pcvariance), main="Data variance explained by PCs", xlab="Number of PCs included in variance sum", ylab="Proportion of variance explained")

####################
# exercise 2.2.4
####################

diag(svdres$d) #->matrix where the singular values are visualized in its main digonal and outside it are all 0
#multiply the orthonormal matrix u(DataObjects*Components) with the diagonal matrix d
Z <- svdres$u%*%diag(svdres$d)

#we take 2 principal components to represent our data -> first and second column
pcx <- Z[,1]
pcy <- Z[,2]
#setting the limits for the plot
plot(c(min(pcx), max(pcx)), c(min(pcy), max(pcy)), xlab="PC A", ylab="PC B", main="NanoNose data", type="n")

# plot points for each pc in separate colors
cols <- colors()
for(i in sort(unique(y))){
points(pcx[y==i], pcy[y==i], col=cols[(i+1)*10])
}


# get the order that classes were gone through and plotted in for loop
sorted <- sort(class_names, index.return=TRUE)
# add legend
legend("topright", legend=class_names[sorted$ix], fill = cols[10*(1:5)])

svdres$v[,2]
