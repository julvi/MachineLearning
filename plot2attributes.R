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
plot(c(min(sensorx), max(sensorx)), c(min(sensory), max(sensory)), xlab="Sensor A", ylab="Sensor B", main="NanoNose data")

# plot points for each sensor in separate colors
cols <- colors()
for(i in sort(unique(y))){
points(sensorx[y==i], sensory[y==i], col=cols[(i+1)*10])
}

# get the order that classes were gone through and plotted in for loop
sorted <- sort(class_names, index.return=TRUE)
# add legend
legend("topright", legend=class_names[sorted$ix], fill = cols[10*(1:5)])
