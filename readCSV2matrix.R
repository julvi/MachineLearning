####################
# exercise 2.2.1
####################
# read data into R
dat <- read.csv("./Data/nanonose.csv", sep=",", check.names=FALSE)
# extract class labels of observations
class_labels <- colnames(dat)
#do not look at the first 2
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
