# exercise 4.1.1
source('setup.R')
# read data into R
dat <- read.csv("./Data/iris.csv", sep=",", check.names=FALSE)

# inspect the contents of the variable "dat"
names(dat)
head(dat)

# Extract the rows and columns corresponding to the data, i.e. the attribute values
X = dat[,1:4]

# Extract attribute names from the first row
attributeNames = colnames(dat)[1:(dim(dat)[2]-1)];

# Extract unique class names from the last column
classLabels = dat[,5];
classNames = unique(classLabels);

# Extract class labels that match the class names
y = match(classLabels, classNames)
y = t(y)-1;

# Get the number of data objects, attributes, and classes
N = dim(X)[1];
M = dim(X)[2];
C = length(classNames);

