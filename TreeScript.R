####################################################################################
# Fit a classification tree
####################################################################################
##Use scripts from ex5_1_1 to 5_1_4.

#Clear workspace ect:
rm(list=ls())
graphics.off() # close all open graphics windows
#use controle l to clear the consol window

#----
#Packages
#----
library(rpart)

#----------------------
#Load the data into R
#----------------------
#Set working directory
#setwd("~/Google Drive/PhD/Courses/Introducion to Machine Learning and Data Mining /dataset")
setwd('/Users/lenesommer/DTU/Kandidat/F2014/IntroTilMachineLearning /Project2')

#Read data in
dat <- read.table("phageDS.complete25FEB.txt", header = T, as.is = T)
dim(dat)

#Extract class labels of observations
classlabels <- dat[1:97,5] #only four classes as a test


#Remove the fith column because it is with the class labels
#Remove the first because it is the ID
#Remove host GC content because it is related to the host and also host genome size
#Make it so that there are only four classes - as a test (1:97)
XC <- dat[1:97,-c(1,5,6,7)]
dim(XC)

#Extract attributes, i.e. names of attributes
attributeNamesC <- colnames(XC)

#Extract the class names present in data
classNamesC <- unique(classlabels)

#Extract numeric class assignments
yC <- as.numeric(as.factor(classlabels))
yC <- yC-1

# Number data objects, attributes, and classes
N = 97;
M = 12;
C = 4;


Xdatframe <- data.frame(XC)
colnames(Xdatframe) <- attributeNamesC

#convert the columns into factors
Xdatframe[,attributeNamesC] <- lapply(Xdatframe[,attributeNamesC] , factor)

# check that Xdatframe represents data as categorical variables
summary(Xdatframe)


#Get a cell array of class names by using y to index into classNames like this:
classassignments <- (classNamesC[yC+1])

#use the Gini index, parms=list(split=’gini’) to rpart.

#library(alr3) - test for at bruge na.action=na.exclude
#options("na.action")

# construct formula to fit automatically to avoid typing in each variable name
(fmla <- as.formula(paste("classassignments ~ ", paste(attributeNamesC, collapse= "+"))))
mytree <- rpart(fmla, data=Xdatframe, na.action="na.exclude", control=rpart.control(minsplit=100, minbucket=1, cp=0), parms=list(split='gini'), method="class")
#wont run when minsplit is chnged from 100 to 10, not even when it is only 90.

par(xpd=NA) # make room for text labels
plot(mytree)
text(mytree, pretty=0) # pretty = 0 makes attribute values show up as the numerical values they take in the data matrix X instead of encoding using a, b, c, etc.
#Error in plot.rpart(mytree) : fit is not a tree, just a root

# inspect details of tree
summary(mytree)
