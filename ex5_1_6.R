#exercise 5.1.6

# get data
source(file.path("Scripts", "ex5_1_5.R"))

library(rpart)
Xdatframe <- data.frame(X)
colnames(Xdatframe) <- attributeNames
classassignments <- classNames[y+1]

# construct formula to fit automatically to avoid typing in each variable name
(fmla <- as.formula(paste("classassignments ~ ", paste(attributeNames, collapse= "+"))))

# fit classification tree
mytree <- rpart(fmla, data=Xdatframe,control=rpart.control(minsplit=100, minbucket=1, cp=0), parms=list(split='gini'), method="class")
#the lower minbucket the higher the number of nodes in the tree
#minsplit indicate the minumn number of object that are needed to make a split
par(xpd=NA)
plot(mytree)
text(mytree)

