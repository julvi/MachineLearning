# exercise 5.1.2
source(file.path("Scripts", "ex5_1_1.R")) # get data
# if you do not already have the package "rpart" installed, install it with the command install.packages("rpart")
#rpart can be used both for Classification and Regression taskyu
library(rpart)
Xdatframe <- data.frame(X)
colnames(Xdatframe) <- attributeNames
#convert the columns into factors
Xdatframe[,attributeNames] <- lapply(Xdatframe[,attributeNames] , factor)
# check that Xdatframe represents data as categorical variables
summary(Xdatframe)
####################################################################################
# fit classification tree
####################################################################################
#Get a cell array of class names for each data object by using y to index
#into classNames like this:
#classNames[y+1]
#[1] "Mammal"    "Reptile"   "Fish"      "Mammal"    "Amphibian" "Reptile"   "Mammal"   
#[8] "Bird"      "Mammal"    "Fish"      "Reptile"   "Bird"      "Mammal"    "Fish"     
#[15] "Amphibian"
#The parameter split in the list of parameters, parms, can be used to choose
#the splitting criterion. So for example, to use the Gini index, pass the following
#parms=list(split=’gini’) to rpart.
#The parameters minsplit, minbucket, and cp inﬂuence the stopping criterion
mytree <- rpart(classNames[y+1] ~Body.temperature + Skin.cover + Gives.birth + Aquatic.creature + Aerial.creature + Has.legs + Hibernates , data=Xdatframe, control=rpart.control(minsplit=1, minbucket=0, cp=0), parms=list(split='gini'), method="class")

par(xpd=NA) # make room for text labels
plot(mytree)
text(mytree, pretty=0) # pretty = 0 makes attribute values show up as the numerical values they take in the data matrix X instead of encoding using a, b, c, etc.
# inspect details of tree
summary(mytree)
