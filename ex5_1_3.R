# exercise 5.1.3
source(file.path("Scripts", "ex5_1_1.R")) # get data
library(rpart)
Xdatframe <- data.frame(X)
colnames(Xdatframe) <- attributeNames
Xdatframe[,attributeNames] <- lapply(Xdatframe[,attributeNames] , factor)
# check that Xdatframe represents data as categorical variables
summary(Xdatframe)

# fit classification tree using  entropy
mytree <- rpart(classNames[y+1] ~Body.temperature + Skin.cover + Gives.birth + Aquatic.creature + Aerial.creature + Has.legs + Hibernates , data=Xdatframe, control=rpart.control(minsplit=1, minbucket=0, cp=0), parms=list(split='information'), method="class")


par(xpd=NA) # make room for text labels
plot(mytree)
text(mytree, pretty=0) # pretty = 0 makes attribute values show up as the numerical values they take in the data matrix X instead of encoding using a, b, c, etc.
# inspect details of tree
summary(mytree)


#The tree is slightly different from Matlab one: the attribute values at the node
#position corresponds to the branch that splits further (or on the left side). 