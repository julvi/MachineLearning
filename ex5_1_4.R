# exercise 5.1.4
source(file.path("Scripts", "ex5_1_1.R")) # get data
library(rpart)
Xdatframe <- data.frame(X)
colnames(Xdatframe) <- attributeNames
Xdatframe[,attributeNames] <- lapply(Xdatframe[,attributeNames] , factor)

mytree <- rpart(classNames[y+1] ~Body.temperature + Skin.cover + Gives.birth + Aquatic.creature + Aerial.creature + Has.legs + Hibernates , data=Xdatframe, control=rpart.control(minsplit=1, cp=0), parms=list(split='information'), method="class")

# Define a new data object (a dragon) with the attributes given in the text
x = data.frame(t(c(0, 2, 1, 2, 1, 1, 1)))
colnames(x) <- attributeNames
x[,attributeNames] <- lapply(x[,attributeNames] , factor)

# Evaluate the classification tree for the new data object
#?predict.rpart
predict(mytree, newdat=x)
