##Supervised Learning methods
#Random forest

library(dplyr)
library(randomForest)
library(ggplot2)

rfdata=iris
str(rfdata)

#Y: Species
#X: the rest

names(rfdata)
#Split our data into Train and test
s=sample(nrow(rfdata), 0.7*nrow(rfdata))

rftrain=rfdata[s,]
rftest=rfdata[-s,]

#Train the model

rfm=randomForest(Species~., data = rftrain)

##Test the perfomrance of the model using test dataset
rfpredict=predict(rfm, rftest)

#Cofunsion matrix
rfcm=table(rftest$Species, rfpredict)
rfcm
