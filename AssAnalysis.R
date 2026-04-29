##Unsupervised Learning methods
##Association Analysis
#Market Basket Analysis

library(arules)
library(arulesViz)

m=read.csv("market.csv")
View(m)
dim(m)

q=as.matrix(m)
q=as(q, "transactions")

inspect(q)
summary(q)

itemFrequency(q)
itemFrequencyPlot(q, support=0.5)

#Bread has the highest Support
#Freq an Item/Total number of Transaction 

#Conf(Bread->Orange) 
#Pr(Orange | Bread) The chance of Buying Orange
#give bread is already bought

m1= apriori(q)
inspect(m1)
