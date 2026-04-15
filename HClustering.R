###Unspervised Learning methods
#1. H-Clustering
library(dplyr)
library(factoextra)


g=read.csv("employee.csv")
head(g)
View(g)

#Objective: Cluster the objects based on their 
#physical charateristics

#Height and Weight 

g1= g %>%
      select(Height, Weight)
head(g1)

#Step 1: Specify the optimal number of clusters

n_clust= fviz_nbclust(g1, FUNcluster = hcut, method = "silhouette")
n_clust

#n_cluster=2
#Step 2: use either Average or Complete to create the 2 clusters
h1= hclust(dist(g1), method = "complete")
plot(h1)

h1= hclust(dist(g1), method = "average")
plot(h1)

#we go for average
#Step 3: Extract cluster
hw.cl=cutree(h1, 2)
hw.cl

#Afterwork tasks

g1=g1 %>%
  mutate(hw.cl)

View(g1)

#visualize the clusters
plot(g1$Weight, g1$Height, 
     col=g1$hw.cl, 
     pch=8)
legend(82, 180, unique(g1$hw.cl), 
       col = 1:2, pch = 8)




