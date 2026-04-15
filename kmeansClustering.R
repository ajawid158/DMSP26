###Unspervised Learning methods
#2. KMeans Clustering
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

n_clust= fviz_nbclust(g1, kmeans, method = "silhouette")
n_clust

#therefore the number of clusters = 2

#Step 2: create the 2 clusters 
h.w.cl=kmeans(g1, 2)

#Step 3: Extract the two clusters
h.w.cl=h.w.cl$cluster
h.w.cl

head(g1)
g1=g1 %>%
  mutate(h.w.cl)
head(g1)

#visualize the clusters
plot(g1$Height, g1$Weight, 
     col=g1$h.w.cl, 
     pch=8)

legend(180, 80, unique(g1$h.w.cl), 
       col = 1:2, pch = 8)

