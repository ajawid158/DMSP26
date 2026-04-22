#Unsupervised Learning Methods 
#PCA

library(corrplot)
library(dplyr)

x=read.csv("employee.csv")
head(x)

#PCA is to reduce the # of variables 
#High Correlation 
names(x)

fv=x %>%
  select(Salary, Tax, Spending, Sving)

cr.fv=cor(fv)
cr.fv

corrplot(cr.fv, method = "pie", type = "lower")

#apply PCA
fv_pca=prcomp(fv, scale= TRUE)
summary(fv_pca)

#extract the pcs / 2PC
pca_select=fv_pca$x
head(pca_select)


#Slection the first 2 PCs
pca_fv_best=pca_select[,c(1,2)] %>% as.data.frame()
head(pca_fv_best)

cor(pca_fv_best)
#-9.5/ 10^17 = 0 

