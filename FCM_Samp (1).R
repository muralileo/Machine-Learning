library(readxl)
library("cluster")
install.packages("NbClust")
library(NbClust)
install.packages("factoextra")
library("factoextra")
install.packages("ppclust")
library(ppclust)
MSN_Sample <- read_excel("D:/Ph.D/DataSets/MSN DataSet/MSN_Col.xlsx")

# To find the optimalnoof clusters
nb <- NbClust(MSN_Sample,min.nc = 2, max.nc = 10, method = 'kmeans',index="all")
fviz_nbclust(nb)+theme_minimal()

#Using FCM Algorithm
result_fcm <- fcm(MSN_Sample,centers = 3, nstart = 5)
summary(result_fcm)
sil <- silhouette(result_fcm$cluster,dist(MSN_Sample))
head(sil[,1:3],10)
plot(sil,main = "Slhouette Plot for Fuzzy Means")
fviz_silhouette(sil)

#Using Scale
x <- scale(MSN_Sample[,17])
v <- inaparc::kmpp(x, k=3)$v
print(v)
u <- inaparc::imembrand(nrow(x),k=3)$u
print(u)
fcm.res <- fcm(x, centers = v,memberships = u,m=2)
head(fcm.res$u,5)
summary(fcm.res)
sil <- silhouette(fcm.res$cluster,dist(x))
head(sil[,1:3],10)
plot(sil,main = "Slhouette Plot for Fuzzy Means")
fviz_silhouette(sil)


#Using Fanny Algorthim
result_fanny <- fanny(MSN_Sample,3, metric="euclidean")
summary(result_fanny)
print(result_fanny$clustering)
sil_fanny <- silhouette(result_fanny)
fviz_silhouette(sil_fanny)
