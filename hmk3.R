install.packages("cluster")
install.packages("NbClust")
install.packages("factoextra")
install.packages("fpc")
install.packages("dbscan")

library(cluster) 
library(NbClust)
library(factoextra)
library(fpc)
library(dplyr)
library(dbscan)

#Setting seed
set.seed(123)

#Importing data
df <- read.csv("/Users/jamesvalles/Desktop/dshmk3/speed_dating_sample.csv", header=T, sep=',')

#Assign id as name of row
row.names(df) < df$iid

#dropping the id column
df <- select(df, -c(iid))

#Checking to make sure all datatypes are int or num
str(df)

#Scaling data 
df2 = data.frame(scale(df))

#Part 1

#1. Determine optimal value of K. 
nc <- NbClust(df2, min.nc=3, max.nc=6, method="kmeans")

#2. Create K-means model using optimal K and visualize results
km_3 <- kmeans(df2, 3, nstart=25)

#Create cluster visulization using fviz_cluster
fviz_cluster(km_3, data = df2, repel = TRUE, show.clust.cent = FALSE)

#3. Generate boxplots for attr_date, since_date, intel_date, amb_date, shared_date
par(mfrow-c(1,5))
for (i in 18:22){
  boxplot(df[[i]] ~ km_3$cluster, xlab="cluster", main=names(df)[i])
}
par(mfrow=c(1,1)) # Reset to default plot settings

#Part 2: K-medoids (PAM)

#1. create k-medoids model using optimal k value of 3, and euclidian distance measure

pam_3 <- pam(df2, 3, FALSE, "euclidean")

#use fviz_cluster to visualize the results of the clustering
fviz_cluster(pam_3, data = df2, repel = TRUE, show.clust.cent = FALSE)

#2. Generate boxplots for attr_date, since_date, intel_date, amb_date, shared_date
par(mfrow-c(1,5))
for (i in 18:22){
  boxplot(df[[i]] ~ pam_3$cluster, xlab="cluster", main=names(df)[i])
}
par(mfrow=c(1,1)) # Reset to default plot settings

#Part 3: Hierarchial Clustering 

#1 use hclust to create hierarchial clustering

hc <- hclust(dist(df2), method="complete")

#Use cutree to split tree into k = 3. 
hc$cluster <- cutree(hc, k = 3)

#Use fviz_cluster to plot results 
fviz_cluster(list(data = df2, cluster = hc$cluster))

#3 calcuate average silhouette metric across hierarchial clustering. 

hc_stats <- cluster.stats(dist(df2),  hc$cluster, silhouette = TRUE)
hc_stats$avg.silwidth

#calcuate average silhouette metric across k-medoids.
pam3_stats <- cluster.stats(dist(df2),  pam_3$cluster, silhouette = TRUE)
pam3_stats$avg.silwidth

#calcuate average silhouette metric across k-means. 
km3_stats <- cluster.stats(dist(df2),  km_3$cluster, silhouette = TRUE)
km3_stats$avg.silwidth 

