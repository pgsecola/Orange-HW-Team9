

library(TTR)
library(tidyverse)
library(factoextra)
library(knitr)

data=final_data[,2:65]
head(data)
#covert all the variables to numeric in order to perform PCA
data$AGE <- as.numeric(data$AGE)
data$EVER_SMOKE <- as.numeric(data$EVER_SMOKE)
data$ASTHMA <- as.numeric(data$ASTHMA)
data$POVERTY_RATIO <- as.numeric(data$POVERTY_RATIO)
#principal component analysis (scale, rank) 
#I am not sure if we should do use only 5 pca to do clustering or all 64 pcas, I am using all 64
pca_scale_5=prcomp(data, rank.=5,scale=T)
pca_scale=prcomp(data,scale=T)
#output the standard deviation for the five principle componetns
summary(pca_scale_5)
#assign the score on new components to perform clustering 
pca_scale$rotation
tail(pca_scale$x)
cdata=as.data.frame(pca_scale$x)
cdata_5=as.data.frame(pca_scale_5$x)
#plots look at magnitudes of eigenvalues, the stdev of the pca and the score on main principal compoents.
plot(pca_scale)
plot(pca_scale$sdev)
plot(pca_scale$x)
#############################Kmeans cluster##########################
set.seed(12345)
# Visualize the find the optimum k clusters
# wss: optimum 4 silhouete : 2
fviz_nbclust(cdata, kmeans, method = "wss")
fviz_nbclust(cdata, kmeans, method = "silhouette")
#cluster 
kclust <- kmeans(cdata,4,nstart=25)
plot(kclust$centers)
#combine cluster number with the original data
cluster_table <- cbind.data.frame(data,kclust$cluster)
colnames(cluster_table)[colnames(cluster_table)=="kclust$cluster"] <- "Cluster_ID"
#summarize table
colnames(cluster_table[-1])
cluster_sum=cluster_table %>%
  group_by(Cluster_ID)%>%
  summarize(age_m = mean(AGE, na.rm=TRUE))


