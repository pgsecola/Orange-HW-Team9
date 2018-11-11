library(dplyr)
library(bindr)
library(ggplot2)
library(histogram)
library(stringr)
library(tidyverse)
library(tidyr)
library(readxl)
library(tibble)
library(reshape2)
library(data.table)
library(text2vec)
library(tidytext)
library(wordcloud)
library(SnowballC)
library(ggmap)
library(factoextra)
library(mclust)
listings <- read.csv("/Users/peixingli/Desktop/Clustering/boston-airbnb-open-data (1)/listings.csv",stringsAsFactors = FALSE)
calendars <- read.csv("/Users/peixingli/Desktop/Clustering/boston-airbnb-open-data (1)/calendar.csv",stringsAsFactors = FALSE)
reviews<-read.csv("/Users/peixingli/Desktop/Clustering/boston-airbnb-open-data (1)/reviews.csv",stringsAsFactors = FALSE)
#length(unique(listings$transit))  #transit is useful because we can know how far away to attractive location
#max review number is 19.15
#max(listings$reviews_per_month,na.rm=TRUE)
#min(listings$calculated_host_listings_count)
#There are 25 neighborhoods
neighbohoods<-unique(listings$neighbourhood_cleansed)
#There are three different room type entire home/apt,private room,shared room
#print(unique(listings$room_type))
#Filter listings that do not have price
listings$price=as.numeric(gsub('\\$','',listings$price))
listings<-filter(listings,!is.na(listings$price))
listings<-filter(listings,!is.na(listings$beds))


#Only keep variables that are useful
listings_imprtnt <- select(listings, c("id","neighbourhood_cleansed","latitude","longitude",
                                       "property_type", "room_type","price", "weekly_price", "monthly_price",
                                       "transit","review_scores_rating","review_scores_location","reviews_per_month","zipcode","beds","street"))

#and extract those listings that have review_scores_rating
listings_imprtnt<-filter(listings_imprtnt,!is.na(listings_imprtnt$review_scores_rating))
listings_imprtnt$price_per_bed=listings_imprtnt$price/listings_imprtnt$beds
#qplot(listings_imprtnt$price_per_bed,geom='histogram',main='per bed Price of all listings')
#Standardize Clustering input variables
listings_imprtnt$std.lat <- scale(listings_imprtnt$latitude)
listings_imprtnt$std.lon <- scale(listings_imprtnt$longitude)
listings_imprtnt$std.score <- scale(listings_imprtnt$review_scores_rating)
listings_imprtnt$std.price_per_bed<-scale(listings_imprtnt$review_scores_rating)

toC<- select(listings_imprtnt,c("std.lat","std.lon","std.score","std.price_per_bed"))
toC<- select(listings_imprtnt,c("std.lat","std.lon","std.price_per_bed"))
#####KMeans Clustering##########
#use toC matrix to determine the appropiate number of clusters

fviz_nbclust(toC, kmeans, method="wss") 
fviz_nbclust(toC, kmeans, method="silhouette") 
fviz_nbclust(toC, kmeans, method="gap") 

#perform KMeans analysis using optimal number of clusters (5)
km_clust <- kmeans(toC,5,nstart = 5)
km_clust2 <- kmeans(toC,10,nstart = 25)
km_clust$centers
#create Data Frame with location data and cluster information
Boston_cluster_table <- cbind.data.frame(listings_imprtnt,km_clust$cluster)
names(Boston_cluster_table)[22] <- "Cluster_ID" 
##### Hierarchicalh Clustering##########
km_clust23 <- hmeans(toC,10,nstart = 25)
#####################################################
#Geocode Location
#####################################################
library(devtools)
devtools::install_github("dkahle/ggmap", ref = "tidyup")
#####################################################
#TO DO THIS YOU NEED TO REGISTER TO A GOOGLE DEV ACCOUNT
#AND PROVIDE YOUR OWN API KEY!!!!!
#THEN YOU CAN GET A PRETTY MAP!
# https://www.wpgmaps.com/documentation/creating-a-google-maps-api-key/
register_google("AIzaSyBfqoxdvkwfRQnbhUEXchBzi6BXLoUSp-A")
map <- get_map(location ="Allston", zoom = 11)

ggmap(map, fullpage = TRUE) +
  geom_point(data = Allston_cluster_table, aes(x =longitude, y = latitude), color =Allston_cluster_table$Cluster_ID, size = 2)
#By neighbourhood
print(unique(Boston_cluster_table$neighbourhood_cleansed))
#####################################################
#Cluster Analysis
#####################################################
# By neighbourhood
#Allston 
listings_Allston=filter(listings_imprtnt,listings_imprtnt$neighbourhood_cleansed=='Allston')
listings_Allston<-filter(listings_Allston,!is.na(listings_Allston$review_scores_rating))
listings_Allston$price_per_bed=listings_Allston$price/listings_Allston$beds  
listings_Allston$std.lat <- scale(listings_Allston$latitude)
listings_Allston$std.lon <- scale(listings_Allston$longitude)
listings_Allston$std.score <- scale(listings_Allston$review_scores_rating)
listings_Allston$std.price_per_bed<-scale(listings_Allston$review_scores_rating)
toC<- select(listings_Allston,c("std.lat","std.lon","std.score","std.price_per_bed"))
km_clust <- kmeans(toC,4,nstart = 5)
fviz_nbclust(toC, kmeans, method="wss") 
km_clust$centers
Allston_cluster_table <- cbind.data.frame(listings_Allston,km_clust$cluster)
names(Allston_cluster_table)[22] <- "Cluster_ID
