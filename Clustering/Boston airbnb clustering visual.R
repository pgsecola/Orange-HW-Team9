#####################################################
#Geocode Location
#####################################################
# Need experimental version of ggmap
# also need to download and install rtools off of the internet
library(devtools)
devtools::install_github("dkahle/ggmap", ref = "tidyup")
#####################################################
library(readr)
library(lubridate)
library(ggplot2)
library(ggmap)
library(dplyr)
library(data.table)
library(ggrepel)

register_google("AIzaSyBfqoxdvkwfRQnbhUEXchBzi6BXLoUSp-A")
attraction_table=data.frame(
  attractions=c("Fenway Park",
                "Freedom Trail",
                "Boston Common",
                "North End",
                "Museum of Fine Arts",
                "New England Aquarian",
                "Museum of Science",
                "USS Constitution",
                "Boston Tea Party Ship and Museum",
                "Bunker Hill Monument",
                "Harvard University",
                "JFk Museum and library",
                "Boston Logan International Airport",
  ),
  lat<-c(42.346268,
         42.3559735761,
         42.3550,
         42.3647,
         42.3394,
         42.3591,
         42.2244,
         42.3710,
         42.3536,
         42.3764,
         42.3713,
         42.3095,
         42.3656,
         42.314138
  ),
  lon<-c(-71.095764,
         -71.0540531171,
         -71.0655,
         -71.0540,
         -71.0940,
         -71.0498,
         -71.4160,
         -71.0532,
         -71.0524,
         -71.0608,
         -71.1168,
         -71.0338,
         -71.0096,
         -71.102888
  )
)
names(attraction_table)[-1] <- c("lat","lon")
#create a table with just the cluster we are pitching
cluster1_table=select(Boston_cluster_table,c('latitude','longitude','neighbourhood_cleansed','km_clust$cluster'))%>%
  filter(Boston_cluster_table$`km_clust$cluster`==1)
cluster2_table=select(Boston_cluster_table,c('latitude','longitude','neighbourhood_cleansed','km_clust$cluster'))%>%
  filter(Boston_cluster_table$`km_clust$cluster`==2)
cluster3_table=select(Boston_cluster_table,c('latitude','longitude','neighbourhood_cleansed','km_clust$cluster'))%>%
  filter(Boston_cluster_table$`km_clust$cluster`==3)
cluster4_table=select(Boston_cluster_table,c('latitude','longitude','neighbourhood_cleansed','km_clust$cluster'))%>%
  filter(Boston_cluster_table$`km_clust$cluster`==4)
cluster5_table=select(Boston_cluster_table,c('latitude','longitude','neighbourhood_cleansed','km_clust$cluster'))%>%
  filter(Boston_cluster_table$`km_clust$cluster`==5)
cluster_Brighton=filter(Boston_cluster_table,Boston_cluster_table$neighbourhood_cleansed=='Brighton')
cluster_eastboston=filter(cluster5_table,cluster5_table$neighbourhood_cleansed=='East Boston')
cluster_fenway=filter(cluster5_table,cluster5_table$neighbourhood_cleansed=='Fenway')
map <- get_map(location ="Boston", zoom = 12)
ggmap(map, fullpage = TRUE) +
  geom_point(data = attraction_table, aes(x =lon, y = lat),shape=23,fill='red', size = 4)+
  geom_point(data = cluster1_table, aes(x =longitude, y = latitude),color='wheat3', size = 1)+
  geom_point(data = cluster2_table, aes(x =longitude, y = latitude),color='yellow', size = 1)+
  geom_point(data = cluster3_table, aes(x =longitude, y = latitude),color='purple', size = 1)+
  geom_point(data = cluster4_table, aes(x =longitude, y = latitude),color='blue', size = 1)+
  geom_point(data = cluster5_table, aes(x =longitude, y = latitude),color='coral3', size = 1)+
  ggtitle('Appendix.Boston Airbnb Boston Map')+
  theme(plot.title = element_text(hjust = 0.5,size=18),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))
