######Peixing: I was not sure what this part is doing????Wheeler Provided###########
library(splines)
times <- seq(1,295)/100 # Observations in 1/100th of a second
X <- bs(times,intercept=TRUE,df=60) #create a spline to 
#model the data
betas <- matrix(0,ncol=60,nrow = 6792)
###########################################################
# run a linear regression on each data set
# here I am manipulating my data you I can cluster
###########################################################
for (ii in 1:6792){
  temp <- lm(as.numeric(final_data[ii,6:300])~X-1) #-1 removes the natural intercept
  betas[ii,]  <- coefficients(temp)
}
cdata <- cbind(final_data[,1:5],betas)
#################################Starting from here I am running MCLUST############
#CONVERT EVERTYING TO 'numbers'
cdata$AGE <- as.numeric(cdata$AGE)
cdata$EVER_SMOKE <- as.numeric(cdata$EVER_SMOKE)
cdata$ASTHMA <- as.numeric(cdata$ASTHMA)
cdata$POVERTY_RATIO <- as.numeric(cdata$POVERTY_RATIO)
library(mclust)
set.seed(12345)
data_mclust=cdata[,c(10:20)]

clustBIC <-mclustBIC(data_mclust,modelName = "VVV",G=1:20)   # This is model selection
plot(clustBIC)
#Now using G = 6 and modelNames='VVV' and the same columns, provide a graph of each cluster's mean curve  
clust6    <- Mclust(data_mclust,G=6,modelNames = "VVV")
plot(clust6)
#add the cluster to original data 
data_mclust$class <- as.factor(clust6$classification)
data_mclust$SEQN=final_data$SEQN
data_mclust$AGE=final_data$AGE
data_mclust$EVER_SMOKE=as.numeric(final_data$EVER_SMOKE)
data_mclust$ASTHMA=as.numeric(final_data$ASTHMA)
data_mclust$POVERTY_RATIO=as.numeric(final_data$POVERTY_RATIO)
head(data_mclust)

for(i in 1:6792) {
  data_mclust$area[i] <-  integrate(splinefun(times,X%*%betas[i,]),min(times),max(times))$value
}

cluster_sum=data_mclust %>%
  group_by(class)%>%
  summarize(SEQN=mean(SEQN,na.rm=TRUE),
            Age_m = mean(AGE, na.rm=TRUE),
            EVER_SMOKE=mean(EVER_SMOKE,na.rm=TRUE),
            ASTHMA=mean(ASTHMA,na.rm=TRUE),
            POVERTY_RATIO=mean(POVERTY_RATIO,na.rm=TRUE),
            area=sum(area,na.rm=TRUE))
View(cluster_sum)

#Transform the data and graph the mean Spiro
cluster_table <- cbind.data.frame(cdata,clust6$classification)

clust_sum <- aggregate(cluster_table, by = list(cluster_table$`clust6$classification`), FUN = mean)

cluster_spiro <- t(clust_sum[1,7:66])
cluster_2 <- t(clust_sum[2,7:66])
cluster_3 <- t(clust_sum[3,7:66])
cluster_4 <- t(clust_sum[4,7:66])
cluster_5 <- t(clust_sum[5,7:66])
cluster_6<- t(clust_sum[6,7:66])

cluster_spiro <- rbind(cluster_spiro, cluster_2, cluster_3, cluster_4,cluster_5,cluster_6)

sequ <- NULL
sequ <- t(rbind(c(rep(1,60), rep(2,60), rep(3,60),rep(4,60),rep(5,60),rep(6,60))))

tm <- t(rbind(c(seq(1,60,1),seq(1,60,1),seq(1,60,1),seq(1,60,1),seq(1,60,1),seq(1,60,1))))

cluster_spiro <- as.data.frame(cbind(cluster_spiro, sequ, tm))

names(cluster_spiro)[1] <- "V1"

cluster_spiro$V2 <- as.factor(cluster_spiro$V2)

ggplot(data = cluster_spiro) +
  geom_line(mapping = aes(x = V3, y = V1, group = V2, color = V2))+
  ggtitle("Cluster mean spirometry") +
  xlab("time") + ylab("expiratory volumne beta")+
  theme(plot.title = element_text(hjust=0.5))


cluster_summary=cluster_spiro%>%
  group_by(V2)%>%
  summarize(Median=median(V1),
            Mean=mean(V1),
            Stdev=sd(V1),
            Min=min(V1),
            max=max(V1))
names(cluster_area_summary)[1]<-'Cluser_ID'
View(cluster_area_summary)
