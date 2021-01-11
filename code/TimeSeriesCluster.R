##Installing required packages and load required libraries
#---
#install.packages("ggmap")
library(dtwclust,warn.conflicts = FALSE)
library(scales,warn.conflicts = FALSE)
library("ggmap",warn.conflicts = FALSE)
library(maptools,warn.conflicts = FALSE)
library(maps,warn.conflicts = FALSE)
#---

##Importing Data from source(updated daily)
#---
Main <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series"

confirmed_Path <-  file.path(Main,"time_series_covid19_confirmed_global.csv")
Deaths_Path <- file.path(Main,"time_series_covid19_deaths_global.csv")
Recovered_Path <- file.path(Main,"time_series_covid19_recovered_global.csv")

#Read data from stored links:
ConfirmedData <- read.csv(confirmed_Path,stringsAsFactors = FALSE)
ConfirmedData<-as.data.frame(ConfirmedData)
DeathData<- read.csv(Deaths_Path,stringsAsFactors = FALSE)
DeathData<-as.data.frame(DeathData)
RecoveredData <- read.csv(Recovered_Path,stringsAsFactors = FALSE)
RecoveredData<-as.data.frame(RecoveredData)
#---

##Data Proprocessing
#---
#Change the data from accumulated cases into daily change cases
for(i in 1:nrow(ConfirmedData)){
  ConfirmedData[i,6:ncol(ConfirmedData)]<-diff(as.numeric(ConfirmedData[i,5:ncol(ConfirmedData)]),1)
  #plot(as.numeric(ConfirmedData[1,3:ncol(ConfirmedData)]),type='l')
}

for(i in 1:nrow(DeathData)){
  DeathData[i,6:ncol(DeathData)]<-diff(as.numeric(DeathData[i,5:ncol(DeathData)]),1)
  #plot(as.numeric(DeathData[1,3:ncol(DeathData)]),type='l')
}

for(i in 1:nrow(RecoveredData)){
  RecoveredData[i,6:ncol(RecoveredData)]<-diff(as.numeric(RecoveredData[i,5:ncol(RecoveredData)]),1)
  #plot(as.numeric(RecoveredData[1,3:ncol(RecoveredData)]),type='l')
}

#Check if there is any NAs
#table(unique(is.na(ConfirmedData)))
#table(unique(is.na(DeathData)))
#table(unique(is.na(RecoveredData)))

#Found that 43th,53th row of ConfirmedData and Death Data missed the value for Lat and Long, and the number of cases of them are really small(1,1,13)
#table(unique(is.na(ConfirmedData)))
#table(unique(is.na(DeathData)))

#But in the Recovered Data, there are Lat and Long for Canada, so take them to replace the missing value in ConfirmedData and Death Data
ConfirmedData[c(43,53),3]<-RecoveredData[40,3]
ConfirmedData[c(43,53),4]<-RecoveredData[40,4]
DeathData[c(43,53),3]<-RecoveredData[40,3]
DeathData[c(43,53),4]<-RecoveredData[40,4]

#Found that 42th row of ConfirmedData and Death Data are strange, and required case is only 1
#as.numeric(ConfirmedData[42,5:ncol(ConfirmedData)])
#as.numeric(DeathData[42,5:ncol(ConfirmedData)])
#Remove 42th row of ConfirmedData and Death Data
ConfirmedData<-ConfirmedData[-42,]
DeathData<-DeathData[-42,]

#Check again
#table(unique(is.na(ConfirmedData)))
#table(unique(is.na(DeathData)))
#table(unique(is.na(RecoveredData)))

#As some of the 1st column are empty, we insert the 2nd column into 1st column as the Province.State of that row
for(j in 1:nrow(ConfirmedData)){
  if(ConfirmedData[j,1]==""){
    ConfirmedData[j,1]<-ConfirmedData[j,2]
  }
}

for(j in 1:nrow(DeathData)){
  if(DeathData[j,1]==""){
    DeathData[j,1]<-DeathData[j,2]
  }
}

for(j in 1:nrow(RecoveredData)){
  if(RecoveredData[j,1]==""){
    RecoveredData[j,1]<-RecoveredData[j,2]
  }
}

#Check again
#ConfirmedData[,1]==""
#---

##Time Series Clustering for Confirmed Cases
#---
#Hierarchical clustering of Time Series Clustering for Confirmed Cases
hc_dtw_Confirmed <- tsclust(as.ts(ConfirmedData[,5:ncol(ConfirmedData)]),type = "h",k = 20L,preproc = zscore, seed = 899,distance = "dtw_basic", centroid = shape_extraction,control = hierarchical_control(method = "complete"))

#From the dendrogram, we can basically divide them into 6 clusters
plot(hc_dtw_Confirmed)

#Average intra-cluster distance
tsclust(as.ts(ConfirmedData[,5:ncol(ConfirmedData)]), type = "h", k = 6L,
        preproc = zscore,
        seed = 899,
        distance = "dtw_basic",
        centroid = shape_extraction,
        control = hierarchical_control(method = "complete"), #complete = maximal intercluster dissimilarity
        args = tsclust_args(dist = list(window.size = 7L))) #for every 7 days

hc_dtw_Confirmed <- tsclust(as.ts(ConfirmedData[,5:ncol(ConfirmedData)]),type = "h",k = 6,preproc = zscore, seed = 899,distance = "dtw_basic", centroid = shape_extraction,control = hierarchical_control(method = "complete"))
#Centroids Series Plot of Hierarchical clustering for Confirmed Cases
plot(hc_dtw_Confirmed, type = "centroids")
#plot(hc_dtw_Confirmed, type = "series", clus = 1L) #clus=1L : plot for the 1st cluster
#plot(hc_dtw_Confirmed, type = "series", clus = 2L)
#plot(hc_dtw_Confirmed, type = "series", clus = 3L)
#plot(hc_dtw_Confirmed, type = "series", clus = 4L)
#plot(hc_dtw_Confirmed, type = "series", clus = 5L)
#plot(hc_dtw_Confirmed, type = "series", clus = 6L)

#print clusters from Hierarchical clustering
print_clusters_Confirmed <- function(labels, k) {
  for(i in 1:k) {
    print(paste("cluster", i))
    print(ConfirmedData[labels==i,"Province.State"])
  }
}
groups_Confirmed <- cutree(hc_dtw_Confirmed, k=6)
#print_clusters_Confirmed(groups_Confirmed, 6)

lat_Confirmed_1=c()
long_Confirmed_1=c()
Confirmed_1<-ConfirmedData[groups_Confirmed==1,"Province.State"]
for(i in 1:nrow(ConfirmedData)){
  if(any(unique(ConfirmedData[i,1]==ConfirmedData[groups_Confirmed==1,"Province.State"])==TRUE)){
    lat_Confirmed_1[i]<-ConfirmedData[i,3]
    long_Confirmed_1[i]<-ConfirmedData[i,4]
  }
}
lat_Confirmed_1<-lat_Confirmed_1[-which(is.na(lat_Confirmed_1))]
long_Confirmed_1<-long_Confirmed_1[-which(is.na(long_Confirmed_1))]

lat_Confirmed_2=c()
long_Confirmed_2=c()
Confirmed_2<-ConfirmedData[groups_Confirmed==2,"Province.State"]
for(i in 1:nrow(ConfirmedData)){
  if(any(unique(ConfirmedData[i,1]==ConfirmedData[groups_Confirmed==2,"Province.State"])==TRUE)){
    lat_Confirmed_2[i]<-ConfirmedData[i,3]
    long_Confirmed_2[i]<-ConfirmedData[i,4]
  }
}
lat_Confirmed_2<-lat_Confirmed_2[-which(is.na(lat_Confirmed_2))]
long_Confirmed_2<-long_Confirmed_2[-which(is.na(long_Confirmed_2))]

lat_Confirmed_3=c()
long_Confirmed_3=c()
Confirmed_3<-ConfirmedData[groups_Confirmed==3,"Province.State"]
for(i in 1:nrow(ConfirmedData)){
  if(any(unique(ConfirmedData[i,1]==ConfirmedData[groups_Confirmed==3,"Province.State"])==TRUE)){
    lat_Confirmed_3[i]<-ConfirmedData[i,3]
    long_Confirmed_3[i]<-ConfirmedData[i,4]
  }
}
lat_Confirmed_3<-lat_Confirmed_3[-which(is.na(lat_Confirmed_3))]
long_Confirmed_3<-long_Confirmed_3[-which(is.na(long_Confirmed_3))]

lat_Confirmed_4=c()
long_Confirmed_4=c()
Confirmed_4<-ConfirmedData[groups_Confirmed==4,"Province.State"]
for(i in 1:nrow(ConfirmedData)){
  if(any(unique(ConfirmedData[i,1]==ConfirmedData[groups_Confirmed==4,"Province.State"])==TRUE)){
    lat_Confirmed_4[i]<-ConfirmedData[i,3]
    long_Confirmed_4[i]<-ConfirmedData[i,4]
  }
}
lat_Confirmed_4<-lat_Confirmed_4[-which(is.na(lat_Confirmed_4))]
long_Confirmed_4<-long_Confirmed_4[-which(is.na(long_Confirmed_4))]

lat_Confirmed_5=c()
long_Confirmed_5=c()
Confirmed_5<-ConfirmedData[groups_Confirmed==5,"Province.State"]
for(i in 1:nrow(ConfirmedData)){
  if(any(unique(ConfirmedData[i,1]==ConfirmedData[groups_Confirmed==5,"Province.State"])==TRUE)){
    lat_Confirmed_5[i]<-ConfirmedData[i,3]
    long_Confirmed_5[i]<-ConfirmedData[i,4]
  }
}
lat_Confirmed_5<-lat_Confirmed_5[-which(is.na(lat_Confirmed_5))]
long_Confirmed_5<-long_Confirmed_5[-which(is.na(long_Confirmed_5))]

lat_Confirmed_6=c()
long_Confirmed_6=c()
Confirmed_6<-ConfirmedData[groups_Confirmed==6,"Province.State"]
for(i in 1:nrow(ConfirmedData)){
  if(any(unique(ConfirmedData[i,1]==ConfirmedData[groups_Confirmed==6,"Province.State"])==TRUE)){
    lat_Confirmed_6[i]<-ConfirmedData[i,3]
    long_Confirmed_6[i]<-ConfirmedData[i,4]
  }
}
lat_Confirmed_6<-lat_Confirmed_6[-which(is.na(lat_Confirmed_6))]
long_Confirmed_6<-long_Confirmed_6[-which(is.na(long_Confirmed_6))]

#Plot the Hierarchical clustering Result of Confirmed Cases on World Map
map("world", fill=TRUE, col="white", bg="lightblue", ylim=c(-60, 90), mar=c(0,0,0,0))
points(long_Confirmed_1,lat_Confirmed_1, col="red", pch=16)
points(long_Confirmed_2,lat_Confirmed_2, col="blue", pch=16)
points(long_Confirmed_3,lat_Confirmed_3, col="green", pch=16)
points(long_Confirmed_4,lat_Confirmed_4, col="yellow", pch=16)
points(long_Confirmed_5,lat_Confirmed_5, col="pink", pch=16)
points(long_Confirmed_6,lat_Confirmed_6, col="purple", pch=16)

#PCA to visualize the cluster
library(ggplot2)
pca_Confirmed <- prcomp(ConfirmedData[,5:ncol(ConfirmedData)])
NumberofPC_Confirmed <- 6
Projection_Confirmed <- predict(pca_Confirmed, newdata=ConfirmedData[,5:ncol(ConfirmedData)])[,1:NumberofPC_Confirmed]
project.plus_Confirmed <- cbind(as.data.frame(Projection_Confirmed),cluster=as.factor(groups_Confirmed),state=ConfirmedData$Province.State)
ggplot(project.plus_Confirmed, aes(x=PC1, y=PC2))+geom_point(aes(shape=cluster))+geom_text(aes(label=state),hjust=0, vjust=1)
ggplot(project.plus_Confirmed, aes(x=PC3, y=PC4))+geom_point(aes(shape=cluster))+geom_text(aes(label=state),hjust=0, vjust=1)
ggplot(project.plus_Confirmed, aes(x=PC5, y=PC6))+geom_point(aes(shape=cluster))+geom_text(aes(label=state),hjust=0, vjust=1)

for(i in 1:6){
  write.csv(as.data.frame(ConfirmedData[groups_Confirmed==i,"Province.State"]),paste0("Confirmed_Cluster_",i,".csv"),quote=FALSE,row.names=TRUE)
}

#---

##Time Series Clustering for Death Cases
#---
hc_dtw_Death <- tsclust(as.ts(DeathData[,5:ncol(DeathData)]),type = "h",k = 10L,preproc = zscore, seed = 899,distance = "dtw_basic", centroid = shape_extraction,control = hierarchical_control(method = "complete"))

#From the dendrogram, we can basically divide them into 4 clusters
plot(hc_dtw_Death)

#Average intra-cluster distance
tsclust(as.ts(DeathData[,5:ncol(DeathData)]), type = "h", k = 4L,
        preproc = zscore,
        seed = 899,
        distance = "dtw_basic",
        centroid = shape_extraction,
        control = hierarchical_control(method = "complete"),
        args = tsclust_args(dist = list(window.size = 7L))) #]for every 7 days

hc_dtw_Death <- tsclust(as.ts(DeathData[,5:ncol(DeathData)]),type = "h",k = 4,preproc = zscore, seed = 899,distance = "dtw_basic", centroid = shape_extraction,control = hierarchical_control(method = "complete"))

#Centroids Series Plot of Hierarchical clustering for Confirmed Cases
plot(hc_dtw_Death, type = "centroids")
#plot(hc_dtw_Death, type = "series", clus = 1L)
#plot(hc_dtw_Death, type = "series", clus = 2L)
#plot(hc_dtw_Death, type = "series", clus = 3L)
#plot(hc_dtw_Death, type = "series", clus = 4L)

print_clusters_Death <- function(labels, k) {
  for(i in 1:k) {
    print(paste("cluster", i))
    print(DeathData[labels==i,"Province.State"])
  }
}
groups_Death <- cutree(hc_dtw_Death, k=4)
#print_clusters_Death(groups_Death, 4)

lat_Death_1=c()
long_Death_1=c()
Death_1<-DeathData[groups_Death==1,"Province.State"]
for(i in 1:nrow(DeathData)){
  if(any(unique(DeathData[i,1]==DeathData[groups_Death==1,"Province.State"])==TRUE)){
    lat_Death_1[i]<-DeathData[i,3]
    long_Death_1[i]<-DeathData[i,4]
  }
}
lat_Death_1<-lat_Death_1[-which(is.na(lat_Death_1))]
long_Death_1<-long_Death_1[-which(is.na(long_Death_1))]

lat_Death_2=c()
long_Death_2=c()
Death_2<-DeathData[groups_Death==2,"Province.State"]
for(i in 1:nrow(DeathData)){
  if(any(unique(DeathData[i,1]==DeathData[groups_Death==2,"Province.State"])==TRUE)){
    lat_Death_2[i]<-DeathData[i,3]
    long_Death_2[i]<-DeathData[i,4]
  }
}
lat_Death_2<-lat_Death_2[-which(is.na(lat_Death_2))]
long_Death_2<-long_Death_2[-which(is.na(long_Death_2))]

lat_Death_3=c()
long_Death_3=c()
Death_3<-DeathData[groups_Death==3,"Province.State"]
for(i in 1:nrow(DeathData)){
  if(any(unique(DeathData[i,1]==DeathData[groups_Death==3,"Province.State"])==TRUE)){
    lat_Death_3[i]<-DeathData[i,3]
    long_Death_3[i]<-DeathData[i,4]
  }
}
lat_Death_3<-lat_Death_3[-which(is.na(lat_Death_3))]
long_Death_3<-long_Death_3[-which(is.na(long_Death_3))]

lat_Death_4=c()
long_Death_4=c()
Death_4<-DeathData[groups_Death==4,"Province.State"]
for(i in 1:nrow(DeathData)){
  if(any(unique(DeathData[i,1]==DeathData[groups_Death==4,"Province.State"])==TRUE)){
    lat_Death_4[i]<-DeathData[i,3]
    long_Death_4[i]<-DeathData[i,4]
  }
}
lat_Death_4<-lat_Death_4[-which(is.na(lat_Death_4))]
long_Death_4<-long_Death_4[-which(is.na(long_Death_4))]

#Plot the Hierarchical clustering Result of Death Cases on World Map
map("world", fill=TRUE, col="white", bg="lightblue", ylim=c(-60, 90), mar=c(0,0,0,0))
points(long_Death_1,lat_Death_1, col="red", pch=16)
points(long_Death_2,lat_Death_2, col="blue", pch=16)
points(long_Death_3,lat_Death_3, col="green", pch=16)
points(long_Death_4,lat_Death_4, col="yellow", pch=16)

#PCA to visualize the cluster
library(ggplot2)
pca_Death <- prcomp(DeathData[,5:ncol(DeathData)])
NumberofPC_Death <- 4
Projection_Death <- predict(pca_Death, newdata=DeathData[,5:ncol(DeathData)])[,1:NumberofPC_Death]
project.plus_Death <- cbind(as.data.frame(Projection_Death),cluster=as.factor(groups_Death),state=DeathData$Province.State)
ggplot(project.plus_Death, aes(x=PC1, y=PC2))+geom_point(aes(shape=cluster))+geom_text(aes(label=state),hjust=0, vjust=1)
ggplot(project.plus_Death, aes(x=PC3, y=PC4))+geom_point(aes(shape=cluster))+geom_text(aes(label=state),hjust=0, vjust=1)

for(i in 1:4){
  write.csv(as.data.frame(DeathData[groups_Death==i,"Province.State"]),paste0("Death_Cluster_",i,".csv"),quote=FALSE,row.names=TRUE)
}
#---

##Recovered Cases
#Hierarchical clustering for the time series of Recovered Cases
hc_dtw_Recovered <- tsclust(as.ts(RecoveredData[,5:ncol(RecoveredData)]),type = "h",k = 10L,preproc = zscore, seed = 899,distance = "dtw_basic", centroid = shape_extraction,control = hierarchical_control(method = "complete"))

#From the dendrogram, we can basically divide them into 6 clusters
plot(hc_dtw_Recovered)

#Average intra-cluster distance
tsclust(as.ts(RecoveredData[,5:ncol(RecoveredData)]), type = "h", k = 6L,
        preproc = zscore,
        seed = 899,
        distance = "dtw_basic",
        centroid = shape_extraction,
        control = hierarchical_control(method = "complete"), #complete = maximal intercluster dissimilarity
        args = tsclust_args(dist = list(window.size = 7L))) #for every 7 days

hc_dtw_Recovered <- tsclust(as.ts(RecoveredData[,5:ncol(RecoveredData)]),type = "h",k = 6,preproc = zscore, seed = 899,distance = "dtw_basic", centroid = shape_extraction,control = hierarchical_control(method = "complete"))
#Centroids Series Plot of Hierarchical clustering for Recovered Cases
plot(hc_dtw_Recovered, type = "centroids")
#plot(hc_dtw_Recovered, type = "series", clus = 1L)
#plot(hc_dtw_Recovered, type = "series", clus = 2L)
#plot(hc_dtw_Recovered, type = "series", clus = 3L)
#plot(hc_dtw_Recovered, type = "series", clus = 4L)
#plot(hc_dtw_Recovered, type = "series", clus = 5L)
#plot(hc_dtw_Recovered, type = "series", clus = 6L)

#From the dendrogram, we can basically divide them into 5 clusters
#print clusters from Hierarchical clustering
print_clusters_Recovered <- function(labels, k) {
  for(i in 1:k) {
    print(paste("cluster", i))
    print(RecoveredData[labels==i,"Province.State"])
  }
}
groups_Recovered <- cutree(hc_dtw_Recovered, k=6)
#print_clusters_Recovered(groups_Recovered, 6)

lat_Recovered_1=c()
long_Recovered_1=c()
Recovered_1<-RecoveredData[groups_Recovered==1,"Province.State"]
for(i in 1:nrow(RecoveredData)){
  if(any(unique(RecoveredData[i,1]==RecoveredData[groups_Recovered==1,"Province.State"])==TRUE)){
    lat_Recovered_1[i]<-RecoveredData[i,3]
    long_Recovered_1[i]<-RecoveredData[i,4]
  }
}
lat_Recovered_1<-lat_Recovered_1[-which(is.na(lat_Recovered_1))]
long_Recovered_1<-long_Recovered_1[-which(is.na(long_Recovered_1))]

lat_Recovered_2=c()
long_Recovered_2=c()
Recovered_2<-RecoveredData[groups_Recovered==2,"Province.State"]
for(i in 1:nrow(RecoveredData)){
  if(any(unique(RecoveredData[i,1]==RecoveredData[groups_Recovered==2,"Province.State"])==TRUE)){
    lat_Recovered_2[i]<-RecoveredData[i,3]
    long_Recovered_2[i]<-RecoveredData[i,4]
  }
}
lat_Recovered_2<-lat_Recovered_2[-which(is.na(lat_Recovered_2))]
long_Recovered_2<-long_Recovered_2[-which(is.na(long_Recovered_2))]

lat_Recovered_3=c()
long_Recovered_3=c()
Recovered_3<-RecoveredData[groups_Recovered==3,"Province.State"]
for(i in 1:nrow(RecoveredData)){
  if(any(unique(RecoveredData[i,1]==RecoveredData[groups_Recovered==3,"Province.State"])==TRUE)){
    lat_Recovered_3[i]<-RecoveredData[i,3]
    long_Recovered_3[i]<-RecoveredData[i,4]
  }
}
lat_Recovered_3<-lat_Recovered_3[-which(is.na(lat_Recovered_3))]
long_Recovered_3<-long_Recovered_3[-which(is.na(long_Recovered_3))]

lat_Recovered_4=c()
long_Recovered_4=c()
Recovered_4<-RecoveredData[groups_Recovered==4,"Province.State"]
for(i in 1:nrow(RecoveredData)){
  if(any(unique(RecoveredData[i,1]==RecoveredData[groups_Recovered==4,"Province.State"])==TRUE)){
    lat_Recovered_4[i]<-RecoveredData[i,3]
    long_Recovered_4[i]<-RecoveredData[i,4]
  }
}
lat_Recovered_4<-lat_Recovered_4[-which(is.na(lat_Recovered_4))]
long_Recovered_4<-long_Recovered_4[-which(is.na(long_Recovered_4))]

lat_Recovered_5=c()
long_Recovered_5=c()
Recovered_5<-RecoveredData[groups_Recovered==5,"Province.State"]
for(i in 1:nrow(RecoveredData)){
  if(any(unique(RecoveredData[i,1]==RecoveredData[groups_Recovered==5,"Province.State"])==TRUE)){
    lat_Recovered_5[i]<-RecoveredData[i,3]
    long_Recovered_5[i]<-RecoveredData[i,4]
  }
}
lat_Recovered_5<-lat_Recovered_5[-which(is.na(lat_Recovered_5))]
long_Recovered_5<-long_Recovered_5[-which(is.na(long_Recovered_5))]

lat_Recovered_6=c()
long_Recovered_6=c()
Recovered_6<-RecoveredData[groups_Recovered==6,"Province.State"]
for(i in 1:nrow(RecoveredData)){
  if(any(unique(RecoveredData[i,1]==RecoveredData[groups_Recovered==6,"Province.State"])==TRUE)){
    lat_Recovered_6[i]<-RecoveredData[i,3]
    long_Recovered_6[i]<-RecoveredData[i,4]
  }
}
lat_Recovered_6<-lat_Recovered_6[-which(is.na(lat_Recovered_6))]
long_Recovered_6<-long_Recovered_6[-which(is.na(long_Recovered_6))]

#Plot the Hierarchical clustering Result of Recovered Cases on World Map
map("world", fill=TRUE, col="white", bg="lightblue", ylim=c(-60, 90), mar=c(0,0,0,0))
points(long_Recovered_1,lat_Recovered_1, col="red", pch=16)
points(long_Recovered_2,lat_Recovered_2, col="blue", pch=16)
points(long_Recovered_3,lat_Recovered_3, col="green", pch=16)
points(long_Recovered_4,lat_Recovered_4, col="yellow", pch=16)
points(long_Recovered_5,lat_Recovered_5, col="orange", pch=16)
points(long_Recovered_6,lat_Recovered_6, col="purple", pch=16)

#PCA to visualize the cluster
library(ggplot2)
pca_Recovered <- prcomp(RecoveredData[,5:ncol(RecoveredData)])
NumberofPC_Recovered <- 6
Projection_Recovered <- predict(pca_Recovered, newdata=RecoveredData[,5:ncol(RecoveredData)])[,1:NumberofPC_Recovered]
project.plus_Recovered <- cbind(as.data.frame(Projection_Recovered),cluster=as.factor(groups_Recovered),state=RecoveredData$Province.State)
ggplot(project.plus_Recovered, aes(x=PC1, y=PC2))+geom_point(aes(shape=cluster))+geom_text(aes(label=state),hjust=0, vjust=1)
ggplot(project.plus_Recovered, aes(x=PC3, y=PC4))+geom_point(aes(shape=cluster))+geom_text(aes(label=state),hjust=0, vjust=1)
ggplot(project.plus_Recovered, aes(x=PC5, y=PC6))+geom_point(aes(shape=cluster))+geom_text(aes(label=state),hjust=0, vjust=1)

for(i in 1:6){
  write.csv(as.data.frame(RecoveredData[groups_Recovered==i,"Province.State"]),paste0("Recovered_Cluster_",i,".csv"),quote=FALSE,row.names=TRUE)
}
#---

#References:
#https://cran.r-project.org/web/packages/dtwclust/dtwclust.pdf
#https://www.rdocumentation.org/packages/dtwclust/versions/3.1.1/topics/tsclust
#https://www.r-bloggers.com/2013/04/r-beginners-plotting-locations-on-to-a-world-map/
#Codes Examples From This course

