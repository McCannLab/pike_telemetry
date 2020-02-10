library(maptools)
library(spatstat)
library(rgdal)
library(SDMTools)
library(raster)
library(tidyverse)

#read in telemetry data
dat <- read.csv("data/alexie_pike_telemetry_data_raw.csv")

#convert lat long to utm
dat$utm <- project(cbind(c(as.numeric(dat$lon)), c(as.numeric(dat$lat))), "+proj=utm +zone=11 ellps=WGS84")

#read in shoreline
shore <- readOGR("data/Alexie_Outline_Islands.shp")
shore <- spTransform(shore, CRS("+proj=utm +zone=11 +ellps=WGS84"))

#remove points that fall outside of lake
W <- as.owin(shore) #create bound/window
isin <- inside.owin(x=dat$utm[,1], y=dat$utm[,2], w=W)
vps_in <- dat[isin,] #retain only those inside lake
vps_out <- dat[!isin,] #keep those outside lake for later

data <- vps_in
total_vps <- nrow(dat)
outside_lake <- nrow(vps_in)
rm(vps_in, dat, dat1, vps_out)

#read in bathymetry map
bathy<-read.asc("data/bathy.asc")
bathy<-raster.from.asc(bathy)

#add bathy depth to each position
data<-data.frame(cbind(data,extract(bathy, data$utm)))
colnames(data)[10]<-"bathy_depth"
data$bathy_depth<-data$bathy_depth*-1
data$depth<-round(data$depth, 1)
data$under<-data$bathy_depth-data$depth

#check if any above or below lake
dat<-dat[!is.na(dat$depth),]
dat[dat$depth<0, names(dat)=="depth"]<-0
length(which(dat$depth<0))

dat<-dat[!dat$depth>33.7,]
dat[dat$depth>32, names(dat)=="depth"]<-32

hist(dat$depth, col="grey")

#remove points that fall outside of lake
W<-as.owin(shore) #create bound/window
isin<-inside.owin(x=dat$utm[,1], y=dat$utm[,2], w=W)
vps_in<-dat[isin,] #retain only those inside lake
vps_out<-dat[!isin,] #keep those outside lake for later

data<-vps_in
total_vps<-nrow(dat)
outside_lake<-nrow(vps_in)
rm(vps_in, dat, dat1, vps_out, md)

#add bathy depth to each position
data<-data.frame(cbind(data,extract(bathy, data$utm)))
colnames(data)[9]<-"bathy_depth"
data$bathy_depth<-data$bathy_depth*-1
data$depth<-round(data$depth, 1)
data$under<-data$bathy_depth-data$depth

#add temp to each position
temp<-read.csv("temp_string_32_interp.csv", header=T)
temp$temp<-round(temp$temp, 1)

data<-merge(data, temp, by=c("date", "depth"))

#remove all data from first 2 weeks after tagging
data<-data[!data$date<as.Date("2012-06-29"),]

min(data$date)
max(data$date)

#filter by varying HPE
data$date<-as.Date(as.character(data$date))
data$period<-NA
data[data$date>=as.Date("2012-06-28") & data$date<=as.Date("2012-09-11"), names(data)=="period"]<-1
data[data$date>=as.Date("2012-09-12") & data$date<=as.Date("2012-10-27"), names(data)=="period"]<-2
data[data$date>=as.Date("2012-10-28") & data$date<=as.Date("2013-05-25"), names(data)=="period"]<-3
data[data$date>=as.Date("2013-05-26") & data$date<=as.Date("2013-09-24"), names(data)=="period"]<-4
data[data$date>=as.Date("2013-09-25") & data$date<=as.Date("2013-11-01"), names(data)=="period"]<-5
data[data$date>=as.Date("2013-11-02") & data$date<=as.Date("2014-06-11"), names(data)=="period"]<-6
data[data$date>=as.Date("2014-06-12"), names(data)=="period"]<-7

data$hpe<-as.numeric(data$hpe)

data1<-data[data$period==1 & data$hpe<35.14,]
data2<-data[data$period==2 & data$hpe<34.90,]
data3<-data[data$period==3 & data$hpe<115.28,]
data4<-data[data$period==4 & data$hpe<52.67,]
data5<-data[data$period==5 & data$hpe<12.63,]
data6<-data[data$period==6 & data$hpe<30.55,]
data7<-data[data$period==7 & data$hpe<56.71,]

data1a<-data[data$period==1 & data$hpe>=35.14,]
data2a<-data[data$period==2 & data$hpe>=34.90,]
data3a<-data[data$period==3 & data$hpe>=115.28,]
data4a<-data[data$period==4 & data$hpe>=52.67,]
data5a<-data[data$period==5 & data$hpe>=12.63,]
data6a<-data[data$period==6 & data$hpe>=30.55,]
data7a<-data[data$period==7 & data$hpe>=56.71,]

dataa<-rbind(data1, data2, data3, data4, data5, data6, data7)
data_bad<-rbind(data1a, data2a, data3a, data4a, data5a, data6a, data7a)

hist(data$bathy_depth, col="grey", freq=F, xlab="bathymetry depth (m)", main="", breaks=35)
hist(dataa$bathy_depth, col="blue", freq=F, xlab="bathymetry depth (m)", main="", breaks=35)
hist(data_bad$bathy_depth, col="red", freq=F, xlab="bathymetry depth (m)", main="", breaks=35)

#retains 88%
length(dataa$date)/length(data$date)
length(data$date)-length(dataa$date)

min(dataa$date)
max(dataa$date)

rm(data1a, data2a, data3a, data4a, data5a, data6a, data7a)
