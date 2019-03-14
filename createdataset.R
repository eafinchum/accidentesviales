#############################################################################
#############################################################################
##CREATING INCIDENT DATASET
##3/2/2019
#############################################################################
#############################################################################

library(ggplot2)
library(dplyr)
library(tidyr)
library(maps)
library(maptools)
library(rgdal)
library(sp)
library(sf)
library(stringr)
library(gtools)
library(ggmap)
library(mapproj)
library(geoR)
library(spatstat)

rm(list=ls())

mca <- readOGR(dsn=".", layer="09a")
proj4string(mca)
summary(mca)
mca2 <- spTransform(mca,CRS("+proj=utm +zone=14 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
summary(mca2)

#Prepping the 2016 data

data <- read.csv('av_data_total.csv', header=T)

data_2016 <- data %>%
  filter(Year==2016)

data_2016$lon <- data_2016$longitud.N.24.15
data_2016$lat <- data_2016$latitud.N.24.15

coords<-data_2016[c("lon","lat")]
coords<-coords[complete.cases(coords),]

sp <- SpatialPoints(coords)

proj4string(sp) <- CRS("+proj=longlat +datum=WGS84")
sp <- spTransform(sp,CRS("+proj=utm +zone=14 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
summary(sp)

plot(mca2)
plot(sp, col="red", cex=0.1, pch=19, add=T)

##Trying to create an id# for each polygon that is unique for each of 
##the 2432 polygons
#head(mca2@polygons)
duplicated(mca2@data$CVE_AGEB) #some are duplicates,need unique id

#function creates an id that aligns with the polygon #
mca2@data$ID <- sapply(slot(mca2, "polygons"), function(x) slot(x, "ID")) 
mca2@data$ID <- as.numeric(mca2@data$ID)

res<-over(sp, mca2)
#table(res$CVE_AGEB)
accidents_2016 <- as.data.frame(table(res$ID))

acc <- c(0:2431) #these are integers
acc <- as.data.frame(acc)
names(acc) <- "ID"
acc$ID <- as.factor(acc$ID)

names(accidents_2016) <- c("ID", "Obs")

accidents_2016 <- merge(accidents_2016, acc, by="ID", all=T)


accidents_2016$Obs[is.na(accidents_2016$Obs)] <- 0

summary(is.na(accidents_2016$Obs))
#complete dataset generated, 0s where NAs were

##Entering in the rlc data
rlc <- read.csv('RLC.csv', header=T)
coords2 <- rlc[c("LONG","LAT")]
coords2 <- coords2[complete.cases(coords2),]
sp2 <- SpatialPoints(coords2)
proj4string(sp2) <- CRS("+proj=longlat +datum=WGS84")
sp2 <- spTransform(sp2,CRS("+proj=utm +zone=14 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
proj4string(sp2)

plot(mca2)
plot(sp2, col="red", cex=0.2, pch=19, add=T)

#same procedure for red light cams
rlc<-over(sp2, mca2)
#table(rlc$CVE_AGEB)
cams <- as.data.frame(table(rlc$ID)) #46 complete cases

sum(is.na(cams$Freq))
sum(is.na(accidents_2016$Freq))

colnames(cams) <- c("ID", "Cams")
accidents_2016_complete <- merge(accidents_2016, cams, by="ID", all=T)
accidents_2016_complete$Cams[is.na(accidents_2016_complete$Cams)] <- 0

summary(is.na(accidents_2016_complete$Cams))


##The resultant data set from this procedure gives me the number of observed accidents in 2016 
##by AGEB and the number of cameras within the polygon.

#Now extending the Accidents_2016_complete data set with the Expected number of accidents
#using the 2015 data.

data_2015 <- data %>%
  filter(Year==2015)

data_2015$lon <- data_2015$longitud.N.24.15
data_2015$lat <- data_2015$latitud.N.24.15

coords<-data_2015[c("lon","lat")]
coords<-coords[complete.cases(coords),]

sp2015 <- SpatialPoints(coords)

proj4string(sp2015) <- CRS("+proj=longlat +datum=WGS84")
sp2015 <- spTransform(sp2015,CRS("+proj=utm +zone=14 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))

plot(mca2)
plot(sp2015, col="green", cex=0.1, pch=19, add=T)

res2015<-over(sp2015, mca2)
accidents_2015 <- as.data.frame(table(res2015$ID))
colnames(accidents_2015) <- c("ID", "Obs15")

accidents_2015 <- merge(accidents_2015, acc, by="ID", all=T)

accidents_2015$Obs15[is.na(accidents_2015$Obs15)] <- 0

summary(is.na(accidents_2015$Obs15))

accidents_2016_complete <- merge(accidents_2016_complete, accidents_2015, by="ID")

write.csv(accidents_2016_complete, "accidents_2016_complete.csv")
