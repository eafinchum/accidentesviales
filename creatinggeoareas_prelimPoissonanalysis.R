#MCRLC Study

#overlaying the data points for week1, january2016 and calculating the number that fall into each bsa

data <- read.csv("av_data_total.csv", header=T)
data <- data %>%
  filter(Year!=2018)

data_1_1_2016 <- data %>%
  filter(month==1 & Year==2016 & dia_numero.N.10.0<=7) %>%
  select(latitud.N.24.15,longitud.N.24.15)

data_1_1_2016$lon <- data_1_1_2016$longitud.N.24.15
data_1_1_2016$lat <- data_1_1_2016$latitud.N.24.15

coords<-data_1_1_2016[c("lon","lat")]
coords<-coords[complete.cases(coords),]

sp <- SpatialPoints(coords)
proj4string(sp)

proj4string(sp) <- CRS("+proj=longlat +datum=WGS84")
sp <- spTransform(sp,CRS("+proj=utm +zone=14 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))

plot(sp, col="blue", pch=19)

#############################################################################
#############################################################################
##Opening the red light cam locations
#############################################################################
#############################################################################

rlc <- read.csv('RLC.csv', header=T)
coords2 <- rlc[c("LONG","LAT")]
coords2 <- coords2[complete.cases(coords2),]
sp2 <- SpatialPoints(coords2)
proj4string(sp2) <- CRS("+proj=longlat +datum=WGS84")
sp2 <- spTransform(sp2,CRS("+proj=utm +zone=14 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
proj4string(sp2)
plot(sp2, col="red",add=T, pch=20)

#instead mapping the intersection buffers (50m in diameter, N=393)
#intersections <- readOGR(dsn=".", layer="mc5int_buffer.shp")
#install.packages('raster')
#library(raster)


#More recent changes here related to the revised shapefile of
#intersection locations

intersections <- readOGR(dsn=".", layer="mcint5_buffer")

#changing coordinate referencing systems to match one another
proj4string(intersections)
summary(intersections)
intersections <- spTransform(intersections, CRS("+proj=utm +zone=14 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))

##altering the ID number of each intersection
intersections@data$FID<-as.numeric(intersections@data$ORIG_FID)

plot(intersections)
plot(sp, col="red", cex=0.1, add=T)
res <- over(sp,intersections)
table(res$FID)

#############################################################################
#############################################################################
##The above is proof of concept for the subsample of data from January 2016
##Now, using the entirety of 2016 data.
#############################################################################
#############################################################################

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

plot(intersections)
plot(sp, col="red", cex=0.1, pch=19, add=T)
plot(sp2, col="black", cex=0.2, pch=19, add=T)

res<-over(sp, intersections)
table(res$FID) #ORIG_FID is the original identifier that goes from 0 to 392, class(string)
accidents_2016 <- as.data.frame(table(res$FID))
names(accidents_2016) <- c("FID", "Obs")

plot(intersections)
plot(sp2, col="red", cex=0.2, pch=19, add=T)

#same procedure for red light cams
rlc<-over(sp2, intersections)
table(rlc$FID)

##Creating a data frame that represents all possible intersections in this bunch
##with the missing values given as 0s.

accidents_2016_complete <- c(0:392)
accidents_2016_complete <- as.data.frame(accidents_2016_complete)
names(accidents_2016_complete) <- "FID"

accidents_2016_complete <- merge(accidents_2016_complete, accidents_2016, by="FID", all=T)

##Replacing the NAs with 0s (as no reported accidents occurred at these intersections)
baseR.replace      <- function(x) { replace(x, is.na(x), 0) }
accidents_2016_complete$Obs1 <- baseR.replace(accidents_2016_complete$Obs)

#############################################################################
#############################################################################
##Aggregating the 2015 numbers in order to compute expected values
#############################################################################
#############################################################################

data_2015 <- data %>%
  filter(Year==2015)

data_2015$lon <- data_2015$longitud.N.24.15
data_2015$lat <- data_2015$latitud.N.24.15

coords<-data_2015[c("lon","lat")]
coords<-coords[complete.cases(coords),]

sp2015 <- SpatialPoints(coords)

proj4string(sp2015) <- CRS("+proj=longlat +datum=WGS84")
sp2015 <- spTransform(sp2015,CRS("+proj=utm +zone=14 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))

plot(intersections)
plot(sp2015, col="red", cex=0.1, pch=19, add=T)

res2015<-over(sp2015, intersections)
table(res2015$FID) #ORIG_FID is the original identifier that goes from 0 to 392, class(string)
accidents_2015 <- as.data.frame(table(res2015$FID))
names(accidents_2015) <- c("FID", "Obs")

accidents_2015_complete <- c(0:392)
accidents_2015_complete <- as.data.frame(accidents_2015_complete)
names(accidents_2015_complete) <- "FID"

accidents_2015_complete <- merge(accidents_2015_complete, accidents_2015, by="FID", all=T)

##Replacing the NAs with 0s (as no reported accidents occurred at these intersections)

accidents_2015_complete$Obs1 <- baseR.replace(accidents_2015_complete$Obs)

#############################################################################
#############################################################################
##Tallying observations in each year located within 25m of the intersections
#############################################################################
#############################################################################

sum(accidents_2015_complete$Obs1)
sum(accidents_2016_complete$Obs1)

#############################################################################
#############################################################################
##Overlaying the red light cameras on the intersection data
#############################################################################
#############################################################################

rlc <- read.csv('RLC.csv', header=T)
coords2 <- rlc[c("LONG","LAT")]
coords2 <- coords2[complete.cases(coords2),]
sp2 <- SpatialPoints(coords2)
proj4string(sp2) <- CRS("+proj=longlat +datum=WGS84")
sp2 <- spTransform(sp2,CRS("+proj=utm +zone=14 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
proj4string(sp2)

plot(intersections)
plot(sp2, col="red", pch=19, cex=0.5, add=T)

#############################################################################
#############################################################################
##Generating Expected numbers of accidents at each intersection
#############################################################################
#############################################################################
library(lme4)

exp_mod <- glm(Obs1 ~ 1, data=accidents_2015_complete, family=Poisson())
summary(exp_mod)

#############################################################################
#############################################################################
##Attempting this same process with the gsas instead of the intersections
#############################################################################
#############################################################################
rm(list=ls())

mex_city_areas <- readOGR(dsn=".", layer="09a")
proj4string(mex_city_areas)
summary(mex_city_areas)
mca2 <- spTransform(mex_city_areas,CRS("+proj=utm +zone=14 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
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

res<-over(sp, mca2)
#table(res$CVE_AGEB) #area geostatistica basica (AGEB) indicator
accidents_2016 <- as.data.frame(table(res$CVE_AGEB))
names(accidents_2016) <- c("FID", "Obs")

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
cams <- as.data.frame(table(rlc$CVE_AGEB)) #54 complete cases

sum(is.na(cams$Freq))
sum(is.na(accidents_2016$Freq))

colnames(cams) <- c("AGEB", "Cams")
colnames(accidents_2016) <- c("AGEB", "Obs")
accidents_2016_complete <- merge(accidents_2016, cams, by="AGEB")

##The resultant data set from this procedure gives me the number of observed accidents in 2016 
##by AGEB and the number of cameras within the AGEB.

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
#table(res2015$CVE_AGEB) #ORIG_FID is the original identifier that goes from 0 to 392, class(string)
accidents_2015 <- as.data.frame(table(res2015$CVE_AGEB))
colnames(accidents_2015) <- c("AGEB", "Obs15")

accidents_2016_complete <- merge(accidents_2016_complete, accidents_2015, by="AGEB")

write.csv(accidents_2016_complete, "accidents_2016_complete.csv")
