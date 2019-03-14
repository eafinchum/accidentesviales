#cluster analysis for point data
library(lattice)
library(maptools)

#using AXA data as set of points
#first creating SpatialPoints object from the data frame

data <- read.csv("av_data_total.csv", header=T)
data <- data %>%
  filter(Year!=2018)

data$lon <- data$longitud.N.24.15
data$lat <- data$latitud.N.24.15

coords<-data[c("lon","lat")]
coords<-coords[complete.cases(coords),]

sp <- SpatialPoints(coords)


proj4string(sp) <- CRS("+proj=longlat +datum=WGS84") 
sp <- spTransform(sp,CRS("+proj=utm +zone=14 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))

summary(sp)

#using elide function to disguise the coordinate system and translate it to the unit square
sp1 <- elide(sp, scale=T, unitsq=T)

ppp <- as(sp1, "ppp")
summary(ppp)

#plotting the dispersion of the points
sp1_coords <- as.data.frame(coordinates(sp1))
names(sp1_coords) <- c("x","y")

print(xyplot(y~x, data=sp1_coords, pch=19, cex=0.4))
#there is so much data that it makes getting anything out of this extremely
#difficult.

#Proceeding to explore in week-by-week frames
#using the week 1, January 2016 subset:

data_1_1_2016 <- data %>%
  filter(month==1 & Year==2016 & dia_numero.N.10.0<=7) %>%
  select(latitud.N.24.15,longitud.N.24.15)

data_1_1_2016$lon <- data_1_1_2016$longitud.N.24.15
data_1_1_2016$lat <- data_1_1_2016$latitud.N.24.15

coords<-data_1_1_2016[c("lon","lat")]
coords<-coords[complete.cases(coords),]

sp_12016 <- SpatialPoints(coords)

proj4string(sp_12016) <- CRS("+proj=longlat +datum=WGS84") #Need to specify that these are latitudes and longitudes
sp <- spTransform(sp_12016,CRS("+proj=utm +zone=14 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
 
sp_12016 <- elide(sp_12016, scale=T, unitsq=T)

ppp_12016 <- as(sp_12016, "ppp")
summary(ppp_12016)

#plotting the dispersion of the points
sp1_coords_12016<- as.data.frame(coordinates(sp_12016))
names(sp1_coords_12016) <- c("x","y")

print(xyplot(y~x, data=sp1_coords_12016, pch=19, cex=0.4,
             main="Point Patterns of Accidents in Week 1\nof January 2016"))

##Running G functions and monte carlo envelopes
set.seed(120109)
r <- seq(0, sqrt(2)/6, by = 0.001)
env_acc <- envelope(ppp_12016, fun=Gest, nrank=2,nsim=99)
print(xyplot(obs~theo, data=env_acc,
             type="1", xlab="Theoretical",
             ylab="Observed", panel=function(x,y,subscripts){
               lpolygon(c(x,rev(x)), c(env_acc$lo[subscripts], env_acc$hi[subscripts]),
               border="gray", col="gray")
      llines(x,y,col="black",lwd=2)}))


set.seed(1205)
Fenv_acc <- envelope(ppp_12016, fun=Fest,
                     nrank=2, nsim=99)
print(xyplot(obs~theo, data=Fenv_acc,
             type="1", xlab="Theoretical",
             ylab="Observed", panel=function(x,y,subscripts){
               lpolygon(c(x,rev(x)), c(Fenv_acc$lo[subscripts], Fenv_acc$hi[subscripts]),
                        border="gray", col="gray")
               llines(x,y,col="black",lwd=2)}))

