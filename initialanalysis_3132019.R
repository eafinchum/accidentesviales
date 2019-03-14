##This script illustrates the preliminary data analysis for the accident data from 2016
##Emily Finchum-Mason, 3/13/2019

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
#library(ggmap)
library(mapproj)
library(geoR)
#library(spatstat)
library(INLA)
library(SpatialEpi)
library(spdep)
library(RColorBrewer)

accidents_2016_complete <- read.csv("accidents_2016_complete.csv", header=T)
mca2 <- readOGR(dsn=".", layer="09a")

mca2@data$ID <- sapply(slot(mca2, "polygons"), function(x) slot(x, "ID")) 
mca2@data$ID <- as.numeric(mca2@data$ID)

#calculating the SMRs in each AGEB
accidents_2016_complete$SMR <- accidents_2016_complete$Obs/accidents_2016_complete$Obs15

xd <- accidents_2016_complete
rm(accidents_2016_complete)

xd$Y <- as.numeric(xd$Obs)
xd$E <- as.numeric(xd$Obs15)
xd$cam <- as.numeric(xd$Cams)

xd$SMR <- xd$Y/((xd$E+1))

xd <- xd %>%
  dplyr::select(-X)

plot(xd$Cams, xd$SMR,
     xlab="Number of Red Light Cameras",
     ylab="SMR",
     main="SMRs versus Number of Cameras\nin each Geostatistical Area")

###########################################################################
#Fitting basic Poisson model
###########################################################################

mod1 <- glm(Y ~ cam + offset(log(E+1)), data=xd, family="poisson")
summary(mod1)

##Parameter estimates
mod1coeff <- coefficients(mod1)

#Standard errors
mod1SE1 <- sqrt(vcov(mod1)[1,1])
mod1SE2 <- sqrt(vcov(mod1)[2,2])

#relative risk estimate
#RR for areas differing by one camera
RR_poissonest <- exp(mod1coeff[2]/100)

#pander this for final paper
confint(mod1)

###########################################################################
##Quasi-likelihood model, allowing for excess Poisson variability
###########################################################################

mod2 <- glm(Y~cam+offset(log(E+1)), data=xd, family=quasipoisson(link="log"))
summary(mod2)

#significance on the camera effect significantly decreased
mod2coeff <- coefficients(mod2)
mod2SE1 <- sqrt(vcov(mod2)[1,1])
mod2SE2 <- sqrt(vcov(mod2)[2,2])

RR_qpoisest <- exp(mod1coeff[2]/100)

confint(mod2)

###########################################################################
##Assessing overdispersion
###########################################################################

k <- (mod2SE2/mod1SE2)^2

#fairly large overdispersion here, 2.67

###########################################################################
##Assessing distribution of SMRs, plotting out SMRs and their variance
###########################################################################

ggplot(xd, aes(x=SMR))+
  geom_histogram()+
  ggtitle("Distribution of SMRs")+
  ylab("Frequency")
  
xd$varSMR <- xd$SMR/((xd$E)+1) #to avoid 0s in the denominator
summary(xd$varSMR)

ggplot(xd, aes(x=varSMR))+
  geom_histogram()+
  ggtitle("Distribution of SMRs")+
  ylab("Frequency")

#Merging calculating variables with the map data in order to spatially assess

mca2 <- merge(mca2, xd, by.x="ID", by.y="ID")

par(mfrow=c(1,3))
mapvariable(mca2@data$SMR, mca2, nlevels=7,
            main="SMRs by Geostatistical Area")
mapvariable(mca2@data$varSMR, mca2, nlevels=7,
            main="SMR variance by Geostatistical Area")
mapvariable(mca2@data$E, mca2, nlevels=7,
            main="Expected number of accidents\nby Geostatistical Area")

#Do larger SMRs tend to have larger standard errors

xd$SE <- sqrt(xd$SMR/xd$E)
plot(SMR~SE, data=xd, pch=5, cex=0.3, col="red",
     main="Comparison of SMRs to SMR Standard Errors")


#Here we do see that areas with higher SMRs have higher standard errors

###########################################################################
##Gamma smoothing model
###########################################################################
xd$Eadj = xd$E+0.5
xd$Yadj = xd$Y+0.5
ebayes <- eBayes(xd$Y, xd$Eadj)
alpha <- ebayes$alpha
beta0 <- exp(ebayes$beta)

#Assessing gamma assumption

egamma <- qgamma(seq(0.5, length(xd$Y), 1)/length(xd$Y),
                 ebayes$alpha, ebayes$alpha)
par(mfrow = c(1, 2))
plot(egamma, exp(-ebayes$beta) * sort(xd$SMR),
     xlim = c(0,5), ylim = c(0, 5), 
     xlab = "Exp Order Stat", ylab = "Obs Order")
abline(0,1)
plot(egamma, exp(-ebayes$beta) * sort(ebayes$RR),
     xlim = c(0, 5), ylim = c(0, 5), xlab = "Exp Order Stat",
     ylab = "Obs Order Stat (Gamma)")
abline(0,1)

par(mfrow=c(1,2))
plot(xd$SMR, ebayes$RR, xlab="SMR", ylab="Emp. Bayes RR estimates",
     pch=4, col="blue")
abline(0,1)
plot(log(xd$SMR), log(ebayes$RR), xlab="Logged SMR", ylab="Logged Emp. Bayes RR estimates",
     pch=4, col="blue")
abline(0,1)

#Including the covariate for cameras

ebayes_cam <- eBayes(xd$Y, xd$Eadj, xd$Cams)
par(mfrow=c(1,1))
plot(xd$cam, xd$SMR, xlab = "Number of Cameras",
     ylab="SMR",
     main="Gamma smoothing model with Covariate for\nNumber of cameras")
xval <- seq(0, max(xd$Cams), 0.01)
lines(xval, exp(ebayes_cam$beta[1]+ebayes_cam$beta[2]*xval))

#so, we don't see an indication that number of cameras in an area actually influences the
#number of occurrences relative to the number of occurrences in the previous time period.

var.wo.cam <- 1/(ebayes$alpha)
var.w.cam <- 1/(ebayes_cam$alpha)

#indeed, we aren't actually explaining any more of the variance with the inclusion of
#this variable.

my.palette <- brewer.pal(n=7, name="OrRd")

spplot(mca2,"SMR", col.regions=my.palette, cuts=6, col="transparent",
       main="SMRs by Geostatistical Areas")

#############################################################################
##Poisson-lognormal model with INLA, without and then with covariate for cam
#############################################################################

pcprec <- list(theta=list(prior="pc.prec", param=c(1,0.05)))
inla1 <- inla(Y~1+f(ID, model="iid", hyper=pcprec), data=xd,
              family="poisson", E=E, control.predictor = list(compute=T))

postmed <- inla1$summary.fixed[4] #intercept, -0.0215
postsd <- 1/sqrt(inla1$summary.hyperpar[4]) #sd = 0.242

lnormREs <- exp(inla1$summary.random$ID[5])
names(lnormREs) <- "LnormRE"

lnormRRs <- as.double(exp(postmed))*lnormREs[,1]

mca2@data$ebayesRR <- ebayes$RR #from gamma model without covariate
mca2@data$LnormRR <- lnormRRs

spplot(mca2,"ebayesRR", col.regions=my.palette, cuts=6, col="transparent",
       main="Relative Risk Estimates\nby Geostatistical Areas",
       sub="ebayesRR, no Covariate")

spplot(mca2,"LnormRR", col.regions=my.palette, cuts=6, col="transparent",
       main="Relative Risk Estimates\nby Geostatistical Areas",
       sub="Poisson-Lognormal Model (INLA), no Covariate")

##Running the model with the camera covariate

inla_cam <- inla(Y~1+I(Cams)+f(ID, model="iid", hyper=pcprec), data=xd,
                 family="poisson", E=E, control.predictor = list(compute=T))
summary(inla_cam)

postmed_int <- inla_cam$summary.fixed[1,4]
postmed_beta1 <- inla_cam$summary.fixed[2,4]

exp(postmed_beta1)
#The posterior median of the relative risk associated with an extra camera is 0.957,
#indicating that the risk is decreasing
#95% confidence interval calculated below:

ci_lb <- exp(inla_cam$summary.fixed[2,3])
ci_ub <- exp(inla_cam$summary.fixed[2,5])

#the interval is [0.9007,1.018] - it includes one, so we cannot glean that the
#differences are significant.

#the median estimate of the standard deviation, with confidence intervals:
postsd_cam <- 1/sqrt(inla_cam$summary.hyperpar[4]) #0.242
lb_postsd <- 1/sqrt(inla_cam$summary.hyperpar[3]) #0.227
ub_postsd <- 1/sqrt(inla_cam$summary.hyperpar[5]) #0.257

#Posterior median of a 95% RRR interval
lb_RRR <- exp((-1.96)*postsd_cam) #0.623
ub_RRR <- exp((1.96)*postsd_cam) #1.61
#residual relative risk interval is fairly wide

#############################################################################
##Lognormal model with iid & ICAR error terms
#############################################################################

#creating a way of uniquely identifying polygons (sorting out the
#duplicate problem as a temporary work around)

#install.packages("diseasemapping")
library(diseasemapping)

nbFOQ <- poly2nb(mca2)
nb2INLA('nb.adj', nbFOQ)

#Re-writing IDs such that the polygon data match the adjacency matrix identifier
mca2@data$ID <- sapply(slot(mca2, "polygons"), function(x) slot(x, "ID")) 
mca2@data$ID <- as.numeric(mca2@data$ID)

formula <- Y ~ 1 + I(Cams) + f(ID, model="bym2", graph="nb.adj")
inla_spatial <- inla(formula, data = mca2@data, family = "poisson",
                      E = E, control.predictor = list(compute = TRUE))

inla_spatial$summary.hyperpar

#posterior median of total standard deviation
postsd_spatial <- 1/sqrt(inla_spatial$summary.hyperpar[1,4]) #0.23
postsd_spatial
#confidence interval


#posterior median of phi
phi <- inla_spatial$summary.hyperpar[2,4] #0.05
phi

#The proportion of the residual variation that is spatial is actually
#not very large

#Mapping the spatial and non-spatial random effects, residual relative risk
diff <- inla_spatial$summary.random$ID[1:2432, 2]-inla_spatial$summary.random$ID[2433:4864,2]
REnonspat <- exp(diff)
REspat <- exp(inla_spatial$summary.random$ID[2433:4864,5])
mca2@data$REnonspat <- REnonspat
mca2@data$REspat <- REspat

spplot(mca2, "REnonspat", col.regions=my.palette, cuts=6, col="transparent",
       main="Nonspatial Random Effects",
       sub = "Lognormal Model with iid and ICAR errors")

spplot(mca2, "REspat", col.regions=my.palette, cuts=6, col="transparent",
       main="Spatial Random Effects",
       sub = "Lognormal Model with iid and ICAR errors")

#Posterior standard deviation is not changing appreciably with the inclusion of the 
#spatial term, indicating that the spatial model is not providing much empirical 
#leverage.

#plot(inla_spatial)

beta0_sp <- inla_spatial$summary.fixed[1,4]
exp(inla_spatial$summary.fixed[1,3]) #[0.975,1.004]
exp(inla_spatial$summary.fixed[1,5])

beta1_sp <- inla_spatial$summary.fixed[2,4]
exp(inla_spatial$summary.fixed[2,3]) #[0.9035,1.026]
exp(inla_spatial$summary.fixed[2,5])

spmodRR <- exp(beta0_sp)*exp(beta1_sp*mca2@data$Cams)*REnonspat*REspat
