# example soil sensing for ISRIC Springschool
# Author: Titia Mulder
# date: 23 May 2018

# CHANGE THE WORKING DIRECTORY
setwd("D:/ISRIC_springschool/4_SoilSensing/data_example")

## ----step_1, collapse=T, results='hide', message=F,fig.show="hide"-------
# empty memory and workspace
gc()
rm(list=ls())

# load libraries
require(sp)
require(raster)
require(rgdal)
require(pls)
#install.packages("ithir", repos="http://R-Forge.R-project.org")
# or download https://r-forge.r-project.org/R/?group_id=2000
require(ithir)
require(MASS)

## ----step_2, collapse=T, results='hide', message=F-----------------------
# load point data
point <- read.csv('./point_data.csv')
sample_data <- point[,c(1:10)]
names(sample_data) <- c("Site","Date","East","North", "pH_KCl", "SOM", "CEC","Clay","Silt","Sand") 

spectral_data <- point[,c(11:1823)]
spectral_data <- spectral_data[complete.cases(spectral_data),]
wl <- read.csv('./wavelength.csv', header=T)

plot(wl$x, spectral_data[1,], main='Spectral signature',xlab='wavelength', ylab='reflectance', type='p')


## ----step_3, collapse=T, results='hide', message=F,fig.show="hide"-------
test_plsr <- plsr(sample_data$pH_KCl ~ ., ncomp=15,data=spectral_data, validation="LOO",
                  method="oscorespls",na.action=na.exclude)
summary(test_plsr)

plot(test_plsr, plottype='validation')
plot(test_plsr, plottype='prediction')
plot(test_plsr, plottype='correlation')
plot(test_plsr, plottype='biplot')


## ----step_4, collapse=T, results='hide', message=F,fig.show="hide"-------
goof_plsr <- goof(sample_data$pH_KCl, test_plsr$fitted.values[,,10], type='spec')


## ----step_5, collapse=T, results='hide', message=F,fig.show="hide"-------
# create a random subset of calibration and prediction data
index <- sort(sample(nrow(sample_data), size = 20))
sample_cal <- sample_data[-index,]
sample_pred <- sample_data[index,]

spectra_cal <- spectral_data[-index,]
spectra_pred <- spectral_data[index,]

## ----step_6, collapse=T, results='hide', message=F,fig.show="hide"-------
model_plsr <- plsr(sample_cal$CEC ~ ., ncomp=8,data=spectra_cal, validation="LOO",
                  method="oscorespls",na.action=na.exclude)

summary(model_plsr)
plot(model_plsr, plottype='validation')
plot(model_plsr, plottype='prediction')

## ----step_7, collapse=T, results='hide', message=F,fig.show="hide"-------
model_cal <- predict(model_plsr, spectra_cal, comps=8)
model_pred <- predict(model_plsr, spectra_pred, comps=8)
plot(sample_pred$CEC, model_pred[,1], main='Observed vs Predicted new values')

## ----step_8, collapse=T, results='hide', message=F,fig.show="hide"-------
goof_cal <- goof(sample_cal$CEC, model_plsr$fitted.values[,,8], type='spec')
goof_pred <- goof(sample_pred$CEC, model_pred[,1], type='spec') 

goof_final <- rbind(goof_cal, goof_pred)
goof_final$dataset <- c("Calibration", 'Validation')

####################################
# PART 2: 



## ----step_9, collapse=T, results='hide', message=F,fig.show="hide"-------
# load point data
point <- read.csv('./point_data.csv')
sample_data <- point[,c(1:10)]
names(sample_data) <- c("Site","Date","East","North", "pH_KCl", "SOM", "CEC","Clay","Silt","Sand") 
point_xy <- SpatialPoints(point[,c(3,4)], CRS("+init=epsg:4326"))

## ----step_10, collapse=T, results='hide', message=F----------------------
# Load ASTER satellite data
aster <- stack('./aster.tif')
names(aster) <- c('b1','b2','b3','b4','b5','b6','b7','b8','b9','b10','b11','b12','b13','b14')

# Load ASTER GEOSCIENCE product
aster_geo <- stack('./aster_geo.tif')
names(aster_geo) <- c('GV','FO_cont', 'FO_comp','FI_index', 'OpI','AlOH_index', 
                      'AlOH_comp', 'kao_index', 'FeOh_cont','MgOH_cont', 'MgOH_comp', 'mgoh_fe' )

# check allignment raster and point data
plot(aster[[1]])
points(point_xy)


## ----step_11, collapse=T, results='hide', message=F,fig.show="hide"------
# extract data to points
point_aster <- extract(aster, point_xy, df=TRUE)
point_aster_geo <- extract(aster_geo, point_xy, df=TRUE)


# create one dataset RS
data_rs <- data.frame(point_aster, point_aster_geo[,-c(1)])

# add data to soil information
data_all <- data.frame(sample_data, data_rs[,-c(1)])
data_all <- data_all[complete.cases(data_all),]


## ----step_12, collapse=T, results='hide', message=F,fig.show="hide"------
# derive the correlation between variables
correl <- cor(as.matrix(data_all[,-c(1:4)]))


## ----step_13, collapse=T, results='hide', message=F,fig.show="hide"------
# model for SOM with aster bands only
lm_model_aster_0 <- lm(SOM ~ b1+b2+b3+b4+b5+b6+b7+b8+b9+b10+b11+b12+b13+b14, data=data_all )
lm_model_aster <- stepAIC(lm_model_aster_0, direction='both')

summary(lm_model_aster)
lm_model_aster$anova

## ----step_14, collapse=T, results='hide', message=F,fig.show="hide"------
# model for SOM with aster geoscience products only
lm_model_geo_0 <- lm(SOM ~ GV+FO_cont+FO_comp+FI_index+OpI+AlOH_index+AlOH_comp+kao_index+FeOh_cont+MgOH_cont+
                      MgOH_comp+mgoh_fe, data=data_all )
lm_model_geo <- stepAIC(lm_model_geo_0, direction='both')

summary(lm_model_geo)


## ----step_15, collapse=T, results='hide', message=F,fig.show="hide"------
# model for SOM with all aster data
lm_model_all_0 <- lm(SOM ~ b1+b2+b3+b4+b5+b6+b7+b8+b9+b10+b11+b12+b13+b14+
                       GV+FO_cont+FO_comp+FI_index+OpI+AlOH_index+AlOH_comp+kao_index+FeOh_cont+MgOH_cont+MgOH_comp+mgoh_fe, data=data_all )
lm_model_all <- stepAIC(lm_model_all_0, direction='both')

summary(lm_model_all)


## ----step_16, collapse=T, results='hide', message=F----------------------
# plot observed versus predicted
plot(data_all$SOM, lm_model_aster$fitted.values, col='black', main='Observed vs predicted',
     xlab='observed',ylab='predicted')
points(data_all$SOM, lm_model_geo$fitted.values, col= 'red')
points(data_all$SOM, lm_model_all$fitted.values, col='blue')
abline(0,1)

## ----step_17, collapse=T, results='hide', message=F,fig.show="hide"------
############## Goodness of Fit calibration model

goof_aster <- goof(data_all$SOM, lm_model_aster$fitted.values, type='spec') 
goof_geo <- goof(data_all$SOM, lm_model_geo$fitted.values, type='spec') 
goof_all <- goof(data_all$SOM, lm_model_all$fitted.values, type='spec') 

goof_final <- rbind(goof_aster, goof_geo, goof_all)
goof_final$data <- c("aster", 'geo', 'all')


## ----step_18, collapse=T, results='hide', message=F,fig.show="hide"------
map_aster <- predict(aster, model=lm_model_aster,  progress='text',
                     filename='map_aster_lm.tif', format='GTiff',datatype='FLT4S', overwrite=T)

plot(map_aster)

# end of script;