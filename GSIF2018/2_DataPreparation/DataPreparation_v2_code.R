############################################################################################
## R script belonging to the tutorial 'Data preparation for DSM'
## Version: 2.0, 16 May 2018
## Author: Dr. B. Kempen, ISRIC - World Soil Information
## License: CC-BY-NC-SA
############################################################################################

# CHANGE THE WORK DIRECTORY!!
# DO NOT CHANGE THE NAME OF THE FOLDER'2_DataPreparation'

setwd("D:/_temp/2_DataPreparation/")


## ----step_1, collapse=T, results='hide', message=F-----------------------
# empty memory and workspace
gc()
rm(list=ls())

# load libraries
require(sp)
require(raster)
require(plyr)
require(caret)

## ----read_data, collapse=T, results='hide', message=F--------------------
# read profile and horizon data
d_prof <- read.csv("./Data/soil_data/profiles.csv", header = TRUE)
d_hor <- read.csv("./Data/soil_data/soil_horizons.csv", header = TRUE)

# inspect profile data
str(d_prof)
head(d_prof)

# inspect horizon data
str(d_hor)
head(d_hor)
summary(d_hor)

# join the two tables
d1 <- join(d_prof, d_hor, by='ProfID', type = "left") 

# show first six rows of the data.frame
head(d1)


## ----clean_up_data1, message=F-------------------------------------------
# remove some unnecessary columns
d1 <- d1[,-c(2,5)]  

# remove NoData values, which is the value -9999 in this case
d1 <- subset(d1, d1$SOC != -9999) 

## ----clean_up_data2, message=F-------------------------------------------
# Check if there are duplicate rows present in the dataframe
dum <- duplicated(d1[c(1,6,7)]) 
summary(dum)

## ----clean_up_data3, message=F-------------------------------------------
# remove duplicated observations based on similar depth and profile id
d1 <-  d1[!duplicated(d1[c(1,6,7)]),] 

# save
save(d1, file = "pointData.rda")

## ----clean_up, echo=FALSE------------------------------------------------
rm(dum)

## ----source, collapse=T, results='hide', message=F-----------------------
source("funComputeSoilProperty.R")

## ----compute_soil_prop, collapse=T, results='hide', message=F------------
# run the function for weighted averaging soil properties
d2 <- computeSoilProperty("SOC", d1, 0, 30, 25)

## ----exclude_colocated_sites, collapse=T, results='hide', message=F------
# remove observations that have identical coordinates
d <- d2[!duplicated(d2[c(2,3)]),]

# save dataset
save(d, file = "pointData_SOC.rda")

## ----data_inspection, message=F------------------------------------------
# summary statistics
summary(d$SOC)

# histogram
hist(d$SOC, col = "lightblue", xlab="%", main="SOC", breaks=20)

# spatial plot (first convert to SpatialPointsDataFrame)
coordinates(d) <- ~ X_coord+Y_coord
plot(d)

## ----plot, message=F-----------------------------------------------------
# define arrow marker
arrow <- list(
  "SpatialPolygonsRescale", 
  layout.north.arrow(), 
  offset = c(7655000,4530000), 
  scale = 20000
)

# define lengend breaks
breaks <- c(seq(0,5,0.5),seq(10,50,10))

# plot
spplot(d, c("SOC"), 
  cuts = breaks,
  cex = 0.7,
  sp.layout = list(arrow),
  key.space = "right",
  main = "Macedonia - SOC (%)",
  xlab = "easting (m)",
  ylab = "northing (m)",
  scales = list(draw=TRUE)
)

# convert back to data.frame
d <- as(d, Class = "data.frame")

## ----raster_stack--------------------------------------------------------
# list the files in the covariate folder
cov.lst <- list.files(path = "./Data/covariates/1km/wgs84/stack1", pattern =".tif")

# explore the set
head(cov.lst )
length(cov.lst )

# create raster stack
r1 <- stack(paste0("./Data/covariates/1km/wgs84/stack1/", cov.lst )) 

# check object class
class(r1)

# show dimensions
dim(r1)

## ----plot_covariate------------------------------------------------------
plot(r1[["DEMENV5"]], main = "Digital Elevation Model", xlab = "Easting (degrees)", ylab = "Northing (degrees)")

## ----mask, message=F-----------------------------------------------------
# read mask
m <- raster(paste0("./Data/covariates/1km/wgs84/mask/", "mask.tif"))

# mask the stack
r2 <- mask(r1, m)

# inspect the stack object
r2

# plot one covariate layer
plot(r2[["DEMENV5"]], main = "Digital Elevation Model", xlab = "Easting (degrees)", ylab = "Northing (degrees)")


## ----check projection, message=F-----------------------------------------
# check projection
proj4string(r2)


## ----project, message=F--------------------------------------------------
# reproject the stack
r3 <- projectRaster(r2, crs = CRS("+init=epsg:6316"), res = 1000, method = 'ngb')

# inspect the output
r3

# plot one covariate layer
plot(r3[["DEMENV5"]], main = "Digital Elevation Model", xlab = "Easting (m)", ylab = "Northing (m)")

## ----convert, results='hide', message=F----------------------------------
# convert to SpatialGridDataFrame
r4 <- as(r3, "SpatialGridDataFrame")

# further convert to data.frame
r4 <- as(r4, Class = "data.frame")

## ----categorical, results='hide', message=F------------------------------
# list categorical covariates
cat.list <- c("LCEE10")

# check data type
class(r4[,cat.list])
summary(r4[,cat.list])

# convert to factor in a loop
for(i in 1:length(cat.list)){
  r4[,cat.list[i]] <- as.factor(r4[,cat.list[i]])
}

# check again; note the variables have been converted to factor
class(r4[,cat.list])
summary(r4[,cat.list])

## ----cleanup2, echo=FALSE, message=F-------------------------------------
# clean-up
rm(i)

## ----binary_layers1, results='hide', message=F---------------------------
# convert categorical covariates to binary layers
for(i in 1:length(cat.list)){
  
  # create design matrix 
  dum <- model.matrix(as.formula(paste0("~",cat.list[i],"+0")), r4)
  
  # convert to data.frame
  dum <- as.data.frame(dum)
  
  # convert to factor
  dum <- lapply(dum, FUN = factor)
  
  # convert to data.frame
  dum <- as.data.frame(dum)
  
  # append to covariate stack
  r5 <- cbind(r4, dum)
}

## ----binary_layers2, results='hide', message=F---------------------------
# exclude original categorical covariates
r5 <- r5[,!names(r5) %in% cat.list]

## ----nzv, message=F------------------------------------------------------
# run nzv function
nzv <- nearZeroVar(r5, saveMetrics = TRUE)

# inspect the output
nzv

# zero variance covariates
summary(nzv$zeroVar)

# near-zero variance covariates
summary(nzv$nzv)

## ----nzv2, results='hide', message=F-------------------------------------
# drop nzv layers
r6 <- r5[,!nzv[,4]]

## ----statistical_summary, message=F--------------------------------------
# summary statistics
summary(r6)

## ----complete_cases, message=F-------------------------------------------
# exclude pixels that do not have complete covariate data
r <- r6[complete.cases(r6),]

## ----re-order_clean_save, message=F--------------------------------------
# check the covariate names
names(r6)

# re-order; names of coordinate columns are "s1","s2"
r <- cbind(r6[,c("s1","s2")], r6[,!names(r6) %in% c("s1","s2")])

# clean-up
rm(dum, i)

# save covariate stack
save(r, file='covariateStack.rda')

## ----convert_to_spatial_data, results='hide', message=F------------------
# get site locations from the data_stock file
coordinates(d) <- ~X_coord+Y_coord

# set projection
proj4string(d) <- CRS("+init=epsg:6316")

# show the data structure
str(d)

## ----convert_stack_to_spatial_data, results='hide', message=F------------
# get site locations from the data_stock file
gridded(r) <- ~s1+s2

# set projection
proj4string(r) <- CRS("+init=epsg:6316")

## ----overlay, results='hide', message=F----------------------------------
# spatial overlay
dum <- over(d, r)

# append covariate data to soil data
rm <- cbind(d@data, d@coords, dum)

# save
save(rm, file="regressionMatrix.rda")

## ----overlay_raster, eval = FALSE, results='hide', message=F-------------
# # convert stack to data.frame and reorder
# r <- as.data.frame(r)
# r <- cbind(r6[,c("s1","s2")], r6[,!names(r6) %in% c("s1","s2")])
# 
# # create rasterBrick object
# r7 <- rasterFromXYZ(r, crs="+init=epsg:6316")
# 
# # overlay
# dum <- extract(r7, d)
# 
# # convert to data.frame
# dum <- as.data.frame(dum)
# 
# # convert categorical covariates to factor
# dum <- cbind(r6[,c("s1","s2")], dum[,names(dum) %in% c("LCEE")])
# 
# # convert the binary land cover layers to factor
# # use patter recognition with the grep function to identify columns
# dum[,grep("LCEE", names(dum), value = TRUE)] <- lapply(dum[,grep("LCEE", names(dum), value = TRUE)], FUN = factor)
# 
# # append covariate data to soil data
# rm <- cbind(d@data, d@coords, dum)
# 
# # clean-up
# rm(dum)

## ----save_workspace, message=F-------------------------------------------
# save
save.image("DataPreparation.rda")

# end of script;