############################################################################################
## R script belonging to the tutorial 'Random Forest modelling for DSM'
## Version: 1.1, 23 May 2018
## Author: Dr. B. Kempen, ISRIC - World Soil Information
## License: CC-BY-NC-SA
############################################################################################

# CHANGE THE WORK DIRECTORY!!
# DO NOT CHANGE THE NAME OF THE FOLDER '5_MachineLearning1'

setwd("D:/_temp/5_MachineLearning1/")


## ----initialization, collapse=T, results='hide', message=F---------------
# empty memory and workspace
gc()
rm(list=ls())

# load libraries
require(sp)
require(raster)
require(rgdal)
require(randomForest)
require(leaflet)
require(plotKML)
require(htmlwidgets)

## ----seed, collapse=T, results='hide', message=F-------------------------
# set random seed
set.seed(20180523)

## ----read_data, message=F------------------------------------------------
# read profile and horizon data
load("regressionMatrix.rda")
load("covariateStack.rda")

## ----complete_cases, results='hide', message=F---------------------------
# check completeness
summary(complete.cases(rm))

## ----complete_cases2, results='hide', message=F--------------------------
# check if incomplete observations
rm[!complete.cases(rm),]

# exclude incomplete observations
rm <- rm[complete.cases(rm),]

# summary stats
summary(rm$SOC)

## ----remove_SOC0, results='hide', message=F------------------------------
# check if incomplete observations
rm <- rm[rm$SOC!=0,]

## ----prepare_inputs, message=F-------------------------------------------
# check the column name of the soil property we want to model
names(rm) # it is 'SOC'

# copy soil property values to a new vector
d <- rm$SOC

# and detrmine in which columns the covariate values are stored (column numbers)
# copy the covariates to a new data.frame
covar <- rm[,5:58]

## ----fit_model, message=F------------------------------------------------
# fit the model
rf <- randomForest(x = covar, y = d)

## ----inspect, , results='hide', message=F--------------------------------
# inspect the output
str(rf, max.level=2)

## ----getTree, message=F--------------------------------------------------
# inspect the top of the first tree in the forest.
head(getTree(rf, k=1, labelVar = TRUE))

## ----VIP, message=F------------------------------------------------------
varImpPlot(rf, main = "RF model - SOC, Macedonia")

## ----correlation_plot, message=F-----------------------------------------
plot(rf$predicted, rf$y, xlab="Predicted SOC (%)", ylab="Oberved SOC (%)")
abline(0,1)

## ----validation_stats, message=F-----------------------------------------
# MSE
round(rf$mse[rf$ntree],1)

# r-squared
round(rf$rsq[rf$ntree],2)

# RMSE
round(sqrt(rf$mse[rf$ntree]),1)

## ----cumulative_plots, message=F-----------------------------------------
plot(rf$mse, xlab = "trees", ylab = "MSE")
plot(rf$rsq, xlab = "trees", ylab = "Rsquared")

## ----complete_cases3, results='hide', message=F--------------------------
# exclude incomplete observations
r <- r[complete.cases(r),]

## ----predict, , results='hide', message=F--------------------------------
# predict
p.temp <- predict(rf, newdata = r)

# inspect
str(p.temp)
summary(p.temp)

## ----compile, , results='hide', message=F--------------------------------
# check the names of the columns storing the coordinates
names(r)

# compile predictions
p <- data.frame(x = r$s1, y = r$s2, SOC = p.temp)
head(p)

## ----make_spatial, message=F---------------------------------------------
# make spatial and project
gridded(p) <- ~x+y
proj4string(p) <- CRS("+init=epsg:6316")

# convert to a RasteLayer object.
p.r <- raster(p)

## ----plot1, message=F----------------------------------------------------
# plot with the sp package
spplot(p, zcol = "SOC")

# plot with the  raster package
plot(p.r)

## ----plot2, message=F----------------------------------------------------
# sp package
spplot(p, zcol = "SOC", at = c(0,1,2,3,4,5,10,20,40), scales = list(draw=TRUE), main = "Macedonia, SOC content (%)")
  
# or alternatively
plot(p.r, breaks = c(0,1,2,3,4,5,10,20,40), col = terrain.colors(8), main = "Macedonia, SOC content (%)")

## ----export, results='hide', warning=FALSE, message=F--------------------
# create output folder
dir.create(paste0(getwd(),"/out"))

# save as GeoTIFF
writeGDAL(p["SOC"], fname = "./out/SOC.tif", drivername = "GTiff", type = "Float32")

## ----convert_wgs84, message=F--------------------------------------------
# copy regression matrix to a new object
d.ll <- rm

# convert to spatial object amd set projection
coordinates(d.ll) <- ~X_coord+Y_coord
proj4string(d.ll) <- CRS("+init=epsg:6316")

# reproject to WGS84
d.ll <- spTransform(d.ll, CRSobj = CRS("+init=epsg:4326"))

## ----leaflet1, message=F-------------------------------------------------
# create a basic map
leaflet() %>%
  addTiles() %>%
  addMarkers(data = d.ll)

# creat pop-ups for markers (displaying the SOC content)
my_pops <- paste0(
   "<strong>Site: </strong>",
   d.ll$ProfID,
   '<br>
   <strong> SOC content (%): </strong>',
   round(d.ll$SOC,1)
)

# create interactive map
leaflet() %>%
   addProviderTiles("Esri.WorldImagery") %>%
   addMarkers(data = d.ll, popup = my_pops)

## ----leaflet2, message=F-------------------------------------------------
# define colour ramps for markers
pal1 <- colorQuantile("YlOrBr", domain = rm$SOC)
pal2 <- colorNumeric(SAGA_pal[[1]], domain = d.ll$SOC, na.color = "transparent")

# markers based on SOC quantiles
(ll1 <- leaflet() %>%
          addProviderTiles("Esri.WorldImagery") %>%
          addCircleMarkers(data = d.ll, color = ~pal1(SOC), popup = my_pops) %>%
          addLegend("bottomright", pal = pal1, values = d.ll$SOC,
                      title = "SOC content quantiles",
                      opacity = 0.8)
)

# markers based on SOC content
(ll2 <- leaflet() %>%
          addProviderTiles("Esri.WorldImagery") %>%
          addCircleMarkers(data = d.ll, color = ~pal2(SOC), popup = my_pops) %>%
          addLegend("bottomright", pal = pal2, values = d.ll$SOC,
                       title = "SOC content (%)",
                       opacity = 0.8)
)

## ----save_html, message=F------------------------------------------------
# save as HTML widget
saveWidget(ll1, file = paste0(getwd(),"/out/SOC_content.html",sep=""))

## ----leaflet3, message=F-------------------------------------------------
# define plot aesthetics: header and color scale
header <- "SOC content (%)"
pal <- colorNumeric(SAGA_pal[[1]], values(p.r), na.color = "transparent")

# create leaflet
(ll <- leaflet() %>% 
        addProviderTiles("Esri.WorldImagery") %>%
        addRasterImage(p.r, colors=pal, opacity=0.5) %>%
        addLegend(pal=pal, values=values(p.r), title=header)
)

## ----leaflet4, message=F-------------------------------------------------
# define color scale
pal <- colorBin(SAGA_pal[[1]], values(p.r), bins = c(0,1,2,3,4,5,10,20,40), na.color = "transparent")

# create leaflet
(ll <- leaflet() %>% 
        addProviderTiles("Esri.WorldImagery") %>%
        addRasterImage(p.r, colors=pal, opacity=0.5) %>%
        addLegend(pal=pal, values=values(p.r), title=header)
)
  
# save as widget
saveWidget(ll, file = paste0(getwd(),"/out/SOC_content_RF.html",sep=""))

## ----save_png, results='hide', message=F---------------------------------
png(filename = "./out/SOC_Macedonia.png", width = 600, height = 400, pointsize = 12)
spplot(p, zcol = "SOC", at = c(0,1,2,3,4,5,10,20,40), scales = list(draw=TRUE), main = "Macedonia, SOC content (%)")
dev.off()

## ----save_pdf, results='hide', message=F---------------------------------
pdf(file = "./out/SOC_Macedonia.pdf", width = 600, height = 400)
spplot(p, zcol = "SOC", at = c(0,1,2,3,4,5,10,20,40), scales = list(draw=TRUE), main = "Macedonia, SOC content (%)")
dev.off()

## ----save, results='hide', message=F-------------------------------------
# map
save(p, p.r, file="rfSOCmap.rda")

# model
save(rm,rf,file="randomForestModel.rda")

# environment
save.image("randomForest.rda")

# end of script;