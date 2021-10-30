rm(list=ls()) # clean memory

# load libraries
library(sp)
library(rgdal)
library(maptools)
library(gstat)
library(rgeos)
library(MASS)

## ------------------------------------------------------------------------
geul <- read.table("geuldata.txt", header = TRUE)

## ----results='hide'------------------------------------------------------
dim(geul)
names(geul)
summary(geul)
class(geul)

# explore the data
geul
geul$pb
mean(geul$pb)
median(geul$pb)
min(geul$x); max(geul$x)
hist(geul$pb, main = "Topsoil lead concentration Geul valley", 
     col="LightBlue")

# make spatial
class(geul)
coordinates(geul) <- ~x+y
class(geul)

# read boundary study area and riverline
studarea <- readOGR("mask_aoi_shp.shp")
riverline <- readOGR("river_line.shp")

# plot observations
spplot(geul, zcol = "pb", xlim = c(190000,192000),
       ylim = c(314000,316000), cex = 1.5, main = "Pb data",
       key.space = list(x = 0.02, y = 0.26, corner = c(0,1)),
       sp.layout = list(list("sp.polygons", studarea),
       list("sp.lines", riverline, col="red", lwd=2)), 
       scales=list(draw=T), col.regions = bpy.colors(5))

# define gstat object and compute experimental semivariogram
gpb <- gstat(formula = pb~1, data = geul)
vgpb <- variogram(gpb)
plot(vgpb, plot.nu=FALSE)

# define initial semivariogram model
vgmpb <- vgm(nugget = 5000, psill = 25000, range = 400, model = "Sph")
plot(vgpb, vgmpb)

# fit semivariogram model
vgmpb <- fit.variogram(vgpb, vgmpb, fit.method=7)
plot(vgpb, vgmpb)
vgmpb
attr(vgmpb, "SSErr")

# read prediction grid
mask <- readGDAL("geul_mask.txt")

# ordinary point kriging
geul.krig <- krige(formula = pb~1, locations = geul, newdata = mask, 
                   model = vgmpb)
names(geul.krig)

spplot(geul.krig, zcol = "var1.pred", col.regions = bpy.colors(), 
       xlim=c(190200,191300), ylim=c(314300,315600), 
       main="Pb predictions [ppm]")

geul.krig$var1.sd <- sqrt(geul.krig$var1.var)
spplot(geul.krig, zcol = "var1.sd", col.regions = bpy.colors(), 
       main="st dev [ppm]", xlim=c(190200,191300), ylim=c(314300,315600),
       sp.layout=list("sp.points",geul, pch=1, cex=2))
max(geul.krig$var1.sd, na.rm=T)
min(geul.krig$var1.sd, na.rm=T)

# IDW when no semivariogram model specified
geul.idw <- krige(formula = pb~1, locations = geul, newdata = mask)
names(geul.idw)
spplot(geul.idw, zcol = "var1.pred", col.regions = bpy.colors(), 
       xlim=c(190200,191300), ylim=c(314300,315600), 
       main="IDW Pb predictions [ppm]")
summary(geul.idw$var1.var)

# spatial stochastic simulation
geul.sim <- krige(pb~1, geul, newdata = mask, vgmpb, nsim = 9, nmax = 24)
names(geul.sim)
spplot(geul.sim, zcol = "sim1", xlim=c(190200,191300), 
      ylim=c(314300,315600), col.regions = bpy.colors())
spplot(geul.sim[c(4:6,1:3)], xlim=c(190200,191300), 
      ylim=c(314300,315600), col.regions = bpy.colors())

dem <- readGDAL("geul_dem.txt")
dist <- readGDAL("river_dist.txt")
slope <- readGDAL("geul_slope.txt")   

# add explanatory data to geul object and inspect correlations
geul$elev <- over(geul, dem)[[1]] # we need data from data frame
geul$riverdist <- over(geul, dist)$band1 # alternative way
geul$slope <- over(geul, slope)$band1

# exploratory scatterplots
plot(geul$elev, geul$pb) 
plot(geul$riverdist, geul$pb)
plot(geul$slope, geul$pb)

# correlation coeffcients
cor(geul@data)

# fit a linear regression model and inspect the results
geul.lm <- lm(pb~elev+riverdist+slope, data = geul)
summary(geul.lm)

# refit the model
geul.lm <- lm(pb~elev+riverdist, data = geul)
summary(geul.lm)
str(geul.lm)

# append residuals to geul dataset
geul$residuals <- geul.lm$residuals
names(geul)
summary(geul)

# define gstat object and compute experimental semivariogram
gpb2 <- gstat(formula = residuals~1, data = geul)
vgpb2 <- variogram(gpb2)
plot(vgpb2, plot.nu=FALSE)

# define initial semivariogram model
vgmpb2 <- vgm(nugget = 5000, psill = 15000, range = 200, model = "Exp")
plot(vgpb2, vgmpb2)

# fit semivariogram model
vgmpb2 <- fit.variogram(vgpb2, vgmpb2, fit.method=7)
plot(vgpb2, vgmpb2)
vgmpb2

# check if the residuals approximately follow a normal distribution
hist(geul$residuals, col="lightblue") # here they do so there is no need for a transformation

# convert to data.frame to predict the trend
mask$elev <- dem$band1
mask$riverdist <- dist$band1
mask.df <- as.data.frame(mask)

# regression prediction
geul.trend <- predict(geul.lm, newdata = mask.df)

# set predictions outside mask to NA
geul.trend <- ifelse(test = is.na(mask$band1), yes = NA, no = geul.trend)

# check predictions
summary(geul.trend)
hist(geul.trend, col="lightblue")

# set predictions smaller than 0 to 0
geul.trend <- ifelse(geul.trend<0, yes = 0, no = geul.trend)

# krige the residuals
geul.rk <- krige(formula = residuals~1, locations = geul,  beta = 0, 
                 newdata = mask, model = vgmpb2)
names(geul.rk)[1] <- "resid"
geul.rk$trend <- geul.trend

# set kriged residuals and variance outside mask to NA
geul.rk$resid <- ifelse(test = is.na(mask$band1), yes = NA, 
                        no = geul.rk$resid)
geul.rk$var1.var <- ifelse(test = is.na(mask$band1), yes = NA, 
                           no = geul.rk$var1.var)

# obtain RK prediction
geul.rk$predict <- geul.rk$trend + geul.rk$resid

# set predictions smaller than 0 to 0
geul.rk$predict  <- ifelse(geul.rk$predict < 0, yes = 0, 
                           no = geul.rk$predict )
# plot the RK predictions
spplot(geul.rk, zcol = "predict", col.regions = bpy.colors(), 
       xlim=c(190200,191300), ylim=c(314300,315600), 
       main="RK Pb predictions [ppm]")

# compute the kriging standard deviation
geul.rk$var1.sd <- sqrt(geul.rk$var1.var)
spplot(geul.rk, zcol = "var1.sd", col.regions = bpy.colors(), 
       xlim=c(190200,191300), ylim=c(314300,315600), 
       main="st dev [ppm]", 
       sp.layout=list("sp.points",geul, pch=1, cex=2))

spplot(geul.krig, zcol = "var1.sd", col.regions = bpy.colors(),
       xlim=c(190200,191300), ylim=c(314300,315600), 
       main="st dev [ppm]", 
       sp.layout=list("sp.points",geul, pch=1, cex=2))

# universal kriging
geul.uk <- krige(formula = pb ~ elev + riverdist, locations = geul,  
                 newdata = mask, model = vgmpb2)
names(geul.uk)[1] <- "predict"

geul.uk$predict  <- ifelse(geul.uk$predict <0, yes = 0, 
                           no = geul.uk$predict )
geul.uk$var1.var <- ifelse(test = is.na(mask$band1), yes = NA, 
                           no = geul.uk$var1.var)

spplot(geul.uk, zcol = "predict", col.regions = bpy.colors(), 
       xlim=c(190200,191300), ylim=c(314300,315600), 
       main="UK Pb predictions [ppm]")

geul.uk$var1.sd <- sqrt(geul.uk$var1.var)
spplot(geul.uk, zcol = "var1.sd", col.regions = bpy.colors(), 
       xlim=c(190200,191300), ylim=c(314300,315600), 
       main="UK st dev [ppm]", 
       sp.layout=list("sp.points",geul, pch=1, cex=2))

# compare RK and UK (predictions and variances)
summary(geul.uk$predict)
summary(geul.rk$predict)

summary(geul.uk$var1.var)
summary(geul.rk$var1.var)