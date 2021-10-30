rm(list = ls())  # clean memory

# load libraries
library(foreign)
library(sp)
library(rgdal)
library(maptools)
library(gstat)

# read Pb data
geul <- read.table("geuldata.txt", header = TRUE)
coordinates(geul) <- ~x+y

# define gstat object and compute variogram
gpb <- gstat(id = c("pb"), formula = pb~1, data = geul)
vgpb <- variogram(gpb, boundaries=c(50,100,150,200,300,400,600,800,1000))
vgmpb <- vgm(nugget = 5000, psill = 25000, range = 400, model = "Sph")
vgmpb <- fit.variogram(vgpb,vgmpb, fit.ranges=FALSE)
plot(vgpb,vgmpb, plot.numbers = TRUE)

# read mask 
mask <- readGDAL("geul_mask.txt")

# point kriging
geul.krig <- krige(pb~1, geul, newdata = mask, vgmpb)
geul.krig$var1.sd <- sqrt(geul.krig$var1.var)
spplot(geul.krig[c("var1.pred","var1.sd")], col.regions=bpy.colors())

# safe area based on expected values:
geul.krig$safe <- factor(ifelse(geul.krig$var1.pred*0.12 > 50, 
                                1, 0), labels=c("safe","hazard"))
spplot(geul.krig, zcol = "safe", xlim=c(190200,191300), 
       ylim=c(314300,315600), col.regions = c("green","red"),
       main = "Safe areas based on point kriging prediction")

# number of Monte Carlo runs
MC <- 100

# simulate lead concentration maps
geul.sim <- krige(pb~1, geul, newdata = mask, vgmpb, 
                  nsim = MC, nmax = 24)

# plot a few arbitrarily chosen maps for visual checking
spplot (geul.sim, zcol = c("sim1", "sim3", "sim8", "sim15"), 
        xlim=c(190200,191300), ylim=c(314300,315600),
        col.regions = bpy.colors())

# simulate soil consumption
set.seed(169)
MC <- 100
m <- 0.12
s <- 0.250
locat <- log(m) - 0.5 * log(1 + s**2/m**2)
scale <- sqrt(log(s**2/m**2 + 1))

sc <- rlnorm(MC, locat, scale)
mean(sc)
sd(sc)
hist(sc, breaks = "Freedman-Diaconis", xlim=c(0, 2), col="Lightblue")

# check effect of increase MC
set.seed(169)
MC <- 100000
m <- 0.12
s <- 0.250
locat <- log(m) - 0.5 * log(1 + s**2/m**2)
scale <- sqrt(log(s**2/m**2 + 1))

sc <- rlnorm(MC, locat, scale)
mean(sc); sd(sc)
hist(sc, breaks = "Freedman-Diaconis", xlim=c(0, 2), col="Lightblue")

MC <- 100  # reset MC to its original value

# run Monte Carlo uncertainty propagation
ingest <- matrix(nrow=6400,ncol=MC)
for (i in 1:MC)
  ingest[,i] <- sc[i] * geul.sim[[i]]

geul.sim$prob <- apply(ifelse(ingest > 50, 1,0), 1, mean)

spplot(geul.sim, zcol = "prob", col.regions = bpy.colors(), 
       xlim=c(190200,191300), ylim=c(314300,315600),
       main="P(ingestion > 50 µg Pb/day) from MC")

max(geul.sim$prob, na.rm = T)
min(geul.sim$prob, na.rm = T)

geul.sim$safe = factor(ifelse(geul.sim$prob > 0.05, 0, 1), 
                       labels=c("hazard","safe"))

# plot and save result
spplot(geul.sim, zcol = "safe", col.regions = c("red", "green"), 
       xlim=c(190200,191300), ylim=c(314300,315600),
       main="safe areas from MC; ingestion & map uncertain")

# proportion of area classified as 'safe'
mean(as.numeric(geul.sim$safe)-1, na.rm=T) 

# ONLY Pb concentration UNCERTAIN
MC <- 100
ingest2 <- matrix(nrow=6400, ncol=MC)
for (i in 1:MC)
  ingest2[,i] <- m * geul.sim[[i]]
geul.sim$prob2 <- apply(ifelse(ingest2 > 50, 1,0), 1, mean)
geul.sim$safe2 = factor(ifelse(geul.sim$prob2 > 0.05, 1, 0), 
                        labels=c("safe","hazard"))
geul.sim$var2 <- apply(ingest2, 1, var)

spplot(geul.sim, zcol = "safe2", col.regions = c("green", "red"), 
       xlim=c(190200,191300), ylim=c(314300,315600), 
       main=paste("Pb concentration uncertain, ",MC, "realizations"))

# ONLY soil consumption UNCERTAIN
ingest3 <- matrix(nrow=6400, ncol=MC)
for (i in 1:MC)
  ingest3[,i] <- sc[i] * geul.krig$var1.pred
geul.sim$prob3 <- apply(ifelse(ingest3 > 50, 1,0), 1, mean)
geul.sim$safe3 = factor(ifelse(geul.sim$prob3 > 0.05, 1, 0), 
                        labels=c("safe","hazard"))
geul.sim$var3 <- apply(ingest3, 1, var)
spplot(geul.sim, zcol = "safe3", col.regions = c("green", "red"), 
       xlim=c(190200,191300), ylim=c(314300,315600), 
       main=paste("Soil consumption uncertain, ",MC, "realizations"))

# contributions to total variance
geul.sim$PBcontr <- 100*geul.sim$var2/(geul.sim$var2+geul.sim$var3)
geul.sim$Scontr <- 100*geul.sim$var3/(geul.sim$var2+geul.sim$var3)
spplot(geul.sim, zcol = c("PBcontr","Scontr"), col.regions = bpy.colors(), 
       xlim=c(190200,191300), ylim=c(314300,315600), 
       main="Uncertainty contributions (per cent)")

# evaluating whether 2000 runs is enough
MC <- 2000

# first analysis
set.seed(784213)
sc <- rlnorm(MC, locat, scale)
geul.sim <- krige(pb~1, geul, newdata = mask, vgmpb, 
                  nsim = MC, nmax = 24)
ingest1 <- matrix(nrow=6400,ncol=MC)
for (i in 1:MC)
  ingest1[,i] <- sc[i] * geul.sim[[i]]

# second analysis
set.seed(647892)
sc <- rlnorm(MC, locat, scale)
geul.sim <- krige(pb~1, geul, newdata = mask, vgmpb, 
                  nsim = MC, nmax = 24)
ingest2 <- matrix(nrow=6400,ncol=MC)
for (i in 1:MC)
  ingest2[,i] <- sc[i] * geul.sim[[i]]

# merge results
geul.sim$prob1 <- apply(ifelse(ingest1 > 50, 1,0), 1, mean)
geul.sim$safe1 = factor(ifelse(geul.sim$prob1 > 0.05, 0, 1), 
                       labels=c("hazard","safe"))
geul.sim$prob2 <- apply(ifelse(ingest2 > 50, 1,0), 1, mean)
geul.sim$safe2 = factor(ifelse(geul.sim$prob2 > 0.05, 0, 1), 
                       labels=c("hazard","safe"))

spplot(geul.sim, zcol=c("safe1","safe2"), col.regions=c("red", "green"), 
  xlim=c(190200,191300), ylim=c(314300,315600),
  main="Safe areas, comparing two Monte Carlo analyses with 2000 runs")