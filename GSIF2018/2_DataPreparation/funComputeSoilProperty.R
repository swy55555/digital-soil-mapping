# Function to compute soil property values for a specified depth layer based on weighted averaging
# Version: 1.1, 24 May 2017
# Author: Dr. B. Kempen
# ISRIC - World Soil Information


# define function to derive carbon values for each of the four depth intervals
computeSoilProperty <- function(property,pointDataSet,upDepth,lowDepth,thickCond){
 
  # check for missing arguments
  if (missing(property))
    sDepthFrom("Need to specify the soil property.")
  
  if (missing(pointDataSet))
    sDepthFrom("Need to specify the point dataset.")
  
  if (missing(upDepth))
    sDepthFrom("Need to specify the upper depth of the soil layer of interest.")
  
  if (missing(lowDepth))
    sDepthFrom("Need to specify the lower depth of the soil layer of interest.")
  
  if (missing(thickCond))
    sDepthFrom("Need to specify the thickness treshold.")
  
 # copy arguments
 dum <- pointDataSet
 
 # redefining the ranges of the DepthTo and DepthFrom for each profile layer
 dum$upper <- ifelse(dum$DepthFrom <= upDepth, yes = upDepth, no = dum$DepthFrom)
 dum$upper <- ifelse(dum$DepthFrom >= lowDepth, yes = lowDepth, no = dum$upper)
 dum$lower <- ifelse(dum$DepthTo >= lowDepth, yes = lowDepth, no = dum$DepthTo)
 dum$lower <- ifelse(dum$DepthTo < upDepth, yes = upDepth, no = dum$lower)
 
 # determine the thickness for each individual layer (DepthTo-DepthFrom) of each profile
 dum$depth <- dum$lower - dum$upper
 
 # exclude layers outside depth range
 dum <- dum[dum$depth!=0,]
 
 # determine the total thickness for each unique profile
 totalThick <- tapply(X = dum$depth, INDEX = dum$ProfID, FUN = sum)
 
 # apply the thickness condition
 x <- data.frame(ProfID = names(totalThick), thickness = c(totalThick), row.names=NULL)
 x <- x[x$thickness >= thickCond,]
 x$ProfID <- as.character(x$ProfID)
 
 # Tables union
 dum <- join(x = dum, y = x, by = "ProfID", type = "left")
 
 # determine weights
 dum$weight <- dum$depth/dum$thickness
 
 # compute weighted average
 wAv<- ddply(
  .data = dum,
  .variables = c("ProfID", "X_coord", "Y_coord"),
  .fun = function(x){
   round(x[,property] %*% x$weight, 2)
  }
 )
 colnames(wAv)[4] <- c(property)
 wAv$ProfID <- as.character(wAv$ProfID)
 
 # exclude profiles without soil property value
 wAv <- wAv[!is.na(wAv[,property]),]
 
 return(wAv)
}
