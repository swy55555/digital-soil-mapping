## ----general-options,echo=FALSE------------------------------------------

library(knitr)
# output code, but no warnings
opts_chunk$set(echo = TRUE,eval=TRUE,warning=FALSE)
# auto check dependencies (of cached chunks, its an approximation only)
opts_chunk$set(autodep = TRUE)
# dep_auto() # print dependencies 

setwd("~/teaching/2018_05-ISRIC_DSM-spring-school_machine-learning/exercises/")


## ----lasso-binary,fig.width=6,fig.height=3.5, fig.align='center', out.width='0.64\\textwidth',fig.cap = "Cross validation results for lasso fit on binary response.",message=FALSE,fig.pos="h"----
library(grpreg) # grouped lasso
library(geoGAM) # for the berne dataset
# Binary response, presence of waterlogging down to 100 cm (yes/no)
data(berne)
d.wlog100 <- berne[berne$dataset=="calibration"&!is.na(berne$waterlog.100), ]
d.wlog100 <- d.wlog100[complete.cases(d.wlog100[13:ncol(d.wlog100)]), ]
l.covar <- names(d.wlog100[, 13:ncol(d.wlog100)])

# define groups (the same code as for topsoil ph)
l.factors <- names(d.wlog100[l.covar])[ 
  t.f <- unlist( lapply(d.wlog100[l.covar], is.factor) ) ]
l.numeric <-  names(t.f[ !t.f ])
# create a vector that labels the groups with the same number  
g.groups <- c( 1:length(l.numeric), 
               unlist( 
                 sapply(1:length(l.factors), function(n){
        rep(n+length(l.numeric), nlevels(d.wlog100[, l.factors[n]])-1)
                 }) 
               ) 
)
# grpreg needs model matrix as input
XX <- model.matrix( ~., d.wlog100[, c(l.numeric, l.factors), F])[,-1]
# cross validation (CV) to find lambda
wlog.cvfit <- cv.grpreg(X = XX, y = d.wlog100$waterlog.100, 
                        group = g.groups, 
                        penalty = "grLasso",
                        returnY = T) # access CV results
plot(wlog.cvfit) # now, continue as with the topsoil pH example 

## ----convert-apply-to-for-1----------------------------------------------

library(geoGAM) # for the berne dataset

data(berne)
d.ph10 <- berne[berne$dataset == "calibration" & !is.na(berne$ph.0.10), ]
l.covar <- names(d.ph10[, 13:ncol(d.ph10)])

## 1. apply-function
## version 1
l.factors <- c()
for( ii in l.covar){
  # skip if it is not a factor
  # (the loop is not further executed)
  if( !is.factor(d.ph10[, ii]) ) next
  
  # append to vector
  l.factors <- c(l.factors, ii)
}

## version 2
l.factors <- c()
for( ii in l.covar){
  # append to vector, if index on ii is getting TRUE
  l.factors <- c(l.factors, ii[is.factor(d.ph10[, ii] ) ] )
}


## ----convert-apply-to-for-2----------------------------------------------
## 2. apply-function
l.numeric <-  names(t.f[ !t.f ])
g.groups <- 1:length(l.numeric)

for(ll in 1:length(l.factors)){
  # append number of levels of each factor to groups vector
  g.groups <- c(g.groups, 
        rep(ll+length(l.numeric), nlevels(d.ph10[, l.factors[ll]])-1 )) 
  
}

## ----convert-for-to-apply-in-recursive-backward-elimination,cache=TRUE----
library(randomForest) # for random forest
library(geoGAM) # to get the Berne data set

data(berne)
# continuous response, topsoil pH in 0-10 cm  
d.ph10 <- berne[berne$dataset == "calibration" & !is.na(berne$ph.0.10), ]
d.ph10 <- d.ph10[complete.cases(d.ph10[13:ncol(d.ph10)]), ]
l.covar <- names(d.ph10[, 13:ncol(d.ph10)])

rf.ph <- randomForest(x = d.ph10[, l.covar],
                      y = d.ph10$ph.0.10)

# simple example for the Fibonacci series 
f.fibo <- function(n) {
  
  if ( n < 2 ) {
    n
  } else {
    f.fibo(n-1) + f.fibo(n-2) 
  }
}
t.fibo <- sapply(0:26, f.fibo)


## now: backward elimination with a sapply
# create the sequence as before 
s.seq <- sort( c( seq(5, 95, by = 5), 
                  seq(100, length(l.covar), by = 10) ), 
               decreasing = T)

# only to parts of the calculation to save time
s.seq <- s.seq[1:5]

# at one more number at the beginning because we add the starting 
# model within the function
s.seq <- c(1,s.seq)

## first create a function, that we can pass to sapply 
# start.model = rf.ph -> hand over the first model to the function
# (would also work without, but the namespace in a function is a 
# different one than outside of it, this makes sure the model is found 
# inside the function)
f.back.elim <- function(ss, start.model = rf.ph){
  
  set.seed(1) # make random forest reproducible 
  # if you want to compare with the loop, refit with calling the same seed before
  
  if( ss == 1){
    # for the first iteration collect the starting model in a list and return
    return( list( qrf.elim = start.model, oob.mse = tail(start.model$mse, n=1) ))
    
  } else {
    
    # now just do the same as in the loop
    t.imp <- importance( f.back.elim(ss-1)[[1]] )
    t.imp <- t.imp[ order(t.imp[,1], decreasing = T),]
    
    rf.fit <- randomForest(x = d.ph10[, names(t.imp[1:s.seq[ss]])],
                           y = d.ph10$ph.0.10 )
    
    # collect the results in a list and return
    return( list( qrf.elim = rf.fit, oob.mse = tail(rf.fit$mse,n=1) ))
  }
}

# apply this function to the sequence
# do not simplify the results, so we can access it with the given names later
result.list <- sapply( 1:length(s.seq), FUN = f.back.elim, simplify = FALSE)

# the random forest model is then stored in:
result.list[[2]]$qrf.elim
# and the OOB error is stored in:
result.list[[2]]$oob.mse


## ----optimize-mtry-of-random-forest,cache=TRUE---------------------------

library(randomForest) # for random forest
library(geoGAM) # to get the Berne data set
library(caret) # for the tuning with train

data(berne)
# continuous response, topsoil pH in 0-10 cm  
d.ph10 <- berne[berne$dataset == "calibration" & !is.na(berne$ph.0.10), ]
d.ph10 <- d.ph10[complete.cases(d.ph10[13:ncol(d.ph10)]), ]
l.covar <- names(d.ph10[, 13:ncol(d.ph10)])

rf.ph <- randomForest(x = d.ph10[, l.covar],
                      y = d.ph10$ph.0.10)

# create grid of all possible mtry (maximum = number of covariates)
# rf.grid <- expand.grid(.mtry = 1:length(l.covar)) 
# only test part of it, otherwise too slow..
rf.grid <- expand.grid(.mtry = seq(5, length(l.covar), by = 50))

# do tuning with caret based on cross validation
rf.ph.tuned <- train(x = d.ph10[, l.covar],
                     y = d.ph10$ph.0.10,
                     method="rf",
                     tuneGrid = rf.grid, 
                     trControl = trainControl(method = "cv",number = 10) )

# print result
rf.ph.tuned$bestTune

# -> hint: mtry could also be choosen by the OOB errors yielded from every
#    randomForest fit. As far as I am aware, this is not implemented in caret,
#    but can easily be done by a for loop (or apply function), see the code
#    of the recursive backward elimination 


## ----boosted-trees-preparation,message=FALSE-----------------------------
# load data an packages
library(gbm) # for the boosting model
library(caret) # for the tuning with train
library(geoGAM) # for the berne dataset
library(raster) # for plotting as a raster

data(berne)
d.ph10 <- berne[berne$dataset == "calibration" & !is.na(berne$ph.0.10), ]
d.ph10 <- d.ph10[complete.cases(d.ph10[13:ncol(d.ph10)]), ]
l.covar <- names(d.ph10[, 13:ncol(d.ph10)])

## ----boosted-trees-tuning,cache=TRUE,fig.width=5,fig.height=5, fig.align='center', out.width='0.7\\textwidth',fig.cap = "Predictions computed with an optimized boosted trees model of topsoil pH (0--10 cm) for a small part of the Berne study region (white areas are streets, developped areas or forests)."----

# create a grid of the tuning parameters to be tested, 
# main tuning parameters are: 
gbm.grid <- expand.grid(
  # how many splits does each tree have
  interaction.depth = c(2,5,10,15,20),
  # how many trees do we add (number of iterations of boosting algorithm)
  n.trees = seq(2,250, by = 5),
  # put the shrinkage factor to 0.1 (=10% updates as used 
  # in package mboost), the default (0.1%) is a bit too small, 
  # makes model selection too slow. 
  # minimum number of observations per node can be left as is
  shrinkage = 0.1, n.minobsinnode = 10) 

# make tuning reproducible (there are random samples for the cross validation)
set.seed(291201945)

# train the gbm model 
gbm.model <- train(x=d.ph10[, l.covar ],
                   y=d.ph10[, "ph.0.10"],
                   method = "gbm", # choose "generalized boosted regression model"
                   tuneGrid = gbm.grid,
                   verbose = FALSE,
                   trControl = trainControl(
                     # use 10fold cross validation (CV)
                     method = "cv", number = 10,
                     # save fitted values (e.g. to calculate RMSE of the CV)
                     savePredictions = "final"))

# print optimal tuning parameter
gbm.model$bestTune

# compute predictions for the small part of the study area
# (agricultural land, the empty pixels are streets, forests etc.)
data("berne.grid")

berne.grid$pred <- predict.train(gbm.model, newdata = berne.grid )

# create a spatial object for a proper spatial plot
coordinates(berne.grid) <- ~x+y
# add the Swiss projection (see ?berne.grid)
proj4string(berne.grid) <- CRS("+init=epsg:21781")
# create a raster object from the spatial point dataframe 
gridded(berne.grid) <- TRUE
plot(raster(berne.grid, layer = "pred"))


## ----boosted-trees-partial-dependencies,fig.pos="h",fig.width=7,fig.height=7, fig.align='center', out.width='0.8\\textwidth',fig.cap = "Partial dependence plots of boosted trees model for the four most important covariates."----

# get variable importance
t.imp <- varImp(gbm.model$finalModel) 

# check how many covariates were never selected
sum( t.imp$Overall == 0 )

# order and select 4 most important covariates
t.names <- dimnames(t.imp)[[1]][ order(t.imp$Overall, decreasing = T)[1:4] ]

par(mfrow = c(2,2))
for( name in t.names ){
  # select index of covariate
  ix <- which( gbm.model$finalModel$var.names == name )
  plot(gbm.model$finalModel, i.var = ix)
}

# -> improve the plots by using the same y-axis (e.g. ylim=c(..,..)) 
#    for all of them, and try to add labels (xlab = , ylab = ) 
#    or a title (main = )


## ----example-knitr-------------------------------------------------------
# Remove the #s
# \documentclass{article}
# \begin{document}
# \section{Hello World}

# Calculate some basic operation and print the result:

# <<junk-with-some-code>>=
# a <- 1
# b <- 2
# (a + b)^b
# @

# We use the result in the text; it was \Sexpr{(a + b)^b}.
# \bigskip

# We create a random plot:
# <<junk-with-a-silly-figure>>=
# plot(rnorm(10),pch=1:10,col=1:10)
# @

# \end{document}

## ----session-info,results='asis'-----------------------------------------
toLatex(sessionInfo(), locale = FALSE)

## ----export-r-code,echo=FALSE--------------------------------------------
#purl("ISRIC-module-ML-2-training-solution.Rnw")

