#--------------------------------------------------------------------------
# Name:        ISRIC-module-ML-2-training.R
#
# Content:     Plain code, stripped from documentation ISRIC-module-ML-2-training.R. 
#              Exercises for ISRIC spring school on digital soil mapping
#              Module 2 on maching learning
#
# Author:      Madlene Nussbaum, BFH-HAFL
# Date:        Mai 2018
# Licence:     CC-BY-NC-SA
#--------------------------------------------------------------------------


###########################################################################
#### Preperation ----------------
###########################################################################


# DO NOT FORGET TO CHANGE THE WORKING DIRECTORY
setwd("D:/springschool/7_MachineLearning2/")


## ----load-packages,message=FALSE-----------------------------------------
# DO NOT FORGET TO INSTALL PACKAGES THAT ARE NOT INSTALLED YET

# install.packages(c("grpreg", "glmnet", "randomForest", "geoGAM"),
#                  dependencies = T)
library(grpreg) # for group lasso
library(glmnet) # for lasso
library(randomForest) # for random forest
library(geoGAM) # to get the Berne data set

## ----read-in-data--------------------------------------------------------
dim(berne)
data(berne)
# continuous response, topsoil pH in 0-10 cm  
d.ph10 <- berne[berne$dataset == "calibration" & !is.na(berne$ph.0.10), ]
d.ph10 <- d.ph10[complete.cases(d.ph10[13:ncol(d.ph10)]), ]

# ordered/multinomial tesponse, degree of waterlogging, 
# 3 levels aggregated from subqualifiers of Swiss soil classification  
d.drain <- berne[berne$dataset == "calibration" & !is.na(berne$dclass), ]
d.drain <- d.drain[complete.cases(d.drain[13:ncol(d.drain)]), ]

# covariates start at col 13 (see help page ?berne)
l.covar <- names(d.ph10[, 13:ncol(d.ph10)])



###########################################################################
#### Lasso - linear shrinkage method ----------------
###########################################################################


## ----apply-example-------------------------------------------------------
# loop 
# first create a vector to save the results
t.result <- c()
for( ii in 1:10){ t.result[ii] <- ii^2}

# the same as apply
t.result <- sapply(1:10, function(ii){ ii^2 })

# of course, this example is even shorter using:
t.result <- (1:10)^2

## ----lasso-continuous-response,cache=TRUE--------------------------------

# define groups: dummy coding of a factor is treated as group
# find factors
l.factors <- names(d.ph10[l.covar])[ 
  t.f <- unlist( lapply(d.ph10[l.covar], is.factor) ) ]
l.numeric <-  names(t.f[ !t.f ])

# create a vector that labels the groups with the same number  
g.groups <- c( 1:length(l.numeric), 
               unlist( 
                 sapply(1:length(l.factors), function(n){
                   rep(n+length(l.numeric), nlevels(d.ph10[, l.factors[n]])-1)
                 }) 
               ) 
)

# grpreg needs model matrix as input
XX <- model.matrix( ~., d.ph10[, c(l.numeric, l.factors), F])[,-1]

# cross validation (CV) to find lambda
ph.cvfit <- cv.grpreg(X = XX, y = d.ph10$ph.0.10, 
                      group = g.groups, 
                      penalty = "grLasso",
                      returnY = T) # access CV results

## ----lasso-predictions---------------------------------------------------
# choose optimal lambda: CV minimum error + 1 SE (see glmnet)
l.se <- ph.cvfit$cvse[ ph.cvfit$min ] + ph.cvfit$cve[ ph.cvfit$min ]
idx.se <- min( which( ph.cvfit$cve < l.se ) ) - 1

# select validation data
d.ph10.val <- berne[berne$dataset == "validation" & !is.na(berne$ph.0.10), ]
d.ph10.val <- d.ph10.val[complete.cases(d.ph10.val[13:ncol(d.ph10)]), ]

# create model matrix for validation set
newXX <- model.matrix( ~., d.ph10.val[, c(l.factors, l.numeric), F])[,-1]
t.pred.val <-  predict(ph.cvfit, X = newXX, 
                       type = "response",
                       lambda =  ph.cvfit$lambda[idx.se])

# get CV predictions, e.g. to compute R2
ph.lasso.cv.pred <- ph.cvfit$Y[,idx.se]

## ----lasso-get-model-----------------------------------------------------
# get the non-zero coefficients:
t.coef <- ph.cvfit$fit$beta[, idx.se ]
t.coef[ t.coef > 0 ]

## ----lasso-plot-cv -------
plot(ph.cvfit)
abline( h = l.se, col = "grey", lty = "dotted")
abline( v = log( ph.cvfit$lambda[ idx.se ]), col = "grey30", lty = "dotted")

## ----lasso-multinomial-response,cache = TRUE-----------------------------

# create model matrix for drainage classes
# use a subset of covariates only, because model optimization for 
# multinomial takes long otherwise

set.seed(42) # makes sample() reproducible
XX <- model.matrix(~.,d.drain[, l.covar[sample(1:length(l.covar), 30)]])[,-1]

drain.cvfit <- cv.glmnet( XX, d.drain$dclass, nfold = 10,  
                          keep = T, # access CV results
                          family = "multinomial", 
                          type.multinomial = "grouped")

## ----lasso-multinomial-response-coeffs,cache=TRUE------------------------

drain.fit <- glmnet( XX, d.drain$dclass,
                     family = "multinomial", 
                     type.multinomial = "grouped",
                     lambda = drain.cvfit$lambda.min)
# The coeffs are here:
# drain.fit$beta$well
# drain.fit$beta$moderate
# drain.fit$beta$poor



###########################################################################
#### Selection of covariates by random forest ----------------
###########################################################################


## ----fit-random-forest,cache=TRUE----------------------------------------
rf.ph <- randomForest(x = d.ph10[, l.covar],
                      y = d.ph10$ph.0.10)

## ----plot-covar-importance -------
varImpPlot(rf.ph, n.var = 20, main = "")

## ----select-random-forest,cache=TRUE-------------------------------------
# we could remove one covariate after another, this would take long...
# speed up the process by removing 5-10 covariates at a time,
# create a sequence of covariate numbers for this
s.seq <- sort( c( seq(5, 95, by = 5), 
                  seq(100, length(l.covar), by = 10) ), 
               decreasing = T)

# collect results in list
qrf.elim <- oob.mse <- list()

# save model and OOB error of current fit        
qrf.elim[[1]] <- rf.ph
oob.mse[[1]] <- tail(qrf.elim[[1]]$mse, n=1)
l.covar.sel <- l.covar

# iterate through number of retained covariates           
for( ii in 1:length(s.seq) ){
  # compute importance and order it
  t.imp <- importance(qrf.elim[[ii]])
  t.imp <- t.imp[ order(t.imp[,1], decreasing = T),]
  
  # fit random forest (RF) to reduced covariate set
  # only select as many as in s.seq defined above
  qrf.elim[[ii+1]] <- randomForest(x = d.ph10[, names(t.imp[1:s.seq[ii]])],
                                   y = d.ph10$ph.0.10 )
  # save OOB error from RF fit
  oob.mse[[ii+1]] <- tail(qrf.elim[[ii+1]]$mse,n=1)
}

# prepare a data frame for plot
elim.oob <- data.frame(elim.n = c(length(l.covar), s.seq[1:length(s.seq)]),                        elim.OOBe = unlist(oob.mse) )

## ----plot-selection-path ------ ---------
plot(elim.oob$elim.n, elim.oob$elim.OOBe, 
     ylab = "OOB error (MSE)",
     xlab = "n covariates", 
     pch = 20)
abline(v = elim.oob$elim.n[ which.min(elim.oob$elim.OOBe)], lty = "dotted")



###########################################################################
#### Model interpretation ----------------
###########################################################################


## ----partial-residual-plots-lm-lasso --------------------
# create a linear model (example, with covariates from lasso)
ols <- lm( ph.0.10 ~ timeset + ge_geo500h3id + cl_mt_gh_4 + 
             tr_se_curvplan2m_std_25c, data = d.ph10 ) 
par(mfrow = c(1,2)) # two plots on same figure

# residual plot for covariate cl_mt_gh_4
termplot(ols, partial.resid = T, terms = "cl_mt_gh_4",
         ylim = c(-2,2),
         main = "Ordinary Least Squares")
abline(h=0, lty = 2)

## Create partial residual plot for lasso 
# there is no direct function available, but we can easily 
# construct the plot with
# y-axis: residuals + effect of term (XBi), scaled
# x-axis: values covariate
# regression line: model fit of axis y~x 

# get the index of the covariate
idx <- which(names(t.coef) == "cl_mt_gh_4" )

# residuals of lasso model chosen above
residuals <- d.ph10$ph.0.10 - ph.cvfit$Y[,idx.se] 
# prediction for this covariate XBi
Xbeta <- ph.cvfit$fit$beta[idx, idx.se] * d.ph10$cl_mt_gh_4
# calculate partial residuals and center with mean
part.resid <- scale(residuals + Xbeta, scale = F)[,1]

# plot with similar settings
plot(d.ph10$cl_mt_gh_4, 
     part.resid, pch = 1, col = "grey",
     ylim = c(-2,2),
     ylab = "partial residuals [%]", xlab = "cl_mt_gh_4",
     main = "Lasso")
abline(lm(part.resid ~ d.ph10$cl_mt_gh_4), col = "red")
abline(h=0, lty = 2)

## ----partial-dep-rf,fig.pos="h",fig.width=7,fig.height=8, fig.align='center', out.width='0.75\\textwidth',fig.cap = "Partial dependence plots for the 4 most important covariates (cl\\_mt\\_rr\\_xx: monthly or yearly rainfall in mm*10)."----
# select the model with minimum OOB error
rf.selected <- qrf.elim[[ which.min(elim.oob$elim.OOBe)]]

t.imp <- importance(rf.selected, type = 2)
t.imp <- t.imp[ order(t.imp[,1], decreasing = T),]

# select 4 most important covariates for a plot
( t.3 <- names( t.imp[ 1:4 ] ) )

par( mfrow = c(2,2))
# Bug in partialPlot(): function does not allow a variable for the 
# covariate name (e. g. x.var = name) in a loop, hence repeat the code
partialPlot(x = rf.selected, 
            pred.data = d.ph10[, names(rf.selected$forest$xlevels)], 
            x.var = "cl_mt_rr_3", ylab = "ph [-]", main = "") 
partialPlot(x = rf.selected, 
            pred.data = d.ph10[, names(rf.selected$forest$xlevels)], 
            x.var = "cl_mt_rr_11", ylab = "ph [-]", main = "" ) 
partialPlot(x = rf.selected, 
            pred.data = d.ph10[, names(rf.selected$forest$xlevels)], 
            x.var = "timeset", ylab = "ph [-]", main = "" ) 
partialPlot(x = rf.selected, 
            pred.data = d.ph10[, names(rf.selected$forest$xlevels)], 
            x.var = "cl_mt_rr_y", ylab = "ph [-]", main = "" ) 

# end of script;