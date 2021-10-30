############################################################################################
## R script belonging to the tutorial 'Validation of soil maps'
## Version: 2.0, 24 May 2018
## Author: Dr. B. Kempen, ISRIC - World Soil Information
## License: CC-BY-NC-SA
############################################################################################

# CHANGE THE WORK DIRECTORY!!
# DO NOT CHANGE THE NAME OF THE FOLDER '5_MachineLearning'

setwd("D:/_temp/8_Validation/")


## ----initialization, collapse=T, results='hide', message=F---------------
# empty memory and workspace
gc()
rm(list=ls())

# load libraries
require(randomForest)
require(caret)

## ----seed, collapse=T, results='hide', message=F-------------------------
# set random seed
set.seed(2011102)

## ----read_data, message=F------------------------------------------------
# read the random forest model
load("randomForestModel.rda")

## ----explore, results='hide', message=F----------------------------------
# explore input data
summary(rm)

str(rf, max.level = 1)

## ----error, results='hide', message=F------------------------------------
# compute prediction error
pe <- rf$predicted - rf$y

# statistical summary
summary(pe)

## ----accuracy_measures, message=F----------------------------------------
# ME
(me <- round(mean(pe),3))

# MSE
(mse <- round(mean(pe**2),2))

# RMSE
(rmse <- round(sqrt(mean(pe**2)),2))

# RMedSE
(rmedse <- round(sqrt(median(pe**2)),2))

# AVE
(ave <- round(1-(sum(pe**2)/(sum((rf$y-mean(rf$y))**2))),3))

# rsq computed from correldation coefficient
(rsqCor <- round((cor(rf$predicted, rf$y)**2),3))

## ----tabulate, message=F-------------------------------------------------
# create empty matrix
valstat <- matrix(nrow=6, ncol=1)

# add statistics
valstat <- rbind(me,mse,rmse,rmedse,ave,rsqCor)

# give row and column names
row.names(valstat) <- c('ME', 'MSE', 'RMSE', 'RMedSE', 'AVE', 'RSQ-COR')
colnames(valstat) <- c('value')

# export
write.csv(valstat, "rf_validation.csv")

## ----correlation_plot, message=F-----------------------------------------
# scatterplot
plot(rf$predicted, rf$y, xlab="Predicted SOC (%)", ylab="Oberved SOC (%)")
abline(0,1)

## ----randomForest_OOB, message=F-----------------------------------------
# MSE
round(rf$mse[rf$ntree],2)

# r-squared
round(rf$rsq[rf$ntree],3)

## ----predict_randomForest, message=F-------------------------------------
# predict at sampling sites
pred.ss <- predict(rf, newdata = rm)

# compute prediction error
pe2 <- pred.ss - rm$SOC

# compute RMSE 
round(sqrt(mean(pe2**2)),2)

# compute AVE
round(1-(sum(pe2**2)/(sum((rm$SOC-mean(rm$SOC))**2))),3)

## ----randomForest_OOB_caret, message=F-----------------------------------
# caret validation functions
round(RMSE(rf$predicted, rf$y),2)
round(R2(rf$predicted, rf$y),3)

## ----randomForest_OOB_caret2, message=F----------------------------------
# compare to squared correlation coefficient
R2(rf$predicted, rf$y)
cor(rf$predicted, rf$y)**2

## ----randomForest_OOB_caret3, message=F----------------------------------
# caret R2
R2(rf$predicted, rf$y, formula = "traditional")

# AVE
1-(sum(pe**2)/(sum((rf$y-mean(rf$y))**2)))

## ----createDataPartition, message=F--------------------------------------
# partition the dataset
trainIndex <- createDataPartition(rm$SOC, p = 0.7, list = FALSE, times = 1)

# inspect
head(trainIndex)

## ----subset, message=F---------------------------------------------------
# subset the dataset
socTrain <- rm[ trainIndex,]
socTest  <- rm[-trainIndex,]

## ----fit_model, results = 'hide', message=F------------------------------
# check the column names and numbers in the training object (covariates are stored in columns 5 to 63)
names(socTrain)

# copy soil property values to a new vector
dTrain <- socTrain$SOC

# copy the covariates to a new data.frame
covarTrain <- socTrain[,5:63]

# fit the model (takes a little while to run!)
rf.ds <- randomForest(x = covarTrain, y = dTrain, do.trace = 25)

## ----predict, message=F--------------------------------------------------
# predict at sampling sites
pred.ds <- predict(rf.ds, newdata = socTest)

# compute prediction error
pe.ds <- pred.ds - socTest$SOC

## ----accuracy_measures2, message=F---------------------------------------
# ME
(me.ds <- round(mean(pe.ds),3))

# MSE
(mse.ds <- round(mean(pe.ds**2),2))

# RMSE
(rmse.ds <- round(sqrt(mean(pe.ds**2)),2))

# RMedSE
(rmedse.ds <- round(sqrt(median(pe.ds**2)),2))

# AVE
(ave.ds <- round(1-(sum(pe.ds**2)/(sum((socTest$SOC-mean(socTest$SOC))**2))),3))

# rsq computed from correldation coefficient
(rsqCor.ds <- round((cor(pred.ds, socTest$SOC)**2),3))

## ----accuracy_measures3, message=F---------------------------------------
# OOB MSE
round(rf.ds$mse[500],2)

# OOB RMSE
round(sqrt(rf.ds$mse[500]),2)

# OOB R2
round(rf.ds$rsq[500],3)

## ----correlation_plot2, message=F----------------------------------------
# scatterplot
plot(pred.ds, socTest$SOC, xlab="Predicted SOC (%)", ylab="Oberved SOC (%)")
abline(0,1)

## ----fit_model2, results = 'hide', message=F-----------------------------
# copy soil property values to a new vectors
dTrain <- socTrain$SOC
dTest <- socTest$SOC

# copy the covariates to new data.frames
covarTrain <- socTrain[,5:63]
covarTest <- socTest[,5:63]

# fit the random forest model
rf.ds2 <- randomForest(x = covarTrain, y = dTrain, xtest = covarTest, ytest = dTest, do.trace = 25)

# inspect output
str(rf.ds2, max.level=2)

## ----accuracy_measures4, message=F---------------------------------------
# OOB MSE
round(rf.ds2$mse[500],2)

# OOB RMSE
round(sqrt(rf.ds2$mse[500]),2)

# OOB R2
round(rf.ds2$rsq[500],3)

## ----accuracy_measures5, message=F---------------------------------------
# OOB MSE
round(rf.ds2$test$mse[500],2)

# OOB RMSE
round(sqrt(rf.ds2$test$mse[500]),2)

# OOB R2
round(rf.ds2$test$rsq[500],3)

## ----trainControl, results = 'hide', message=F---------------------------
# create an object with the training parameters
cvPar <- trainControl(
  method = "cv",
  number = 5,
  verboseIter = TRUE,
  savePredictions = TRUE
)

# inspect
str(cvPar)

## ----train, results = 'hide', message=F----------------------------------
# copy soil property values to a new vectors
d <- rm$SOC

# copy the covariates to new data.frames
covar <- rm[,5:63]

# define the mtry parameter (1/3 of the number of covariates by default)
mtry <- data.frame(mtry = floor(ncol(covar)/3)) # or: rf$mtry

# cross-validation with caret package
rf.cv <- train(x = covar, y = d, method = "rf", trControl = cvPar, tuneGrid = mtry, do.trace=25)

## ----train_inspect, results = 'hide', message=F--------------------------
# inspect
str(rf.cv, max.level = 1)

# object class
class(rf.cv)

## ----cv_predictions, message=F-------------------------------------------
# cross validation predictions
str(rf.cv$pred)
head(rf.cv$pred)

## ----results_fold, message=F---------------------------------------------
# results per fold
rf.cv$resample

## ----results_aggregated, message=F---------------------------------------
# aggregated results
rf.cv$results

## ----finalModel, message=F-----------------------------------------------
# MSE
round(rf.cv$finalModel$mse[500], digits = 3)

# RMSE
round(sqrt(rf.cv$finalModel$mse[500]), digits = 3)

# R2 (= AVE for OOB)
round(rf.cv$finalModel$rsq[500], digits = 3)

## ----originalModel, message=F--------------------------------------------
# MSE
round(rf$mse[500], digits = 3)

# RMSE
round(sqrt(rf$mse[500]), digits = 3)

# R2 (= AVE for OOB)
round(rf$rsq[500], digits = 3)

## ----cvstats, message=F--------------------------------------------------
# MSE
round(mean((rf.cv$pred$pred-rf.cv$pred$obs)**2), digits = 3)

# RMSE
round(sqrt(mean((rf.cv$pred$pred-rf.cv$pred$obs)**2)), digits = 3)

# AVE
round(1-(sum((rf.cv$pred$pred-rf.cv$pred$obs)**2)/(sum((rf.cv$pred$obs-mean(rf.cv$pred$obs))**2))), digits = 3)

## ----order_append, message=F---------------------------------------------
# copy cross-validation predictions to new data.frame
cv.pred <- rf.cv$pred

# order
cv.pred <- cv.pred[order(cv.pred$rowIndex, decreasing = FALSE),]

# append to dataset
rm$cv.pred <- cv.pred$pred

# calculate prediction error 
rm$pe <- rm$cv.pred-rm$SOC

## ----save, message=F-----------------------------------------------------
# save various outputs
save.image("validation.rda")

