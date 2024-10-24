################################
## Install and load packages ###
################################

install.packages("caret")
install.packages("tidyverse")
install.packages("readr")
install.packages("pROC")
install.packages("ggplot2")
install.packages("cvms")
library(caret)
library(tidyverse)
library(readr)
library(pROC)
library(ggplot2)
library(cvms)

###############
# Import data #
###############

setwd('C:/Users/santi/OneDrive/Documentos/CURSO DATA ANALYTICS/Cours IV/Course 4 Task 3/Data/UJIndoorLoc/UJIndoorLoc')

##-- Load Train data (Dataset 1) --##

Dtrain <- read.csv("trainingData.csv", stringsAsFactors = FALSE)

Dvalid <- read.csv("validationData.csv", stringsAsFactors = FALSE)

#################
# Evaluate data #
#################

##--- Dtrain ---##

head(Dtrain)
tail(Dtrain)

##############
# Preprocess #
##############

##--- Dtrain ---##

anyNA(Dtrain)
anyDuplicated(Dtrain)

# Removing duplicated Rows (#637 duplicated)

Dtrain <- distinct(Dtrain)

# Put the important variables at the beginning

Dtrain <- Dtrain %>% relocate(LONGITUDE, .before = WAP001)
Dtrain <- Dtrain %>% relocate(LATITUDE, .before = WAP001)
Dtrain <- Dtrain %>% relocate(FLOOR, .before = WAP001)
Dtrain <- Dtrain %>% relocate(BUILDINGID, .before = WAP001)
Dtrain <- Dtrain %>% relocate(SPACEID, .before = WAP001)
Dtrain <- Dtrain %>% relocate(RELATIVEPOSITION, .before = WAP001)
Dtrain <- Dtrain %>% relocate(USERID, .before = WAP001)
Dtrain <- Dtrain %>% relocate(PHONEID, .before = WAP001)
Dtrain <- Dtrain %>% relocate(TIMESTAMP, .before = WAP001)

str(Dtrain)

## Create single unique identifier for each location in the data set 

Dtrain$LOCATION <- paste(Dtrain$BUILDINGID, Dtrain$FLOOR, Dtrain$SPACEID, Dtrain$RELATIVEPOSITION, sep="_")
Dtrain <- Dtrain %>% relocate(LOCATION, .before = WAP001)

## Convert required variables to factor

Dtrain[, c("SPACEID", "RELATIVEPOSITION", "BUILDINGID", "FLOOR")] <- lapply(Dtrain[, c("SPACEID", "RELATIVEPOSITION", "BUILDINGID", "FLOOR")], factor)
str(Dtrain)


################
### Sampling ###
################

## Sample with building 1 and floor 3

factor <- c('green', 'red', 'blue')
plot(Dtrain$BUILDINGID, main ='Sample per building', col = factor)

DtrainB1 <- filter(Dtrain, BUILDINGID == 2)


str(DtrainB1)

plot(Dtrain1$FLOOR, main ='Sample per floor', col = Dtrain1$FLOOR )

DtrainB1F1 <- filter(DtrainB1, FLOOR == 3)


str(DtrainB1F1)

# Remove all columns that have a variance of zero.

nzv_vals <- nearZeroVar(DtrainB1, saveMetrics = TRUE)
DtrainB1 <- DtrainB1[ - which(nzv_vals$zeroVar==TRUE)]

nzv_vals1 <- nearZeroVar(DtrainB1F1, saveMetrics = TRUE)
DtrainB1F1 <- DtrainB1F1[ - which(nzv_vals1$zeroVar==TRUE)]

# Leave variables of known location

drop <- c("LATITUDE", "LONGITUDE", "SPACEID", "RELATIVEPOSITION", "BUILDINGID", "FLOOR", "USERID", "PHONEID", "TIMESTAMP")
DtrainB1 <- DtrainB1[,!(names(DtrainB1) %in% drop)]

str(DtrainB1)

drop1 <- c("LATITUDE", "LONGITUDE", "SPACEID", "RELATIVEPOSITION", "BUILDINGID", "FLOOR", "USERID", "PHONEID", "TIMESTAMP")
DtrainB1F1 <- DtrainB1F1[,!(names(DtrainB1F1) %in% drop1)]

str(DtrainB1F1)

##

DtrainB1F1$LOCATION <- as.factor(DtrainB1F1$LOCATION)
DtrainB1$LOCATION <- as.factor(DtrainB1$LOCATION)

###################
##Train/test sets##
###################

##  Building 2 

set.seed(123) 
inTraining <- createDataPartition(DtrainB1$LOCATION, p=0.75, list=FALSE)
oobTrain <- DtrainB1[inTraining,]   
oobTest <- DtrainB1[-inTraining,]   
nrow(oobTrain)
nrow(oobTest)

##  Building 2 and floor 3

set.seed(123) 
inTraining1 <- createDataPartition(DtrainB1F1$LOCATION, p=0.75, list=FALSE)
oobTrain1 <- DtrainB1F1[inTraining1,]   
oobTest1 <- DtrainB1F1[-inTraining1,]   
nrow(oobTrain1)
nrow(oobTest1)

#################
##Train control##
#################

fitControl <- trainControl(method="repeatedcv", number=10, repeats=3) 

################
##Train models##
################

modelLookup('kknn')
modelLookup('C5.0')
modelLookup('rf')

## ------- KNN ------- ##

##  Building 2 

set.seed(98)
oobKKNNfit <- train(LOCATION~., data=oobTrain, method="kknn", importance=T, trControl=fitControl)

oobKKNNfit
plot(oobKKNNfit)
varImp(oobKKNNfit)


#k-Nearest Neighbors 

#6956 samples
#203 predictor
#403 classes

#No pre-processing
#Resampling: Cross-Validated (10 fold, repeated 3 times) 
#Summary of sample sizes: 6259, 6251, 6270, 6252, 6259, 6267, ... 
#Resampling results across tuning parameters:
  
#  kmax  Accuracy   Kappa    
#5     0.6740956  0.6730452
#7     0.6740956  0.6730450
#9     0.6740956  0.6730450

#Tuning parameter 'distance' was held constant at a value of 2
#Tuning parameter 'kernel' was held constant at a value of optimal
#Accuracy was used to select the optimal model using the largest value.
#The final values used for the model were kmax = 9, distance = 2 and kernel = optimal.

##  Building 2 and floor 3

set.seed(98)
oobKKNNfit1 <- train(LOCATION~., data=oobTrain1, method="kknn", importance=T, trControl=fitControl)

oobKKNNfit1
plot(oobKKNNfit1)
varImp(oobKKNNfit1)
getTrainPerf(oobKKNNfit1)

##k-Nearest Neighbors 

#2058 samples
#181 predictor
#110 classes

#No pre-processing
#Resampling: Cross-Validated (10 fold, repeated 3 times) 
#Summary of sample sizes: 1848, 1861, 1854, 1851, 1849, 1853, ... 
#Resampling results across tuning parameters:
  
#kmax  Accuracy   Kappa    
#5     0.6986871  0.6951385
#7     0.6986871  0.6951385
#9     0.6986871  0.6951385

#Tuning parameter 'distance' was held constant at a value of 2
#Tuning parameter 'kernel' was held constant at a value of optimal
#Accuracy was used to select the optimal model using the largest value.
#The final values used for the model were kmax = 9, distance = 2 and kernel = optimal.


## ------- C5.0 ------- ##

##  Building 2

set.seed(98)
oobC5fit <- train(LOCATION~., data=oobTrain, method="C5.0", importance=T, trControl=fitControl)

oobC5fit
plot(oobC5fit)
varImp(oobC5fit)


##  Building 2 and floor 3

set.seed(98)
oobC5fit1 <- train(LOCATION~., data=oobTrain1, method="C5.0", importance=T, trControl=fitControl)

oobC5fit1
plot(oobC5fit1)
varImp(oobC5fit1)
getTrainPerf(oobC5fit1)


##C5.0 

#2058 samples
#178 predictor
#110 classes

#No pre-processing
#Resampling: Cross-Validated (10 fold, repeated 3 times) 
#Summary of sample sizes: 1848, 1861, 1854, 1851, 1849, 1853, ... 
#Resampling results across tuning parameters:
  
#model  winnow  trials  Accuracy   Kappa    
#rules  FALSE    1      0.5980116  0.5932764
#rules  FALSE   10      0.7505888  0.7476915
#rules  FALSE   20      0.7751965  0.7725731
#rules   TRUE    1      0.6043714  0.5997157
#rules   TRUE   10      0.7468295  0.7438800
#rules   TRUE   20      0.7692576  0.7665722
#tree   FALSE    1      0.6069181  0.6023513
#tree   FALSE   10      0.7451731  0.7421903
#tree   FALSE   20      0.7695627  0.7668698
#tree    TRUE    1      0.6096824  0.6051387
#tree    TRUE   10      0.7442494  0.7412575
#tree    TRUE   20      0.7608201  0.7580175

#Accuracy was used to select the optimal model using the largest value.
#The final values used for the model were trials = 20, model = rules and winnow = FALSE.

## ------- Rf ------- ##

##  Building 2

set.seed(123)
oobRFfit <- train(LOCATION~., data=oobTrain, method="rf", importance=T, trControl=fitControl)

oobRFfit
plot(oobRFfit)
varImp(oobRFfit)


##  Building 2 and floor 3

set.seed(123)
oobRFfit1 <- train(LOCATION~., data=oobTrain1, method="rf", importance=T, trControl=fitControl)

oobRFfit1
plot(oobRFfit1)
varImp(oobRFfit1)
getTrainPerf(oobRFfit1)


#Random Forest 

#2058 samples
#178 predictor
#110 classes

#No pre-processing
#Resampling: Cross-Validated (10 fold, repeated 3 times) 
#Summary of sample sizes: 1852, 1855, 1847, 1848, 1849, 1851, ... 
#Resampling results across tuning parameters:
  
#mtry  Accuracy   Kappa    
#2   0.2944740  0.2813576
#90   0.8391688  0.8372648
#178   0.8192373  0.8171020


#Accuracy was used to select the optimal model using the largest value.
#The final value used for the model was mtry = 90.

####################
##Model selection###
####################

Selectmodel <- resamples(list(knn=oobKKNNfit1, C5.0=oobC5fit1, rf=oobRFfit1))

# output summary metrics for tuned models

summary(Selectmodel)

#Call:
#  summary.resamples(object = Selectmodel)

#Models: knn, C5.0, rf 
#Number of resamples: 30 

#Accuracy 
#          Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
#knn  0.6525822 0.6803904 0.6961673 0.6986871 0.7184969 0.7536232    0
#C5.0 0.7101449 0.7615432 0.7737283 0.7751965 0.7861258 0.8421053    0
#rf   0.7894737 0.8150415 0.8365274 0.8391688 0.8605004 0.8926829    0

#Kappa 
#          Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
#knn  0.6486715 0.6765369 0.6926401 0.6951385 0.7152034 0.7507967    0
#C5.0 0.7067850 0.7587475 0.7710799 0.7725731 0.7836652 0.8403657    0
#rf   0.7869274 0.8128780 0.8345944 0.8372648 0.8588429 0.8914299    0



##--- Save/load top performing model ---##

# save top performing model after validation
saveRDS(oobRFfit1, "oobRFfit1.rds")  

# load and name model
SelectModelRF <- readRDS("oobRFfit1.rds")

###################
# Predict test-set#
###################

# predict with RF

RFPred1 <- predict(oobRFfit1, oobTest1)

# performace measurment

postResample(RFPred1, oobTest1$LOCATION)

# plot predicted verses actual

plot(RFPred1, oobTest1$LOCATION)
RFPred1
plot(RFPred1)

cfm <- confusion_matrix( targets = RFPred1, predictions = oobTest1$LOCATION)
cfm

plot_confusion_matrix(cfm)



  
  