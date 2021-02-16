

############################## PREPARE WORKSPACE ###############################

rm(list=ls()) # clean everything done before

packages <- c("caret","Hmisc","dplyr","rattle","ggplot2","lubridate") # select packages
lapply(packages, require, character.only = TRUE) # load packages

################################### GET DATA ###################################

# store URLs as objects
url1 <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
url2 <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

# download files
download.file(url1, destfile = "./dataAll.csv")
download.file(url2, destfile = "./test.csv")

# read data
dataAll <- read.csv2("./dataAll.csv", sep=",")
test <- read.csv2("./test.csv", sep=",")

################################# PREPARE DATA #################################

# view data structure
str(dataAll, list.len=ncol(dataAll)) 

# set proper class to variables
nonNumeric <- c("user_name","cvtd_timestamp","new_window","classe")
dataAll <- dataAll %>% 
  mutate_at(vars(-all_of(nonNumeric)), as.numeric)
factorVar <- c("user_name","new_window","classe")
dataAll <- dataAll %>% 
  mutate_at(vars(all_of(factorVar)), as.factor)
dataAll$cvtd_timestamp <- dmy_hm(dataAll$cvtd_timestamp)

################################# SUBSET DATA ##################################

# partitionate data
set.seed(12367)
inBuild <- createDataPartition(y=dataAll$classe,
                               p=0.7, list=FALSE) 
data3 <- dataAll[-inBuild,] 
buildData <- dataAll[inBuild,] 
set.seed(4257)
inTrain <- createDataPartition(y=buildData$classe,
                               p=0.7, list=FALSE) 

data1 <- buildData[inTrain,]
data2 <- buildData[-inTrain,]

# dimensions of data sets
cbind(c("Data Set","Data 1","Data 2","Data 3"),
      rbind(c("Rows", "Columns"),
            dim(data1),
            dim(data2),
            dim(data3)))

########################### PREPROCESS TRAIN DATA SET ##########################

# exclude variables with near zero variation
nzv <- nearZeroVar(data1)
data1 <- data1[,-nzv]

# exclude variables that have high NAs
highNA <- names(data1)[colSums(is.na(data1)) / length(data1$user_name) > 0.50]
data1 <- data1[, !(names(data1) %in% highNA)]

# exclude variable X
# reason: it does not mean anything
data1 <- select(data1, -X)

# check if imputation is necessary
colSums(is.na(data1)) 

# remove variables that are highly correlated
except <- c("user_name","cvtd_timestamp","classe")
temp1 <- data1[,!(names(data1) %in% except)]
temp1Cor <- cor(temp1)
highlyCor <- findCorrelation(temp1Cor, cutoff = .75) 
temp2 <- temp1[,-highlyCor]
temp2Cor <- cor(temp2)
summary(temp1Cor[upper.tri(temp1Cor)])
summary(temp2Cor[upper.tri(temp2Cor)])
data1 <- cbind(temp2, select(data1, c("user_name","cvtd_timestamp","classe")))

# view data structure
str(data1, list.len=ncol(data1)) 

############################## CLASSIFICATION TREE #############################

# fit model using Classification Tree method
set.seed(124589)
modCT <- train(classe ~ ., method="rpart", data=data1)
modCT
modCT$finalModel

# plot classification tree
windows()
fancyRpartPlot(modCT$finalModel)

# apply predictions to the validation data set
predCT <- predict(modCT, newdata=data2)

# evaluate model
cmCT <- confusionMatrix(predCT, data2$classe)
cmCT$overall["Accuracy"]

################################# RANDOM FOREST ################################

# fit Random Forest model
set.seed(3697)
modRF <- train(classe ~ ., data=data1, 
               method="rf",
               trControl=trainControl("cv"),
               number=3)
modRF
modRF$finalModel

# apply predictions to the validation data set
predRF <- predict(modRF, newdata=data2)

# evaluate model
cmRF <- confusionMatrix(predRF, data2$classe)
cmRF$overall["Accuracy"]

################################### BOOSTING ###################################

# fit a boosting method with trees
set.seed(4501)
modBoost <- train(classe ~ ., method="gbm", data=data1, verbose=FALSE)
modBoost
modBoost$finalModel

# apply predictions to the validation data set
predBoost <- predict(modBoost, newdata=data2)

# evaluate model
cmBoost <- confusionMatrix(predBoost, data2$classe)
cmBoost$overall["Accuracy"]

################################ COMBINED MODEL ################################

# create a dataframe with all predictors and outcome
dataAllpred <- data.frame(predCT, predRF, predBoost, classe=data2$classe)

# fit a model that will be a combination from all models
set.seed(60376)
modAll <- train(classe ~., method="gam", data=dataAllpred)
modAll
modAll$finalModel

# apply predictions from all models on data3
predCT3 <- predict(modCT, data3)
predRF3 <- predict(modRF, data3)
predBoost3 <- predict(modBoost, data3)

# create data frame with all predictors
dataFinal <- data.frame(predCT=predCT3, 
                        predRF=predRF3, 
                        predBoost=predBoost3)

# get predictions from combined model on validation data set
predAll3 <- predict(modAll, dataFinal)

# evaluate all models
cmCT3 <- confusionMatrix(predCT3, data3$classe)
cmCT3$overall["Accuracy"]

cmRF3 <- confusionMatrix(predRF3, data3$classe)
cmRF3$overall["Accuracy"]

cmBoost3 <- confusionMatrix(predBoost3, data3$classe)
cmBoost3$overall["Accuracy"]

cmAll3 <- confusionMatrix(predAll3, data3$classe)
cmAll3$overall["Accuracy"]

cbind(c("Classification Tree","Random Forest","Boosting", "Combined"),
      rbind(cmCT3$overall["Accuracy"],
      cmRF3$overall["Accuracy"],
      cmBoost3$overall["Accuracy"],
      cmAll3$overall["Accuracy"]))





