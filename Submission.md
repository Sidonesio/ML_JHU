Predicting how well people perform barbell
================
Sidney S. P. Bisssoli
16/02/2021

## Executive Summary

Using devices such as *Jawbone Up*, *Nike FuelBand* and *Fitbit*, it is
now possible to collect a large amount of data about personal activity
relatively inexpensively. These type of devices are part of the
quantified self movement – a group of enthusiasts who take measurements
about themselves regularly to improve their health, to find patterns in
their behavior, or because they are tech geeks.

In this project, the goal is to use data from accelerometers on the
belt, forearm, arm, and dumbell of 6 participants. They were asked to
perform barbell lifts correctly and incorrectly in 5 different ways.

Data set was subset in three subsamples. First, it was selected the
possible best predictors. After that, four models were fitted:
*Classification Tree*; *Random Forest*; *Boosting with Trees*; and a
*Combined Model* from previous ones. *Random Forest* was the algorithm
with the best accuracy (0.9976). *Combined model* got a very poor
performance on validation data set, probably due to overfitting.

## Part I: Loading and Preparing Data Set

First, we need to load and install (if necessary) the packages that we
will be required for this project.

``` r
# packages' name
packages <- c("caret","dplyr","lubridate")

# install packages if they have not been installed yet
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# load packages
invisible(lapply(packages, library, character.only = TRUE))
```

Then, let’s load the data:

``` r
# store URL as an object in R
URL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv" 

# download file
download.file(URL, destfile = "./dataAll.csv")

# read/load data into R workspace
dataAll <- read.csv2("./dataAll.csv", sep=",")
```

How does this data look like?

``` r
# view data structure
str(dataAll, list.len=ncol(dataAll))
```

    ## 'data.frame':    19622 obs. of  160 variables:
    ##  $ X                       : int  1 2 3 4 5 6 7 8 9 10 ...
    ##  $ user_name               : chr  "carlitos" "carlitos" "carlitos" "carlitos" ...
    ##  $ raw_timestamp_part_1    : int  1323084231 1323084231 1323084231 1323084232 1323084232 1323084232 1323084232 1323084232 1323084232 1323084232 ...
    ##  $ raw_timestamp_part_2    : int  788290 808298 820366 120339 196328 304277 368296 440390 484323 484434 ...
    ##  $ cvtd_timestamp          : chr  "05/12/2011 11:23" "05/12/2011 11:23" "05/12/2011 11:23" "05/12/2011 11:23" ...
    ##  $ new_window              : chr  "no" "no" "no" "no" ...
    ##  $ num_window              : int  11 11 11 12 12 12 12 12 12 12 ...
    ##  $ roll_belt               : chr  "1.41" "1.41" "1.42" "1.48" ...
    ##  $ pitch_belt              : chr  "8.07" "8.07" "8.07" "8.05" ...
    ##  $ yaw_belt                : chr  "-94.4" "-94.4" "-94.4" "-94.4" ...
    ##  $ total_accel_belt        : int  3 3 3 3 3 3 3 3 3 3 ...
    ##  $ kurtosis_roll_belt      : chr  "" "" "" "" ...
    ##  $ kurtosis_picth_belt     : chr  "" "" "" "" ...
    ##  $ kurtosis_yaw_belt       : chr  "" "" "" "" ...
    ##  $ skewness_roll_belt      : chr  "" "" "" "" ...
    ##  $ skewness_roll_belt.1    : chr  "" "" "" "" ...
    ##  $ skewness_yaw_belt       : chr  "" "" "" "" ...
    ##  $ max_roll_belt           : chr  NA NA NA NA ...
    ##  $ max_picth_belt          : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ max_yaw_belt            : chr  "" "" "" "" ...
    ##  $ min_roll_belt           : chr  NA NA NA NA ...
    ##  $ min_pitch_belt          : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ min_yaw_belt            : chr  "" "" "" "" ...
    ##  $ amplitude_roll_belt     : chr  NA NA NA NA ...
    ##  $ amplitude_pitch_belt    : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ amplitude_yaw_belt      : chr  "" "" "" "" ...
    ##  $ var_total_accel_belt    : chr  NA NA NA NA ...
    ##  $ avg_roll_belt           : chr  NA NA NA NA ...
    ##  $ stddev_roll_belt        : chr  NA NA NA NA ...
    ##  $ var_roll_belt           : chr  NA NA NA NA ...
    ##  $ avg_pitch_belt          : chr  NA NA NA NA ...
    ##  $ stddev_pitch_belt       : chr  NA NA NA NA ...
    ##  $ var_pitch_belt          : chr  NA NA NA NA ...
    ##  $ avg_yaw_belt            : chr  NA NA NA NA ...
    ##  $ stddev_yaw_belt         : chr  NA NA NA NA ...
    ##  $ var_yaw_belt            : chr  NA NA NA NA ...
    ##  $ gyros_belt_x            : chr  "0" "0.02" "0" "0.02" ...
    ##  $ gyros_belt_y            : chr  "0" "0" "0" "0" ...
    ##  $ gyros_belt_z            : chr  "-0.02" "-0.02" "-0.02" "-0.03" ...
    ##  $ accel_belt_x            : int  -21 -22 -20 -22 -21 -21 -22 -22 -20 -21 ...
    ##  $ accel_belt_y            : int  4 4 5 3 2 4 3 4 2 4 ...
    ##  $ accel_belt_z            : int  22 22 23 21 24 21 21 21 24 22 ...
    ##  $ magnet_belt_x           : int  -3 -7 -2 -6 -6 0 -4 -2 1 -3 ...
    ##  $ magnet_belt_y           : int  599 608 600 604 600 603 599 603 602 609 ...
    ##  $ magnet_belt_z           : int  -313 -311 -305 -310 -302 -312 -311 -313 -312 -308 ...
    ##  $ roll_arm                : chr  "-128" "-128" "-128" "-128" ...
    ##  $ pitch_arm               : chr  "22.5" "22.5" "22.5" "22.1" ...
    ##  $ yaw_arm                 : chr  "-161" "-161" "-161" "-161" ...
    ##  $ total_accel_arm         : int  34 34 34 34 34 34 34 34 34 34 ...
    ##  $ var_accel_arm           : chr  NA NA NA NA ...
    ##  $ avg_roll_arm            : chr  NA NA NA NA ...
    ##  $ stddev_roll_arm         : chr  NA NA NA NA ...
    ##  $ var_roll_arm            : chr  NA NA NA NA ...
    ##  $ avg_pitch_arm           : chr  NA NA NA NA ...
    ##  $ stddev_pitch_arm        : chr  NA NA NA NA ...
    ##  $ var_pitch_arm           : chr  NA NA NA NA ...
    ##  $ avg_yaw_arm             : chr  NA NA NA NA ...
    ##  $ stddev_yaw_arm          : chr  NA NA NA NA ...
    ##  $ var_yaw_arm             : chr  NA NA NA NA ...
    ##  $ gyros_arm_x             : chr  "0" "0.02" "0.02" "0.02" ...
    ##  $ gyros_arm_y             : chr  "0" "-0.02" "-0.02" "-0.03" ...
    ##  $ gyros_arm_z             : chr  "-0.02" "-0.02" "-0.02" "0.02" ...
    ##  $ accel_arm_x             : int  -288 -290 -289 -289 -289 -289 -289 -289 -288 -288 ...
    ##  $ accel_arm_y             : int  109 110 110 111 111 111 111 111 109 110 ...
    ##  $ accel_arm_z             : int  -123 -125 -126 -123 -123 -122 -125 -124 -122 -124 ...
    ##  $ magnet_arm_x            : int  -368 -369 -368 -372 -374 -369 -373 -372 -369 -376 ...
    ##  $ magnet_arm_y            : int  337 337 344 344 337 342 336 338 341 334 ...
    ##  $ magnet_arm_z            : int  516 513 513 512 506 513 509 510 518 516 ...
    ##  $ kurtosis_roll_arm       : chr  "" "" "" "" ...
    ##  $ kurtosis_picth_arm      : chr  "" "" "" "" ...
    ##  $ kurtosis_yaw_arm        : chr  "" "" "" "" ...
    ##  $ skewness_roll_arm       : chr  "" "" "" "" ...
    ##  $ skewness_pitch_arm      : chr  "" "" "" "" ...
    ##  $ skewness_yaw_arm        : chr  "" "" "" "" ...
    ##  $ max_roll_arm            : chr  NA NA NA NA ...
    ##  $ max_picth_arm           : chr  NA NA NA NA ...
    ##  $ max_yaw_arm             : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ min_roll_arm            : chr  NA NA NA NA ...
    ##  $ min_pitch_arm           : chr  NA NA NA NA ...
    ##  $ min_yaw_arm             : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ amplitude_roll_arm      : chr  NA NA NA NA ...
    ##  $ amplitude_pitch_arm     : chr  NA NA NA NA ...
    ##  $ amplitude_yaw_arm       : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ roll_dumbbell           : chr  "13.05217456" "13.13073959" "12.85074981" "13.43119971" ...
    ##  $ pitch_dumbbell          : chr  "-70.49400371" "-70.63750507" "-70.27811982" "-70.39379464" ...
    ##  $ yaw_dumbbell            : chr  "-84.87393888" "-84.71064711" "-85.14078134" "-84.87362553" ...
    ##  $ kurtosis_roll_dumbbell  : chr  "" "" "" "" ...
    ##  $ kurtosis_picth_dumbbell : chr  "" "" "" "" ...
    ##  $ kurtosis_yaw_dumbbell   : chr  "" "" "" "" ...
    ##  $ skewness_roll_dumbbell  : chr  "" "" "" "" ...
    ##  $ skewness_pitch_dumbbell : chr  "" "" "" "" ...
    ##  $ skewness_yaw_dumbbell   : chr  "" "" "" "" ...
    ##  $ max_roll_dumbbell       : chr  NA NA NA NA ...
    ##  $ max_picth_dumbbell      : chr  NA NA NA NA ...
    ##  $ max_yaw_dumbbell        : chr  "" "" "" "" ...
    ##  $ min_roll_dumbbell       : chr  NA NA NA NA ...
    ##  $ min_pitch_dumbbell      : chr  NA NA NA NA ...
    ##  $ min_yaw_dumbbell        : chr  "" "" "" "" ...
    ##  $ amplitude_roll_dumbbell : chr  NA NA NA NA ...
    ##  $ amplitude_pitch_dumbbell: chr  NA NA NA NA ...
    ##  $ amplitude_yaw_dumbbell  : chr  "" "" "" "" ...
    ##  $ total_accel_dumbbell    : int  37 37 37 37 37 37 37 37 37 37 ...
    ##  $ var_accel_dumbbell      : chr  NA NA NA NA ...
    ##  $ avg_roll_dumbbell       : chr  NA NA NA NA ...
    ##  $ stddev_roll_dumbbell    : chr  NA NA NA NA ...
    ##  $ var_roll_dumbbell       : chr  NA NA NA NA ...
    ##  $ avg_pitch_dumbbell      : chr  NA NA NA NA ...
    ##  $ stddev_pitch_dumbbell   : chr  NA NA NA NA ...
    ##  $ var_pitch_dumbbell      : chr  NA NA NA NA ...
    ##  $ avg_yaw_dumbbell        : chr  NA NA NA NA ...
    ##  $ stddev_yaw_dumbbell     : chr  NA NA NA NA ...
    ##  $ var_yaw_dumbbell        : chr  NA NA NA NA ...
    ##  $ gyros_dumbbell_x        : chr  "0" "0" "0" "0" ...
    ##  $ gyros_dumbbell_y        : chr  "-0.02" "-0.02" "-0.02" "-0.02" ...
    ##  $ gyros_dumbbell_z        : chr  "0" "0" "0" "-0.02" ...
    ##  $ accel_dumbbell_x        : int  -234 -233 -232 -232 -233 -234 -232 -234 -232 -235 ...
    ##  $ accel_dumbbell_y        : int  47 47 46 48 48 48 47 46 47 48 ...
    ##  $ accel_dumbbell_z        : int  -271 -269 -270 -269 -270 -269 -270 -272 -269 -270 ...
    ##  $ magnet_dumbbell_x       : int  -559 -555 -561 -552 -554 -558 -551 -555 -549 -558 ...
    ##  $ magnet_dumbbell_y       : int  293 296 298 303 292 294 295 300 292 291 ...
    ##  $ magnet_dumbbell_z       : chr  "-65" "-64" "-63" "-60" ...
    ##  $ roll_forearm            : chr  "28.4" "28.3" "28.3" "28.1" ...
    ##  $ pitch_forearm           : chr  "-63.9" "-63.9" "-63.9" "-63.9" ...
    ##  $ yaw_forearm             : chr  "-153" "-153" "-152" "-152" ...
    ##  $ kurtosis_roll_forearm   : chr  "" "" "" "" ...
    ##  $ kurtosis_picth_forearm  : chr  "" "" "" "" ...
    ##  $ kurtosis_yaw_forearm    : chr  "" "" "" "" ...
    ##  $ skewness_roll_forearm   : chr  "" "" "" "" ...
    ##  $ skewness_pitch_forearm  : chr  "" "" "" "" ...
    ##  $ skewness_yaw_forearm    : chr  "" "" "" "" ...
    ##  $ max_roll_forearm        : chr  NA NA NA NA ...
    ##  $ max_picth_forearm       : chr  NA NA NA NA ...
    ##  $ max_yaw_forearm         : chr  "" "" "" "" ...
    ##  $ min_roll_forearm        : chr  NA NA NA NA ...
    ##  $ min_pitch_forearm       : chr  NA NA NA NA ...
    ##  $ min_yaw_forearm         : chr  "" "" "" "" ...
    ##  $ amplitude_roll_forearm  : chr  NA NA NA NA ...
    ##  $ amplitude_pitch_forearm : chr  NA NA NA NA ...
    ##  $ amplitude_yaw_forearm   : chr  "" "" "" "" ...
    ##  $ total_accel_forearm     : int  36 36 36 36 36 36 36 36 36 36 ...
    ##  $ var_accel_forearm       : chr  NA NA NA NA ...
    ##  $ avg_roll_forearm        : chr  NA NA NA NA ...
    ##  $ stddev_roll_forearm     : chr  NA NA NA NA ...
    ##  $ var_roll_forearm        : chr  NA NA NA NA ...
    ##  $ avg_pitch_forearm       : chr  NA NA NA NA ...
    ##  $ stddev_pitch_forearm    : chr  NA NA NA NA ...
    ##  $ var_pitch_forearm       : chr  NA NA NA NA ...
    ##  $ avg_yaw_forearm         : chr  NA NA NA NA ...
    ##  $ stddev_yaw_forearm      : chr  NA NA NA NA ...
    ##  $ var_yaw_forearm         : chr  NA NA NA NA ...
    ##  $ gyros_forearm_x         : chr  "0.03" "0.02" "0.03" "0.02" ...
    ##  $ gyros_forearm_y         : chr  "0" "0" "-0.02" "-0.02" ...
    ##  $ gyros_forearm_z         : chr  "-0.02" "-0.02" "0" "0" ...
    ##  $ accel_forearm_x         : int  192 192 196 189 189 193 195 193 193 190 ...
    ##  $ accel_forearm_y         : int  203 203 204 206 206 203 205 205 204 205 ...
    ##  $ accel_forearm_z         : int  -215 -216 -213 -214 -214 -215 -215 -213 -214 -215 ...
    ##  $ magnet_forearm_x        : int  -17 -18 -18 -16 -17 -9 -18 -9 -16 -22 ...
    ##  $ magnet_forearm_y        : chr  "654" "661" "658" "658" ...
    ##  $ magnet_forearm_z        : chr  "476" "473" "469" "469" ...
    ##  $ classe                  : chr  "A" "A" "A" "A" ...

Variables’ class are not properly set up. Let’s fix it:

``` r
# set proper class to variables
nonNumeric <- c("user_name","cvtd_timestamp","new_window","classe")
dataAll <- dataAll %>% 
  mutate_at(vars(-all_of(nonNumeric)), as.numeric)
factorVar <- c("user_name","new_window","classe")
dataAll <- dataAll %>% 
  mutate_at(vars(all_of(factorVar)), as.factor)
dataAll$cvtd_timestamp <- dmy_hm(dataAll$cvtd_timestamp)
```

Before any preprocessing, let’s partitionate data in three others.
Training, test and validation data sets will be called *data1*, *data2*
and *data3*, respectivelly.

``` r
# partitionate data
set.seed(12367)
inBuild <- createDataPartition(y=dataAll$classe, p=0.7, list=FALSE) 
data3 <- dataAll[-inBuild,] 
buildData <- dataAll[inBuild,] 
set.seed(4257)
inTrain <- createDataPartition(y=buildData$classe, p=0.7, list=FALSE)
data1 <- buildData[inTrain,]
data2 <- buildData[-inTrain,]
```

These are the number of observations and variables (rows and columns)
from the three data sets:

``` r
# dimensions of data sets
cbind(c("Data Set","Data 1","Data 2","Data 3"),
      rbind(c("Rows", "Columns"),
            dim(data1),
            dim(data2),
            dim(data3)))
```

    ##      [,1]       [,2]   [,3]     
    ## [1,] "Data Set" "Rows" "Columns"
    ## [2,] "Data 1"   "9619" "160"    
    ## [3,] "Data 2"   "4118" "160"    
    ## [4,] "Data 3"   "5885" "160"

## Part II: Preprocessing the training data set

Now that we got the three data sets in which we are going to work on, we
need to run some type of preparation on the training data set. It is a
huge data frame, with lot’s of variables (160), we need to build some
strategy to approach prediction modelling.

The first technique will be removing variables that has near zero
variance:

``` r
# exclude variables with near zero variation
nzv <- nearZeroVar(data1)
data1 <- data1[,-nzv]
```

Secondly, we will remove all variables that has more than 50% of missing
values (NAs), because probably imputation would not be so reliable in
this case:

``` r
# exclude variables that have high NAs
highNA <- names(data1)[colSums(is.na(data1)) / length(data1$user_name) > 0.50]
data1 <- data1[, !(names(data1) %in% highNA)]
```

Now, we are going to remove variable “X”, because it does not mean
anything (it just order the observations):

``` r
# exclude variable X
data1 <- select(data1, -X)
```

Now that we did it all, let’s see if there is need for NA’s imputation:

``` r
# check if imputation is necessary
colSums(is.na(data1)) 
```

    ##            user_name raw_timestamp_part_1 raw_timestamp_part_2 
    ##                    0                    0                    0 
    ##       cvtd_timestamp           num_window            roll_belt 
    ##                    0                    0                    0 
    ##           pitch_belt             yaw_belt     total_accel_belt 
    ##                    0                    0                    0 
    ##         gyros_belt_x         gyros_belt_y         gyros_belt_z 
    ##                    0                    0                    0 
    ##         accel_belt_x         accel_belt_y         accel_belt_z 
    ##                    0                    0                    0 
    ##        magnet_belt_x        magnet_belt_y        magnet_belt_z 
    ##                    0                    0                    0 
    ##             roll_arm            pitch_arm              yaw_arm 
    ##                    0                    0                    0 
    ##      total_accel_arm          gyros_arm_x          gyros_arm_y 
    ##                    0                    0                    0 
    ##          gyros_arm_z          accel_arm_x          accel_arm_y 
    ##                    0                    0                    0 
    ##          accel_arm_z         magnet_arm_x         magnet_arm_y 
    ##                    0                    0                    0 
    ##         magnet_arm_z        roll_dumbbell       pitch_dumbbell 
    ##                    0                    0                    0 
    ##         yaw_dumbbell total_accel_dumbbell     gyros_dumbbell_x 
    ##                    0                    0                    0 
    ##     gyros_dumbbell_y     gyros_dumbbell_z     accel_dumbbell_x 
    ##                    0                    0                    0 
    ##     accel_dumbbell_y     accel_dumbbell_z    magnet_dumbbell_x 
    ##                    0                    0                    0 
    ##    magnet_dumbbell_y    magnet_dumbbell_z         roll_forearm 
    ##                    0                    0                    0 
    ##        pitch_forearm          yaw_forearm  total_accel_forearm 
    ##                    0                    0                    0 
    ##      gyros_forearm_x      gyros_forearm_y      gyros_forearm_z 
    ##                    0                    0                    0 
    ##      accel_forearm_x      accel_forearm_y      accel_forearm_z 
    ##                    0                    0                    0 
    ##     magnet_forearm_x     magnet_forearm_y     magnet_forearm_z 
    ##                    0                    0                    0 
    ##               classe 
    ##                    0

No need for imputation, there are no missing values (NAs) in the data
set. Even though, we have variables that are highly correlated with each
other. We will remove those variables:

``` r
except <- c("user_name","cvtd_timestamp","classe")
temp1 <- data1[,!(names(data1) %in% except)]
temp1Cor <- cor(temp1)
highlyCor <- findCorrelation(temp1Cor, cutoff = .75) 
temp2 <- temp1[,-highlyCor]
temp2Cor <- cor(temp2)
data1 <- cbind(temp2, select(data1, c("user_name","cvtd_timestamp","classe")))
```

Now we can model.

## Part III: Modeling

First, we will model using **Classification Tree** method:

``` r
# fit model using Classification Tree method
set.seed(124589)
modCT <- train(classe ~ ., method="rpart", data=data1)
modCT$finalModel
```

    ## n= 9619 
    ## 
    ## node), split, n, loss, yval, (yprob)
    ##       * denotes terminal node
    ## 
    ##   1) root 9619 6884 A (0.28 0.19 0.17 0.16 0.18)  
    ##     2) pitch_forearm< -26.55 879   42 A (0.95 0.048 0 0 0) *
    ##     3) pitch_forearm>=-26.55 8740 6842 A (0.22 0.21 0.19 0.18 0.2)  
    ##       6) magnet_belt_y>=557.5 8003 6107 A (0.24 0.23 0.21 0.18 0.15)  
    ##        12) yaw_belt>=169.5 402   39 A (0.9 0.042 0 0.055 0) *
    ##        13) yaw_belt< 169.5 7601 5810 B (0.2 0.24 0.22 0.19 0.15)  
    ##          26) raw_timestamp_part_1< 1.32249e+09 237    0 A (1 0 0 0 0) *
    ##          27) raw_timestamp_part_1>=1.32249e+09 7364 5573 B (0.18 0.24 0.23 0.19 0.16)  
    ##            54) raw_timestamp_part_1< 1.32249e+09 306    7 B (0.016 0.98 0.0065 0 0) *
    ##            55) raw_timestamp_part_1>=1.32249e+09 7058 5386 C (0.18 0.21 0.24 0.2 0.17)  
    ##             110) roll_dumbbell< 56.62508 4334 2929 C (0.24 0.16 0.32 0.13 0.15)  
    ##               220) pitch_belt>=15.05 1698  990 A (0.42 0.23 0.15 0.15 0.059) *
    ##               221) pitch_belt< 15.05 2636 1484 C (0.12 0.11 0.44 0.13 0.2) *
    ##             111) roll_dumbbell>=56.62508 2724 1879 D (0.094 0.3 0.098 0.31 0.2)  
    ##               222) magnet_belt_y>=590.5 2395 1562 D (0.11 0.34 0.073 0.35 0.14)  
    ##                 444) total_accel_dumbbell>=5.5 1734  995 B (0.084 0.43 0.1 0.23 0.16) *
    ##                 445) total_accel_dumbbell< 5.5 661  222 D (0.17 0.1 0 0.66 0.067) *
    ##               223) magnet_belt_y< 590.5 329  109 E (0 0.018 0.28 0.036 0.67) *
    ##       7) magnet_belt_y< 557.5 737  142 E (0.0027 0.015 0.0054 0.17 0.81) *

``` r
# apply predictions to the test data set
predCT <- predict(modCT, newdata=data2)
```

Second modelling strategy will use **Random Forest** method:

``` r
set.seed(3697)
modRF <- train(classe ~ ., data=data1, 
               method="rf",
               trControl=trainControl("cv"),
               number=3)
modRF$finalModel
```

    ## 
    ## Call:
    ##  randomForest(x = x, y = y, mtry = param$mtry, number = 3) 
    ##                Type of random forest: classification
    ##                      Number of trees: 500
    ## No. of variables tried at each split: 21
    ## 
    ##         OOB estimate of  error rate: 0.18%
    ## Confusion matrix:
    ##      A    B    C    D    E  class.error
    ## A 2734    1    0    0    0 0.0003656307
    ## B    2 1859    0    0    0 0.0010746910
    ## C    0    4 1670    4    0 0.0047675805
    ## D    0    0    4 1573    0 0.0025364616
    ## E    0    0    0    2 1766 0.0011312217

``` r
# apply predictions to the test data set
predRF <- predict(modRF, newdata=data2)
```

Third model will be **Boosting method with trees**:

``` r
# fit a boosting method with trees
set.seed(4501)
modBoost <- train(classe ~ ., method="gbm", data=data1, verbose=FALSE)
modBoost
```

    ## Stochastic Gradient Boosting 
    ## 
    ## 9619 samples
    ##   36 predictor
    ##    5 classes: 'A', 'B', 'C', 'D', 'E' 
    ## 
    ## No pre-processing
    ## Resampling: Bootstrapped (25 reps) 
    ## Summary of sample sizes: 9619, 9619, 9619, 9619, 9619, 9619, ... 
    ## Resampling results across tuning parameters:
    ## 
    ##   interaction.depth  n.trees  Accuracy   Kappa    
    ##   1                   50      0.7665296  0.7037972
    ##   1                  100      0.8486120  0.8083646
    ##   1                  150      0.8907108  0.8617249
    ##   2                   50      0.9182006  0.8964617
    ##   2                  100      0.9757468  0.9693094
    ##   2                  150      0.9906469  0.9881644
    ##   3                   50      0.9617103  0.9515398
    ##   3                  100      0.9916921  0.9894880
    ##   3                  150      0.9952596  0.9940022
    ## 
    ## Tuning parameter 'shrinkage' was held constant at a value of 0.1
    ## 
    ## Tuning parameter 'n.minobsinnode' was held constant at a value of 10
    ## Accuracy was used to select the optimal model using the largest value.
    ## The final values used for the model were n.trees = 150, interaction.depth =
    ##  3, shrinkage = 0.1 and n.minobsinnode = 10.

``` r
# apply predictions to the test data set
predBoost <- predict(modBoost, newdata=data2)
```

Finally, let’s combine all three models:

``` r
# create a dataframe with all predictors and outcome
dataAllpred <- data.frame(predCT, predRF, predBoost, classe=data2$classe)

# fit a model that will be a combination from the three models
set.seed(60376)
modAll <- train(classe ~., method="gam", data=dataAllpred)
```

Now that we run four models (in fact, three models and a combined one),
we are going to evaluate them. First, we will get predicted values on
the validation data set:

``` r
# apply predictions from 3 models on data3
predCT3 <- predict(modCT, data3)
predRF3 <- predict(modRF, data3)
predBoost3 <- predict(modBoost, data3)

# create data frame with 3 predictors
dataFinal <- data.frame(predCT=predCT3, 
                        predRF=predRF3, 
                        predBoost=predBoost3)
# get predictions from combined model on validation data set
predAll3 <- predict(modAll, dataFinal)
```

Finally, let’s evaluate the accuracy for the four models

``` r
# evaluate all models
cmCT3 <- confusionMatrix(predCT3, data3$classe)
cmRF3 <- confusionMatrix(predRF3, data3$classe)
cmBoost3 <- confusionMatrix(predBoost3, data3$classe)
cmAll3 <- confusionMatrix(predAll3, data3$classe)
cbind(c("Classification Tree","Random Forest","Boosting", "Combined"),
      rbind(cmCT3$overall["Accuracy"],
      cmRF3$overall["Accuracy"],
      cmBoost3$overall["Accuracy"],
      cmAll3$overall["Accuracy"]))
```

    ##                            Accuracy           
    ## [1,] "Classification Tree" "0.583007646559048"
    ## [2,] "Random Forest"       "0.997960917587086"
    ## [3,] "Boosting"            "0.996261682242991"
    ## [4,] "Combined"            "0.477655055225149"

As we can see, **Random Forest** revealed to be the best modelling
strategy with this data set.
