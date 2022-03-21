library(data.table)
library(reshape)
library(tidyr)
library(reshape2)
library(expm)
library(plyr)
library(readxl)
library(purrr)
library(sqldf)
library(ggplot2)
library(zoo)
library(ITNr)
library(dplyr)
library(cluster)
library(tidyverse) 
library(factoextra)
library(lubridate)
library(randomForest)

setwd('...')

set.seed(20211111)



# Preprocess the data -------

## Load the data --------

DataOrig <- read.table("spambasedata-Orig.csv",sep=",",header=T,
                       stringsAsFactors=F)

ord <- sample(nrow(DataOrig))
DataOrig <- DataOrig[ord,]


# Change IsSpam to a factor

DataOrig$IsSpam <- factor(DataOrig$IsSpam)


## Split the data ----------

# Doing a 60-20-20 split
TrainInd <- ceiling(nrow(DataOrig)*0.6)
TrainDF <- DataOrig[1:TrainInd,]
tmpDF <- DataOrig[-(1:TrainInd),]
ValInd <- ceiling(nrow(tmpDF)*0.5)
ValDF <- tmpDF[1:ValInd,]
TestDF <- tmpDF[-(1:ValInd),]

remove(TrainInd,tmpDF,ValInd,ord)



# Question 1 --------------------------

# Create the string of the formula to use in the function

SmallFm <- IsSpam ~ 1
Vars <- names(TrainDF)
BigFm <- paste(Vars[58],"~",paste(Vars[1:57],collapse=" + "),sep=" ")
BigFm <- formula(BigFm)


# Stepwise logistic regression and compute the predicted
OutBig <- glm(BigFm, family=binomial(link = "logit"), data = TrainDF)

OutSmall <- glm(SmallFm, family=binomial(link = "logit"), data = TrainDF)
summary(OutSmall)

sc <- list(lower = SmallFm,upper = BigFm)
out <- step(OutSmall,scope = sc,direction = "both")
summary(out)
AIC(out)

# Predict the data
probsVal1 = predict(out, ValDF, type="response")
probsTest1 = predict(out, TestDF, type="response")

# Plot ROC curve
source("RocPlot.r")
ROCPlot(probsVal1,ValDF$IsSpam)
ROCPlot(probsTest1,TestDF$IsSpam)



# Question 2 --------------------------

# Model
out2 <- randomForest(BigFm,data = TrainDF,
                     ntree=1000)

# Predict
probsVal2 = predict(out2, ValDF, type="prob")
probsVal2 = probsVal2[,2]
probsTest2 = predict(out2, TestDF, type="prob")
probsTest2 = probsTest2[,2]

# ROC Plot
ROCPlot(probsVal2,ValDF$IsSpam)
ROCPlot(probsTest2,TestDF$IsSpam)



# Question 3 --------------------------

# Store the input
write.table(TrainDF,file="NNHWTrain.csv",sep=",",row.names=F,col.names=T)
write.table(ValDF,file="NNHWVal.csv",sep=",",row.names=F,col.names=T)
write.table(TestDF,file="NNHWTest.csv",sep=",",row.names=F,col.names=T)

# Load the output
SpamNNWideTrainOutput <- read.table("SpamNNWideTrainDFOutput.csv",header=T,sep=",",quote=NULL,stringsAsFactors = F)
SpamNNWideValOutput <- read.table("SpamNNWideValDFOutput.csv",header=T,sep=",",quote=NULL,stringsAsFactors = F)
SpamNNWideTestOutput <- read.table("SpamNNWideTestDFOutput.csv",header=T,sep=",",quote=NULL,stringsAsFactors = F)

# ROC Plot
names(SpamNNWideValOutput)
ROCPlot(SpamNNWideValOutput$ValP,SpamNNWideValOutput$IsSpam)
ROCPlot(SpamNNWideTestOutput$TestP,SpamNNWideTestOutput$IsSpam)


# Question 4 --------------------------

# Load the output
SpamNNWideTrainOutput <- read.table("SpamNNWideTrainDFOutput.csv",header=T,sep=",",quote=NULL,stringsAsFactors = F)
SpamNNWideValOutput <- read.table("SpamNNWideValDFOutput.csv",header=T,sep=",",quote=NULL,stringsAsFactors = F)
SpamNNWideTestOutput <- read.table("SpamNNWideTestDFOutput.csv",header=T,sep=",",quote=NULL,stringsAsFactors = F)

# ROC Plot
names(SpamNNWideValOutput)
ROCPlot(SpamNNWideValOutput$ValP,SpamNNWideValOutput$IsSpam)
ROCPlot(SpamNNWideTestOutput$TestP,SpamNNWideTestOutput$IsSpam)