# 1. Load libraries and data ----------------------------------------------
rm(list = ls())                                        # Reset R's brain
gc()                                                   # Clean cache memory
graphics.off()
getwd()                                                # getwd tells you where R is currently looking

# only load necessary libraries
library(ggplot2)
library(caret)   # for CV and repeated CV
library(rminer)
library(klaR)
library(bio3d)
library(igraph)
library(cvq2)
library(tidyverse)

setwd("D:/PhD/Summer Schools/R_Course/R Course InGrA 04 2019/R-course Day 3")                  # setwd tells R to look

list.files()                                                                                   # list files in the working directory

dat.train2 <- read.csv("Test_R-squared.csv", header=T, sep=",", dec=".")     # Input data for training set


str(dat.train2)



###### TESTING for personal use ######
set.seed(1)

qsar_test_new.lm = lm(pIC50.smHDAC8 ~ AM1.GB1_MD_1.50 + PEOE_VSA_FPPOS,
                      data = dat.train2)

summary(qsar_test_new.lm)
str(summary(qsar_test.lm))
qsar_test.resid.lm <- qsar_test.lm$residuals
qsar_test.resid.lm


#######################################

set.seed(1)

control = trainControl(method="LOOCV")    # done just once "repeas=20" not read in the function

fit.loocv = train(pIC50.smHDAC8 ~ AM1.GB1_MD_1.50 + PEOE_VSA_FPPOS,
                  data = dat.train2,
                  method = "lm", trControl = control)
fit.loocv

str(fit.loocv)



# 2. model selection --------------------------------------------------------

set.seed(1)

control = trainControl(method="cv", number = 10)                          # other methods "cv", "boot", "LOOCV", 


##### fit lm model that predicts pIC50.smHDAC8 with one descriptor #####
fit.cv_new.lm = train(pIC50.smHDAC8 ~ AM1.GB1_MD_1.50 + PEOE_VSA_FPPOS,                 # One descriptor
                      data = dat.train2,
                      method = "lm", trControl = control)                ## other methods "ranger" random forest; "lm" linear model; "boot" bootstrap "nb" Naive Bayes....
fit.cv_new.lm
str(fit.cv_new.lm)



#####

set.seed(1)

control = trainControl(method="repeatedcv", number=11, repeats=10000)                          # other methods "cv", "boot", "LOOCV", 

##### fit lm model that predicts pIC50.smHDAC8 with one descriptor #####
fit.repeatedcv.lm = train(pIC50.smHDAC8 ~ AM1.GB1_MD_1.50  + PEOE_VSA_FPPOS,                 # One descriptor
                          data = dat.train2,
                          method = "lm", trControl = control)                ## other methods "ranger" random forest; "lm" linear model; "boot" bootstrap "nb" Naive Bayes....
fit.repeatedcv.lm


##### fit random forest model that predicts pIC50.smHDAC8 with more than one descriptor #####

set.seed(1)

control = trainControl(method="repeatedcv", number=11, repeats=500) 

fit.repeated.rf.model14 = train(pIC50.smHDAC8 ~ AM1.GB1_MD_1.50 + PEOE_VSA_FPPOS,
                                data = dat.train2,
                                method = "ranger", trControl = control)

fit.repeated.rf.model14
