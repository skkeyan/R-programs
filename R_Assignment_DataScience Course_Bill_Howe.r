# R Program to process seaflow file for Data Science course

#Packages to be installed:

# install.packages("caret")
# install.packages("rpart")
# install.packages("tree")
# install.packages("randomForest")
# install.packages("e1071")
# install.packages("ggplot2")

# Libraries required

library(ggplot2) - For plotting
library(tree) - For decision tree
library(randomForest) - For random forest
library(e1071) - For Support Vector Machines


seaflow <- read.csv("seaflow_21min.csv")  # Read the csv file
summary(seaflow) # Analyze the summary of the seaflow dataframe to answer questions 1 & 2

#
# Split the dataset into training and test datasets
#

set.seed(2)
train <- sample(1:nrow(seaflow),(nrow(seaflow)+1)/2)
test <- -train

seaflow_train <- seaflow[train,]
seaflow_test <- seaflow[test,]

#
# Plot the dataset
#
ggplot()+geom_point(data = seaflow,aes(x=pe,y=chl_small,colour=pop))

#
# Train the decision tree
#
dt_formula <- formula(pop ~ fsc_small + fsc_perp + fsc_big + pe + chl_big + chl_small) # Create the appropriate formula
model <- tree(dt_formula,method="class",data = seaflow_train)                          # Create the decision tree
print(model)

#
# Predict using the decision tree computed in the previous step
#
predictions_dt <- predict(model, newdata=seaflow_test, type="class")  # Create the prediction vector. length(predictions) == length(seaflow_test$pop)
summary(seaflow_test$pop == predictions_dt)   # Accurary is count of true /(count of true + count of false)

#
# Train the random forest and make predictions
#
rf_formula <- formula(pop ~ fsc_small + fsc_perp + fsc_big + pe + chl_big + chl_small)
rf_model <- randomForest(rf_formula, data=seaflow_train,ntree=40)
predictions_rf <- predict(rf_model, newdata=seaflow_test, type="class")
summary(seaflow_test$pop == predictions_rf)  # To create the confusion matrix

importance(rf_model) # To get the Gini coefficient

#
# Train the Support Vector Machine and make predictions
#
svm_formula <- formula(pop ~ fsc_small + fsc_perp + fsc_big + pe + chl_big + chl_small)
svm_model <- svm(svm_formula, data=seaflow_train)
predictions_svm <- predict(svm_model, newdata=seaflow_test, type="class")
summary(seaflow_test$pop == predictions_svm)  # To create the confusion matrix

#
# Create a table for the confusion matrix
#

table(pred = predictions_svm, true = seaflow_test$pop) # for the svm model
table(pred = predictions_rf, true = seaflow_test$pop)  # for the randomForest model
table(pred = predictions_dt, true = seaflow_test$pop)  # for the decision tree model

#
# Remove outlier data (in this case, file_id = 208) and then recreate the models
#

seaflow_reduced <- seaflow[seaflow$file_id != 208,]
set.seed(2)
train_reduced <- sample(1:nrow(seaflow_reduced),(nrow(seaflow_reduced))/2)
test_reduced <- -train_reduced

seaflow_red_train <- seaflow_reduced[train_reduced,]
seaflow_red_test <- seaflow_reduced[test_reduced,]

# svm_formula <- formula(pop ~ fsc_small + fsc_perp + fsc_big + pe + chl_big + chl_small)
svm_model_reduced <- svm(svm_formula, data=seaflow_red_train)
predictions_svm_reduced <- predict(svm_model_reduced, newdata=seaflow_red_test, type="class")
summary(seaflow_red_test$pop == predictions_svm_reduced)  # To create the confusion matrix



###################################################################
#R Programming Quiz: Working out the Answers

#1) Population of Synecho:
seaflow <- read.csv("seaflow_21min.csv")
summary(seaflow)

#Answer - 18146

#2) 3rd Quantile of fsc_small - Answer: 39184

#3) Split the dataset equally into training and test dataset:

set.seed(2)
train <- sample(1:nrow(seaflow),nrow(seaflow)/2)
test <- -train

seaflow_train <- seaflow[train,]
seaflow_test <- seaflow[test,]

#Answer - mean(seaflow_train$time) - 341.6968

#4) Plotting
ggplot()+geom_point(data = seaflow,aes(x=pe,y=chl_small,colour=pop))  #Answer - Nano & Pico


#5) Train the decision tree

#fol <- formula(response ~ var1 + var2 + var3)
# Use library tree

fol <- formula(pop ~ fsc_small + fsc_perp + fsc_big + pe + chl_big + chl_small)
# Option 1: model <- rpart(fol,method="class",data = seaflow_train) 
# Option 2: model <- tree(fol,method="class",data = seaflow_train)  <Used this>
print(model)

#Answer to Q 5 - Crypto
#Answer to Q 6 - 5001.5
#Answer to Q 7 - pe, chl_small

8) Predict using the model

predictions <- predict(model, newdata=seaflow_test, type="class")
length(predictions) should be equal to length(seaflow_test$pop)
summary(seaflow_test$pop == predictions)
<Result is - FALSE: 5647, TRUE: 30525>

Accuracy = count of true /(count of true + count of false) = 0.8438

9) Random Forest:
rf_formula <- formula(pop ~ fsc_small + fsc_perp + fsc_big + pe + chl_big + chl_small)
rf_model <- randomForest(rf_formula, data=seaflow_train,ntree=)
predictions <- predict(rf_model, newdata=seaflow_test, type="class")
summary(seaflow_test$pop == predictions) 

   Mode   FALSE    TRUE    NA's 
logical    2908   33263       0 

Accuracy = 0.9196

10) importance(rf_model)
          MeanDecreaseGini
fsc_small         2659.940
fsc_perp          2050.426
fsc_big            202.857
pe                8862.205
chl_big           4802.872
chl_small         8268.048

Answer - pe and chl_small

11) SVM: summary(seaflow_test$pop == predictions_svm)
   Mode   FALSE    TRUE    NA's 
logical    2906   33265       0 

Answer : 0.9196

12) Answer to 12: ultra is mistaken for pico

table(pred = predictions_svm, true = seaflow_test$pop)
         true
pred      crypto  nano  pico synecho ultra
  crypto      44     1     0       0     0
  nano         3  5634     0       3   339
  pico         0     0 10023      21  1384
  synecho      3     3    70    9029     7
  ultra        0   756   315       1  8535
  
13) Answer to 13: fsc_big is not a continuous variable. Can be found out by using plot().

14) summary(seaflow_red_test$pop == predictions_svm_reduced)
   Mode   FALSE    TRUE    NA's 
logical     870   29304       0

New Accuracy = 0.9712

Answer: Change in Accuracy for SVM - (0.9712 - 0.9196) = 0.05
###################################