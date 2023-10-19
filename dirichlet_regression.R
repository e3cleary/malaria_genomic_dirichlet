rm(list = ls())

# Load libraries and data
library("DirichletReg")  
library(pROC)

# set working directory and read in data files

setwd("user/analysis_files")
data <- read.csv("cleary_et_al_data_file.csv", header = T)

data$lat2 <-  data$lat^2
data$long2 <-  data$long^2

# Split data into train and test
set.seed(65)
num_data <- nrow(data)
train_size <- round(num_data * 0.7)
train_index <- sample(num_data, size = train_size)
train <- data0[train_index, ]
test <- data0[-train_index, ]

# Define outcome variables
PF <- DR_data(data0[, 1:8]) 
#PF <- DR_data(train[, 1:8])

# Exploratory analysis
DR_quad <- DirichReg(PF ~ long + lat,  data)
DR_quad <- DirichReg(PF ~ long + lat + elev, data)
DR_quad <- DirichReg(PF ~ long + lat + elev + pop, data)
DR_quad <- DirichReg(PF ~ long + lat + elev + coast, data)
DR_quad <- DirichReg(PF ~ long + lat + elev + pop + coast, data)
DR_quad <- DirichReg(PF ~ long2 + lat2,  data)
DR_quad <- DirichReg(PF ~ long2 + lat2 + elev, data)
DR_quad <- DirichReg(PF ~ long2 + lat2 + elev + pop, data)
DR_quad <- DirichReg(PF ~ long2 + lat2 + elev + pop + coast, data)
DR_quad <- DirichReg(PF ~ long + lat + long2 + lat2,  data)
DR_quad <- DirichReg(PF ~ long + lat + long2 + lat2 + elev + pop + coast, data)
summary(DR_quad)
print(DR_quad)


# make predictions 
PF_predict <- read.csv("model_prediction_file.csv", header = T) 
head(PF_predict)

DR_quad_predict <- predict(DR_quad, newdata=PF_predict)  

write.csv(DR_quad_predict, "/analysis_files/model_predictions.csv")


# Make predictions
DR_quad_pred_train <- predict(DR_quad, train)
DR_quad_pred_train

DR_quad_pred_test <-  predict(DR_quad, test)
DR_quad_pred_test

# Convert predictions to dataframe

DR_quad_pred_train_df <- as.data.frame(DR_quad_pred_train)
DR_quad_pred_test_df <- as.data.frame(DR_quad_pred_test)


## Set levels in observed data before evaluating models

############# Cluster_1 ###############################

## Cluster_1
head(train)
range(train$Cluster_1)
median(train$Cluster_1)
mean(train$Cluster_1)

train_roc <- train
train_roc$Cluster_1[train_roc$Cluster_1 <.1] <- 0
train_roc$Cluster_1[train_roc$Cluster_1 >=.1] <- 1

train_roc <- train
train_roc$Cluster_1[train_roc$Cluster_1 <.25] <- 0
train_roc$Cluster_1[train_roc$Cluster_1 >=.25] <- 1

train_roc <- train
train_roc$Cluster_1[train_roc$Cluster_1 <.5] <- 0
train_roc$Cluster_1[train_roc$Cluster_1 >=.5] <- 1

train_roc <- train
train_roc$Cluster_1[train_roc$Cluster_1 <.039] <- 0
train_roc$Cluster_1[train_roc$Cluster_1 >=.039] <- 1


## Cluster_1 test
head(test)
range(test$Cluster_1)
median(test$Cluster_1)
mean(test$Cluster_1)

test_roc <- test
test_roc$Cluster_1[test_roc$Cluster_1 <.1] <- 0
test_roc$Cluster_1[test_roc$Cluster_1 >=.1] <- 1

test_roc <- test
test_roc$Cluster_1[test_roc$Cluster_1 <.25] <- 0
test_roc$Cluster_1[test_roc$Cluster_1 >=.25] <- 1

test_roc <- test
test_roc$Cluster_1[test_roc$Cluster_1 <.5] <- 0
test_roc$Cluster_1[test_roc$Cluster_1 >=.5] <- 1

test_roc <- test
test_roc$Cluster_1[test_roc$Cluster_1 <.0305] <- 0
test_roc$Cluster_1[test_roc$Cluster_1 >=.0305] <- 1


# Evaluate model predictions (after levels are set) 
roc_train <- roc(train_roc$Cluster_1, DR_quad_pred_train_df$V1)
roc_test <- roc(test_roc$Cluster_1, DR_quad_pred_test_df$V1)


# Show results
par(mfrow = c(1,2))  # two plots side-by-side
plot(roc_train, main = paste("train Cluster_1", signif(roc_train$auc,3))); 
plot(roc_test, main = paste("test Cluster_1", signif(roc_test$auc, 3) ))
par(mfrow = c(1,1))  # restore default


############# Cluster_2 ###############################

## Cluster_2 train
head(train)
range(train$Cluster_2)
median(train$Cluster_2)
mean(train$Cluster_2)

train_roc <- train
train_roc$Cluster_2[train_roc$Cluster_2 <.1] <- 0
train_roc$Cluster_2[train_roc$Cluster_2 >=.1] <- 1

train_roc <- train
train_roc$Cluster_2[train_roc$Cluster_2 <.25] <- 0
train_roc$Cluster_2[train_roc$Cluster_2 >=.25] <- 1

train_roc <- train
train_roc$Cluster_2[train_roc$Cluster_2 <.5] <- 0
train_roc$Cluster_2[train_roc$Cluster_2 >=.5] <- 1

train_roc <- train
train_roc$Cluster_2[train_roc$Cluster_2 <.108] <- 0
train_roc$Cluster_2[train_roc$Cluster_2 >=.108] <- 1


## Cluster_2 test
head(test)
range(test$Cluster_2)
median(test$Cluster_2)
mean(test$Cluster_2)

test_roc <- test
test_roc$Cluster_2[test_roc$Cluster_2 <.1] <- 0
test_roc$Cluster_2[test_roc$Cluster_2 >=.1] <- 1

test_roc <- test
test_roc$Cluster_2[test_roc$Cluster_2 <.25] <- 0
test_roc$Cluster_2[test_roc$Cluster_2 >=.25] <- 1

test_roc <- test
test_roc$Cluster_2[test_roc$Cluster_2 <.5] <- 0
test_roc$Cluster_2[test_roc$Cluster_2 >=.5] <- 1

test_roc <- test
test_roc$Cluster_2[test_roc$Cluster_2 <.108] <- 0
test_roc$Cluster_2[test_roc$Cluster_2 >=.108] <- 1


# Evaluate model predictions (after levels are set) 
roc_train <- roc(train_roc$Cluster_2, DR_quad_pred_train_df$V2)
roc_test <- roc(test_roc$Cluster_2, DR_quad_pred_test_df$V2)


# Show results
par(mfrow = c(1,2))  # two plots side-by-side
plot(roc_train, main = paste("train Cluster_2", signif(roc_train$auc,3))); 
plot(roc_test, main = paste("test Cluster_2", signif(roc_test$auc, 3) ))
par(mfrow = c(1,1))  # restore default


############# Cluster_3 ###############################

## Cluster_3 train
head(train)
range(train$Cluster_3)
median(train$Cluster_3)
mean(train$Cluster_3)

train_roc <- train
train_roc$Cluster_3[train_roc$Cluster_3 <.1] <- 0
train_roc$Cluster_3[train_roc$Cluster_3 >=.1] <- 1

train_roc <- train
train_roc$Cluster_3[train_roc$Cluster_3 <.25] <- 0
train_roc$Cluster_3[train_roc$Cluster_3 >=.25] <- 1

train_roc <- train
train_roc$Cluster_3[train_roc$Cluster_3 <.5] <- 0
train_roc$Cluster_3[train_roc$Cluster_3 >=.5] <- 1

train_roc <- train
train_roc$Cluster_3[train_roc$Cluster_3 <.024] <- 0
train_roc$Cluster_3[train_roc$Cluster_3 >=.024] <- 1


## Cluster_3 test
head(test)
range(test$Cluster_3)
median(test$Cluster_3)
mean(test$Cluster_3)

test_roc <- test
test_roc$Cluster_3[test_roc$Cluster_3 <.1] <- 0
test_roc$Cluster_3[test_roc$Cluster_3 >=.1] <- 1

test_roc <- test
test_roc$Cluster_3[test_roc$Cluster_3 <.25] <- 0
test_roc$Cluster_3[test_roc$Cluster_3 >=.25] <- 1

test_roc <- test
test_roc$Cluster_3[test_roc$Cluster_3 <.5] <- 0
test_roc$Cluster_3[test_roc$Cluster_3 >=.5] <- 1

test_roc <- test
test_roc$Cluster_3[test_roc$Cluster_3 <.024] <- 0
test_roc$Cluster_3[test_roc$Cluster_3 >=.024] <- 1


# Evaluate model predictions (after levels are set) 
roc_train <- roc(train_roc$Cluster_3, DR_quad_pred_train_df$V3)
roc_test <- roc(test_roc$Cluster_3, DR_quad_pred_test_df$V3)


# Show results
par(mfrow = c(1,2))  # two plots side-by-side
plot(roc_train, main = paste("train Cluster_3", signif(roc_train$auc,3))); 
plot(roc_test, main = paste("test Cluster_3", signif(roc_test$auc, 3) ))
par(mfrow = c(1,1))  # restore default

############# Cluster_4 ###############################

## Cluster_4 train
head(train)
range(train$Cluster_4)
median(train$Cluster_4)
mean(train$Cluster_4)

train_roc <- train
train_roc$Cluster_4[train_roc$Cluster_4 <.01] <- 0
train_roc$Cluster_4[train_roc$Cluster_4 >=.01] <- 1

train_roc <- train
train_roc$Cluster_4[train_roc$Cluster_4 <.025] <- 0
train_roc$Cluster_4[train_roc$Cluster_4 >=.025] <- 1

train_roc <- train
train_roc$Cluster_4[train_roc$Cluster_4 <.05] <- 0
train_roc$Cluster_4[train_roc$Cluster_4 >=.05] <- 1

train_roc <- train
train_roc$Cluster_4[train_roc$Cluster_4 <.03] <- 0
train_roc$Cluster_4[train_roc$Cluster_4 >=.03] <- 1


## Cluster_4 test
head(test)
range(test$Cluster_4)
median(test$Cluster_4)
mean(test$Cluster_4)

test_roc <- test
test_roc$Cluster_4[test_roc$Cluster_4 <.01] <- 0
test_roc$Cluster_4[test_roc$Cluster_4 >=.01] <- 1

test_roc <- test
test_roc$Cluster_4[test_roc$Cluster_4 <.025] <- 0
test_roc$Cluster_4[test_roc$Cluster_4 >=.025] <- 1

test_roc <- test
test_roc$Cluster_4[test_roc$Cluster_4 <.05] <- 0
test_roc$Cluster_4[test_roc$Cluster_4 >=.05] <- 1

test_roc <- test
test_roc$Cluster_4[test_roc$Cluster_4 <.03] <- 0
test_roc$Cluster_4[test_roc$Cluster_4 >=.03] <- 1


# Evaluate model predictions (after levels are set) 
roc_train <- roc(train_roc$Cluster_4, DR_quad_pred_train_df$V4)
roc_test <- roc(test_roc$Cluster_4, DR_quad_pred_test_df$V4)


# Show results
par(mfrow = c(1,2))  # two plots side-by-side
plot(roc_train, main = paste("train Cluster_4", signif(roc_train$auc,3))); 
plot(roc_test, main = paste("test Cluster_4", signif(roc_test$auc, 3) ))
par(mfrow = c(1,1))  # restore default


############# Cluster_5 ###############################

## Cluster_5 train
head(train)
range(train$Cluster_5)
median(train$Cluster_5)
mean(train$Cluster_5)

train_roc <- train
train_roc$Cluster_5[train_roc$Cluster_5 <.1] <- 0
train_roc$Cluster_5[train_roc$Cluster_5 >=.1] <- 1

train_roc <- train
train_roc$Cluster_5[train_roc$Cluster_5 <.25] <- 0
train_roc$Cluster_5[train_roc$Cluster_5 >=.25] <- 1

train_roc <- train
train_roc$Cluster_5[train_roc$Cluster_5 <.5] <- 0
train_roc$Cluster_5[train_roc$Cluster_5 >=.5] <- 1

train_roc <- train
train_roc$Cluster_5[train_roc$Cluster_5 <.033] <- 0
train_roc$Cluster_5[train_roc$Cluster_5 >=.033] <- 1


## Cluster_5 test
head(test)
range(test$Cluster_5)
median(test$Cluster_5)
mean(test$Cluster_5)

test_roc <- test
test_roc$Cluster_5[test_roc$Cluster_5 <.1] <- 0
test_roc$Cluster_5[test_roc$Cluster_5 >=.1] <- 1

test_roc <- test
test_roc$Cluster_5[test_roc$Cluster_5 <.25] <- 0
test_roc$Cluster_5[test_roc$Cluster_5 >=.25] <- 1

test_roc <- test
test_roc$Cluster_5[test_roc$Cluster_5 <.5] <- 0
test_roc$Cluster_5[test_roc$Cluster_5 >=.5] <- 1

test_roc <- test
test_roc$Cluster_5[test_roc$Cluster_5 <.033] <- 0
test_roc$Cluster_5[test_roc$Cluster_5 >=.033] <- 1


# Evaluate model predictions (after levels are set) 
roc_train <- roc(train_roc$Cluster_5, DR_quad_pred_train_df$V5)
roc_test <- roc(test_roc$Cluster_5, DR_quad_pred_test_df$V5)


# Show results
par(mfrow = c(1,2))  # two plots side-by-side
plot(roc_train, main = paste("train Cluster_5", signif(roc_train$auc,3))); 
plot(roc_test, main = paste("test Cluster_5", signif(roc_test$auc, 3) ))
par(mfrow = c(1,1))  # restore default


############# Cluster_6 ###############################

## Cluster_6 train
head(train)
range(train$Cluster_6)
median(train$Cluster_6)
mean(train$Cluster_6)

train_roc <- train
train_roc$Cluster_6[train_roc$Cluster_6 <.01] <- 0
train_roc$Cluster_6[train_roc$Cluster_6 >=.01] <- 1

train_roc <- train
train_roc$Cluster_6[train_roc$Cluster_6 <.025] <- 0
train_roc$Cluster_6[train_roc$Cluster_6 >=.025] <- 1

train_roc <- train
train_roc$Cluster_6[train_roc$Cluster_6 <.05] <- 0
train_roc$Cluster_6[train_roc$Cluster_6 >=.05] <- 1

train_roc <- train
train_roc$Cluster_6[train_roc$Cluster_6 <.065] <- 0
train_roc$Cluster_6[train_roc$Cluster_6 >=.065] <- 1


## Cluster_6 test
head(test)
range(test$Cluster_6)
median(test$Cluster_6)
mean(test$Cluster_6)

test_roc <- test
test_roc$Cluster_6[test_roc$Cluster_6 <.01] <- 0
test_roc$Cluster_6[test_roc$Cluster_6 >=.01] <- 1

test_roc <- test
test_roc$Cluster_6[test_roc$Cluster_6 <.025] <- 0
test_roc$Cluster_6[test_roc$Cluster_6 >=.025] <- 1

test_roc <- test
test_roc$Cluster_6[test_roc$Cluster_6 <.05] <- 0
test_roc$Cluster_6[test_roc$Cluster_6 >=.05] <- 1

test_roc <- test
test_roc$Cluster_6[test_roc$Cluster_6 <.065] <- 0
test_roc$Cluster_6[test_roc$Cluster_6 >=.065] <- 1


# Evaluate model predictions (after levels are set) 
roc_train <- roc(train_roc$Cluster_6, DR_quad_pred_train_df$V6)
roc_test <- roc(test_roc$Cluster_6, DR_quad_pred_test_df$V6)


# Show results
par(mfrow = c(1,2))  # two plots side-by-side
plot(roc_train, main = paste("train Cluster_6", signif(roc_train$auc,3))); 
plot(roc_test, main = paste("test Cluster_6", signif(roc_test$auc, 3) ))
par(mfrow = c(1,1))  # restore default


############# Cluster_7 ###############################

## Cluster_7 train
head(train)
range(train$Cluster_7)
median(train$Cluster_7)
mean(train$Cluster_7)

train_roc <- train
train_roc$Cluster_7[train_roc$Cluster_7 <.1] <- 0
train_roc$Cluster_7[train_roc$Cluster_7 >=.1] <- 1

train_roc <- train
train_roc$Cluster_7[train_roc$Cluster_7 <.025] <- 0
train_roc$Cluster_7[train_roc$Cluster_7 >=.025] <- 1

train_roc <- train
train_roc$Cluster_7[train_roc$Cluster_7 <.05] <- 0
train_roc$Cluster_7[train_roc$Cluster_7 >=.05] <- 1

train_roc <- train
train_roc$Cluster_7[train_roc$Cluster_7 <.017] <- 0
train_roc$Cluster_7[train_roc$Cluster_7 >=.017] <- 1


## Cluster_7 test
head(test)
range(test$Cluster_7)
median(test$Cluster_7)
mean(test$Cluster_7)

test_roc <- test
test_roc$Cluster_7[test_roc$Cluster_7 <.1] <- 0
test_roc$Cluster_7[test_roc$Cluster_7 >=.1] <- 1

test_roc <- test
test_roc$Cluster_7[test_roc$Cluster_7 <.025] <- 0
test_roc$Cluster_7[test_roc$Cluster_7 >=.025] <- 1

test_roc <- test
test_roc$Cluster_7[test_roc$Cluster_7 <.05] <- 0
test_roc$Cluster_7[test_roc$Cluster_7 >=.05] <- 1

test_roc <- test
test_roc$Cluster_7[test_roc$Cluster_7 <.017] <- 0
test_roc$Cluster_7[test_roc$Cluster_7 >=.017] <- 1


# Evaluate model predictions (after levels are set) 
roc_train <- roc(train_roc$Cluster_7, DR_quad_pred_train_df$V7)
roc_test <- roc(test_roc$Cluster_7, DR_quad_pred_test_df$V7)


# Show results
par(mfrow = c(1,2))  # two plots side-by-side
plot(roc_train, main = paste("train Cluster_7", signif(roc_train$auc,3))); 
plot(roc_test, main = paste("test Cluster_7", signif(roc_test$auc, 3) ))
par(mfrow = c(1,1))  # restore default


############# Cluster_8 ###############################

## Cluster_8 train
head(train)
range(train$Cluster_8)
median(train$Cluster_8)
mean(train$Cluster_8)

train_roc <- train
train_roc$Cluster_8[train_roc$Cluster_8 <.01] <- 0
train_roc$Cluster_8[train_roc$Cluster_8 >=.01] <- 1

train_roc <- train
train_roc$Cluster_8[train_roc$Cluster_8 <.025] <- 0
train_roc$Cluster_8[train_roc$Cluster_8 >=.025] <- 1

train_roc <- train
train_roc$Cluster_8[train_roc$Cluster_8 <.05] <- 0
train_roc$Cluster_8[train_roc$Cluster_8 >=.05] <- 1

train_roc <- train
train_roc$Cluster_8[train_roc$Cluster_8 <.03] <- 0
train_roc$Cluster_8[train_roc$Cluster_8 >=.03] <- 1


## Cluster_8 test
head(test)
range(test$Cluster_8)
median(test$Cluster_8)
mean(test$Cluster_8)

test_roc <- test
test_roc$Cluster_8[test_roc$Cluster_8 <.01] <- 0
test_roc$Cluster_8[test_roc$Cluster_8 >=.01] <- 1

test_roc <- test
test_roc$Cluster_8[test_roc$Cluster_8 <.025] <- 0
test_roc$Cluster_8[test_roc$Cluster_8 >=.025] <- 1

test_roc <- test
test_roc$Cluster_8[test_roc$Cluster_8 <.05] <- 0
test_roc$Cluster_8[test_roc$Cluster_8 >=.05] <- 1

test_roc <- test
test_roc$Cluster_8[test_roc$Cluster_8 <.03] <- 0
test_roc$Cluster_8[test_roc$Cluster_8 >=.03] <- 1


# Evaluate model predictions (after levels are set) 
roc_train <- roc(train_roc$Cluster_8, DR_quad_pred_train_df$V8)
roc_test <- roc(test_roc$Cluster_8, DR_quad_pred_test_df$V8)


# Show results
par(mfrow = c(1,2))  # two plots side-by-side
plot(roc_train, main = paste("train Cluster_8", signif(roc_train$auc,3))); 
plot(roc_test, main = paste("test Cluster_8", signif(roc_test$auc, 3) ))
par(mfrow = c(1,1))  # restore default


