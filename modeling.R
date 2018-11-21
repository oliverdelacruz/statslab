#####################################################
### Load necessary files
#####################################################
# Add libraries
library(dplyr)
library(lattice)
library(caret)
library(ggplot2)
library(randomForest)
library(psych)
library(corrplot)
library(scatterplot3d)
library(MASS)
library(cluster)
require(mclust)
library(glmnet)
library(lars)
library(e1071)
library(lmridge)
# Read files
rm(list=ls())
cat("\014")
path_data <- "../data/"
master_data <- readRDS(paste(path_data,"mastertable.rds",sep=""))
log_data <-  readRDS(paste(path_data,"logDataAnonym.rds",sep=""))
course_data <-  readRDS(paste(path_data,"quizList.rds",sep=""))
exam_data <-  readRDS(paste(path_data,"exam.rds",sep=""))
#####################################################
### Select relevant data for analysis
#####################################################
# 1 Data set
#####################################################
# Remove students who do not interacted with system
master_data <- readRDS(paste(path_data,"all_created_variables.rds",sep=""))
master_data[is.na(master_data[,177]),177:185] <- 0
# Select relevant features
data1 <- master_data[, 177:185]
# Setup column names
colnames_log <- c("Test - has had their attempt",
                  "Test - has started the attempt",
                  "Test - has submitted the attempt",
                  "Test - has viewed attempt",
                  "Test - has viewed the summary for the attempt",
                  "Kernsystem - updated the grade",
                  "Textseite - viewed the page activity course module",
                  "Test - viewed the quizz activity",
                  "Kernsystem - viewed the section number")
colnames(data1) <- colnames_log 
#####################################################
# 2 Data set
#####################################################
# Data set with pys parameters
data2 <-  readRDS(paste(path_data,"model_data_psy_var.rds",sep=""))
#####################################################
# 3 Data set
#####################################################
# Data set with non pys parameters
data3 <-  readRDS(paste(path_data,"model_data_nonpsy_var.rds",sep=""))
#####################################################
# Response variable
#####################################################
# labels - grades
y <- master_data[,c("Total")]
#####################################################
# Analysis
#####################################################
# Cross validation function 
cv.10f <- function(fun, data, ...){
  nr.folds <- 10
  set.seed(13)
  flds     <- createFolds(data$Total, k = nr.folds)
  mse.flds <- numeric(nr.folds)
  pred.val <- numeric(nrow(data))
  for(i in 1:nr.folds){
    mod <- fun(Total~., data[-flds[[i]],], ...)
    pred.val[flds[[i]]] <- predict(mod, newdata = data[flds[[i]],]) 

    mse.flds[i] <- mean((pred.val[flds[[i]]] - data$Total[flds[[i]]])^2)
  }
  return(list("MSE" = mean(mse.flds), "pred" = pred.val))
}
#####################################################
# Select data for analysis
data <-  data3
bool_data <- TRUE
data <- data[,-which(names(data) %in% c("Grade","Failed","Rep"))]
# Analyse independent variables
par(mfrow=c(NCOL(data1)/3,3)) 
for (i in 1:NCOL(data1)) hist(data1[,i], main=names(data1)[i], xlab =names(data1)[i])
par(mfrow=c(1,1))
## Example how to use the function: 
rf.d1 <- cv.10f(fun = randomForest, data = data)
#####################################################
# Cluster and PCA
# Select data
data_psy_sem1 <- data2[,27:35]
data_psy_sem2  <- data2[,36:44]
pca_psy <- princomp(data_psy_sem1)
pca_log <- princomp(data1)
cond_grade <- as.integer(master_data$Grade >= 5) + 10
cond_grade_psy <- as.integer(data2$Grade >= 5) + 10
old.par <- par(mar = c(5, 5, 5, 5))
# Plot data
plot(pca_psy$scores[,1],pca_psy$scores[,2],col = cond_grade_psy, ylab= "2nd Principal Component - Psy",
     xlab = "1st Principal Component - Psy", pch =20)
plot(-pca_log$scores[,1],master_data$Total, ylab = "Number of Total Points",
     xlab = "1st Principal Component", col = cond_grade, pch = 20)
plot(-pca_psy$scores[,1],data2$Total, ylab = "Number of Total Points",
     xlab = "1st Principal Component - Psy", col = cond_grade_psy , pch = 20)
scatterplot3d(-pca_psy$scores[,1],-pca_psy$scores[,2],-pca_psy$scores[,3], 
              color = cond_grade_psy, pch = 20,
              ylab = "2nd Principal Component",
              xlab = "1st Principal Component",
              zlab = "3rd Principal Component")
res_kmeans <- kmeans(pca_psy$scores[,1:7],4)
# Kmeans - clustering
plot(data2$Total, data2$Total, col = res_kmeans$cluster + 1, pch = 20,
     xlab = "Number of Total of points",
     ylab = "Number of Total of points")
plot(data2$Total, res_kmeans$cluster , col = res_kmeans$cluster + 1, pch = 20,
     xlab = "Number of Total of points",
     ylab = "Number of Total of points")
plot(res_kmeans$cluster,data2$Total, col = res_kmeans$cluster + 1, pch = 20,
     xlab = "Number of Total of points",
     ylab = "Clusters")
# Gaussian mixture - clustering
res_gaussian <- Mclust(pca_psy$scores[,1:7], G = 4)
plot(data2$Total, data2$Total, col = res_gaussian$classification + 1, pch = 20,
     xlab = "Number of Total of points",
     ylab = "Number of Total of points")
plot(data2$Total, res_gaussian$classification , col = res_gaussian$classification + 1, pch = 20,
     xlab = "Number of Total of points",
     ylab = "Number of Total of points")
plot(res_gaussian$classification,data2$Total, col = res_gaussian$classification + 1, pch = 20,
     xlab = "Number of Total of points",
     ylab = "Clusters")
par(old.par)
#####################################################
# Exploratory analysis
pairs(data1[,1:7], col = cond_grade, pch = 20)
#####################################################
# Variable selection
# - Linear regression
f.full <- lm(data$Total ~ ., data = data)
f.null <- lm(data$Total ~ 1, data = data)
sc <- list(lower=f.null, upper=f.full)
f.both <- step(f.null, scope=sc, direction="both", k=2) 
summary(f.both)
variables_selected_lm <- names(f.both$coefficients)
variables_selected_lm <- variables_selected_lm[c(-1)]
# - Lasso regression
drops <- c("Total")
cvfit_lasso <-  cv.glmnet(as.matrix(data[, !(names(data) %in% drops)]), data$Total, type.measure = "mse", nfolds = 10, alpha = 1)
summary(cvfit_lasso)
co<-coef(cvfit_lasso ,s = "lambda.min")
inds<-which(co!=0)
variables_lasso <-row.names(co)[inds]
variables_lasso <- variables_lasso[!(variables_lasso %in% '(Intercept)')];
# Final set of variables
features <- unique(as.vector(cbind(variables_lasso,variables_selected_lm)))
#####################################################
# Data preparation
data_full <- data
data_selected_var <- data[,c(features,"Total")]
data_full_out <- data[,- which(names(data) %in% c("Total"))]
data_selected_out <- data_full_out [,c(features)]
y_labels <- data$Total
#####################################################
# Linear regression - all - CAN BE WRITTEN INTO A FUNCTION
fit_all <- lm(data_full$Total ~ ., data = data_full)
summary(fit_all)
# Linear regression - selected
fit_all_selected <- lm(data_full$Total ~ ., data = data_selected_var)
summary(fit_all_selected)
# Linear regression - pca all
pca_var_all <- princomp(data_full_out)
fit_pca_all <- lm(y_labels ~ . , data = as.data.frame(pca_var_all$scores))
summary(fit_pca_all)
#Linear regression - pca selected
pca_var_selected <- princomp(data_selected_out)
fit_pca_selected<- lm(y_labels ~ . , data = as.data.frame(pca_var_selected$scores))
summary(fit_pca_selected)
# Backward & Forward selection - selected
f.full <- lm(data_selected_var$Total ~ ., data= as.data.frame(data_selected_var))
f.null <- lm(data_selected_var$Total ~ 1, data= as.data.frame(data_selected_var))
sc <- list(lower=f.null, upper=f.full)
f.both <- step(f.null, scope=sc, direction="both", k=2) 
summary(f.both)
variables_selected <- names(f.both$coefficients)
variables_selected <- variables_selected[c(-1)]
data_selected <- cbind(data_selected_var[,variables_selected],y_labels)
names(data_selected)[NROW(names(data_selected))] <- "Total"
# Backward & Forward selection - all pca
f.full <- lm(y_labels ~ ., data = as.data.frame(pca_var_all$scores))
f.null <- lm(y_labels ~ 1, data = as.data.frame(pca_var_all$scores))
sc <- list(lower=f.null, upper=f.full)
f.both <- step(f.null, scope=sc, direction="both", k=2) 
summary(f.both)
variables_selected <- names(f.both$coefficients)
variables_selected <- variables_selected[c(-1)]
data_pca_all <- cbind(as.data.frame(pca_var_all$scores[,variables_selected]),y_labels)
names(data_pca_all)[NROW(names(data_pca_all))] <- "Total"
# Backward & Forward selection - pca
f.full <- lm(y_labels ~ ., data= as.data.frame(pca_var_selected$scores))
f.null <- lm(y_labels ~ 1, data= as.data.frame(pca_var_selected$scores))
sc <- list(lower=f.null, upper=f.full)
f.both_pca <- step(f.null, scope=sc, direction="both", k=2) 
summary(f.both_pca)
variables_selected <- names(f.both_pca$coefficients)
variables_selected <- variables_selected[c(-1)]
data_pca_selected <- cbind(as.data.frame(pca_var_selected$scores[,variables_selected]),y_labels)
names(data_pca_selected)[NROW(names(data_pca_selected))] <- "Total"
# Cross validation - Best models 
if (bool_data == TRUE) {
  idx_rm <- -12
  idx_beg <- -12
} else {
  idx_rm <- NCOL(data_pca_selected)
  idx_beg <- 1
}
res_lm <- cv.10f(fun = lm, data = data_selected)
res_pca_selected <- cv.10f(fun = lm, data = data_pca_selected[,idx_beg:idx_rm])
res_pca_all <- cv.10f(fun = lm, data = data_pca_all)
#####################################################
# General function for cross validation 
cv.10f <- function(fun, data, ...){
  nr.folds <- 10
  flds     <- createFolds(data$Total, k = nr.folds)
  mse.flds <- numeric(nr.folds)
  pred.val <- numeric(nrow(data))
  for(i in 1:nr.folds){
    mod <- fun(Total~., data[-flds[[i]],], ...)
    pred.val[flds[[i]]] <- predict(mod, newdata = data[flds[[i]],]) 
    
    mse.flds[i] <- mean((pred.val[flds[[i]]] - data$Total[flds[[i]]])^2)
  }
  return(list("MSE" = mean(mse.flds), "pred" = pred.val))
}
#####################################################
# SVM regression
# Cross validation - guassian
res_svm <- cv.10f(fun = svm, data = data_selected)
res_svm_pca_selected <- cv.10f(fun = svm, data = data_pca_selected[,idx_beg:idx_rm])
res_svm_pca_all <- cv.10f(fun = svm, data =  data_pca_all)
# Cross validation - linear
res_svm_linear <- cv.10f(fun = svm, data = data_selected, type = 'eps-regression', kernel ='linear')
res_svm_pca_selected_linear <- cv.10f(fun = svm, data = data_pca_selected[,idx_beg :idx_rm], type = 'eps-regression', kernel ='linear')
res_svm_pca_all_linear <- cv.10f(fun = svm, data =  data_pca_all,type = 'eps-regression', kernel ='linear')
# Cross validation - sigmoid
res_svm_sigmoid <- cv.10f(fun = svm, data = data_selected, type = 'eps-regression', kernel ='sigmoid')
res_svm_pca_selected_sigmoid <- cv.10f(fun = svm, data = data_pca_selected[,idx_beg:idx_rm], type = 'eps-regression', kernel ='sigmoid')
res_svm_pca_all_sigmoid <- cv.10f(fun = svm, data =  data_pca_all, type = 'eps-regression', kernel ='sigmoid')
# Cross validation -  polynomial
res_svm_poly <- cv.10f(fun = svm, data = data_selected, type = 'eps-regression', kernel ='polynomial')
res_svm_pca_selected_poly <- cv.10f(fun = svm, data = data_pca_selected[,idx_beg:idx_rm], type = 'eps-regression', kernel ='polynomial')
res_svm_pca_all_poly <- cv.10f(fun = svm, data =  data_pca_all, type = 'eps-regression', kernel ='polynomial')
#####################################################
# Ridge regression
# Cross validation
res_lm_ridge_full <- cv.10f(fun = lmridge, data =  data_total)
res_lm_ridge <- cv.10f(fun = lmridge, data =  data_selected)
res_lm_ridge_pca_selected  <- cv.10f(fun = lmridge, data = data_pca_selected[,idx_beg:idx_rm])
res_lm_ridge_pca_all <- cv.10f(fun = lmridge, data = data_pca_all)
#####################################################
# Lasso regression
cvfit_lasso_full <-  cv.glmnet(as.matrix(data_full[,- which(names(data_full) %in% c("Total"))]), y_labels, type.measure = "mse", nfolds = 10, alpha = 1)
cvfit_lasso_selected <-  cv.glmnet(as.matrix(data_selected_out), y_labels, type.measure = "mse", nfolds = 10, alpha = 1)
cvfit_lasso_pca_selected <-  cv.glmnet(as.matrix(data_pca_selected[,- which(names(data_pca_selected) %in% c("Total"))]), y_labels, type.measure = "mse", nfolds = 10, alpha = 1)
cvfit_lasso_pca_all <-  cv.glmnet(as.matrix(data_pca_all[,- which(names(data_pca_all) %in% c("Total"))]), y_labels, type.measure = "mse", nfolds = 10, alpha = 1)
# Cross validation
mse.min_full <- cvfit_lasso_full$cvm[cvfit_lasso_full$lambda == cvfit_lasso_full$lambda.min]
mse.min_selected <-cvfit_lasso_selected$cvm[cvfit_lasso_selected$lambda == cvfit_lasso_selected$lambda.min]
mse.min_pca_selected <- cvfit_lasso_pca_selected$cvm[cvfit_lasso_pca_selected$lambda == cvfit_lasso_pca_selected$lambda.min]
mse.min_pca_all <- cvfit_lasso_pca_all$cvm[cvfit_lasso_pca_all$lambda == cvfit_lasso_pca_all$lambda.min]
