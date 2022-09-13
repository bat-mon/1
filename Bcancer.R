install.packages("tidyverse")
install.packages("caret")
install.packages("ggcorrplot")
install.packages("GGally")
install.packages("randomForest")
install.packages("e1071")
install.packages("ROCR")
install.packages("pROC")
install.packages("RCurl")
install.packages("rmarkdown")
library(tidyverse)
library(caret)
library(ggcorrplot)
library(GGally)
library(randomForest)
library(e1071)
library(ROCR)
library(pROC)
library(RCurl)
library(rmarkdown)
UCI_data_URL = getURL('https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data')
names = c('id_number', 'diagnosis', 'radius_mean', 
           'texture_mean', 'perimeter_mean', 'area_mean', 
           'smoothness_mean', 'compactness_mean', 
           'concavity_mean','concave_points_mean', 
           'symmetry_mean', 'fractal_dimension_mean',
           'radius_se', 'texture_se', 'perimeter_se', 
           'area_se', 'smoothness_se', 'compactness_se', 
           'concavity_se', 'concave_points_se', 
           'symmetry_se', 'fractal_dimension_se', 
           'radius_worst', 'texture_worst', 
           'perimeter_worst', 'area_worst', 
           'smoothness_worst', 'compactness_worst', 
           'concavity_worst', 'concave_points_worst', 
           'symmetry_worst', 'fractal_dimension_worst')
##

breast_cancer = read.table(textConnection(UCI_data_URL),
                            sep = ',', col.names = names)
breast_cancer$id_number = NULL
head(breast_cancer)
breast_cancer %>% dim()
breast_cancer %>% str()
#To check for class imbalance
breast_cancer %>% 
  count(diagnosis) %>%
  group_by(diagnosis) %>%
  summarize(perc_dx = round((n / 569)* 100, 2))
#Creating traset.seed(42)
trainIndex = createDataPartition(breast_cancer$diagnosis, 
                                  p = .8, list = FALSE, 
                                  times = 1)
training_set = breast_cancer[ trainIndex, ]
test_set = breast_cancer[ -trainIndex, ]
#Hyperparameters optimisation
customRF = list(type = "Classification", 
                 library = "randomForest", loop = NULL)
customRF$parameters = data.frame(parameter = c("mtry",
                                                "ntree", "nodesize"), 
                                  class = rep("numeric", 3), 
                                  label = c("mtry", "ntree", "nodesize"))
customRF$grid = function(x, y, len = NULL, search = "grid") {}
customRF$fit = function(x, y, wts, param, lev, 
                         last, weights, classProbs, ...) {
  randomForest(x, y, mtry = param$mtry, ntree=param$ntree,
               nodesize=param$nodesize, ...)
}
customRF$predict = function(modelFit, newdata, 
                             preProc = NULL, submodels = NULL)
predict(modelFit, newdata)
customRF$prob = function(modelFit, newdata, 
                          preProc = NULL, 
                          submodels = NULL)predict(modelFit, 
                                                   newdata, 
                                                   type = "prob")
customRF$sort = function(x) x[order(x[,1]),]
customRF$levels = function(x) x$classes
## 3 fold repeated 10 times
fitControl = trainControl(method = "repeatedcv",
  number = 3, 
  repeats = 10)

grid = expand.grid(.mtry=c(floor(sqrt(ncol(training_set))),
                           (ncol(training_set) - 1), 
                           floor(log(ncol(training_set)))), 
                    .ntree = c(100, 300, 500, 1000),
                    .nodesize =c(1:4))
dev.off()
set.seed(42)
fit_rf = train(as.factor(diagnosis) ~ ., data = training_set,
                method = customRF, metric = "Accuracy", 
                tuneGrid= grid,trControl = fitControl)
a=fit_rf$finalModel
print(a)
predict_values = predict(fit_rf, newdata = test_set)
ftable(predict_values, test_set$diagnosis)

