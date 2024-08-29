

### INITIALISATION ----

load("data_test.rda")
load("data_train.rda")

dataTr<-data_train
data_all<-rbind(data_train[1:32], data_test)

data_quali<-dataTr[,-c(3,30, 31, 32, 33)]
data_quali_all<-data_all[,-c(3,30, 31, 32, 33)]

#variables quantitatives
dataTr$age<-as.numeric(dataTr$age)
dataTr$absences<-as.numeric(dataTr$absences)
dataTr$G1<-as.numeric(dataTr$G1)
dataTr$G2<-as.numeric(dataTr$G2)
dataTr$G3<-as.numeric(dataTr$G3)
var.numeric <- which(sapply(dataTr, class)=="numeric")
names(var.numeric)
length(var.numeric)


### DONNEES MANQUANTES ====


library(mice)
library(VIM)

## imputation
set.seed(250)
res.mice <- mice(dataTr, method='pmm', m=20, maxit = 5, printFlag = FALSE)
tableau1 <- complete(res.mice)
summary(tableau1)
plot(res.mice)
densityplot(res.mice, main='pmm, 10rep')


#impute data in the full dataset
full_dataTr <- complete(res.mice)
sum(is.na(full_dataTr))

### CLASSIFICATION DE VARIABLES -----

library(ClustOfVar)
library(factoextra)
library(ggplot2)


# only on the qualitative variables + age and absence - on all the data (train+test)

tree2 <- hclustvar(X.quanti=data_all[, c(3,30)], X.quali=data_quali_all)
plot(tree2)

# Cluster in 3 groups
part_hier3 <- cutreevar(tree2, 3)
str(part_hier3$score)

clustVar_all<-as.data.frame(part_hier3$score)
# Append to G1-G3 sur les datasets de train et test
data_test_clust3<-cbind(clustVar_all[434:649,], data_test[c(31,32)])
train_clust3_imput<-cbind(clustVar_all[1:433,], full_dataTr[c(31,32,33)])

# cluster in 6 groups
part_hier6 <- cutreevar(tree2, 6)
str(part_hier6$score)
clust6Var_all<-as.data.frame(part_hier6$score)

data_test_clust6<-cbind(clust6Var_all[434:649,], data_test[c(31,32)])
train_clust6_imput<-cbind(clust6Var_all[1:433,], full_dataTr[c(31,32,33)])


### CREATION TRAIN ET TEST DATASETS ######

test<-list()
ech<-list()
y_test<-list()

for (i in 1:5){
  set.seed(i)
  id <- sample(seq(nrow(dataTr)), size = ceiling(nrow(dataTr)*8/10))
  test[[i]] <- full_dataTr[-id, 1:32]
  ech[[i]] <- full_dataTr[id, ]
  y_test[[i]]<- full_dataTr[-id, 33]
}


test_clust3<-list()
ech_clust3<-list()
y_test_clust3<-list()
for (i in 1:5){
  set.seed(i)
  id <- sample(seq(nrow(dataTr)), size = ceiling(nrow(dataTr)*8/10))
  test_clust3[[i]] <- train_clust3_imput[-id, 1:5]
  ech_clust3[[i]] <- train_clust3_imput[id, ]
  y_test_clust3[[i]]<- train_clust3_imput[-id, 6]
}


test_clust6<-list()
ech_clust6<-list()
y_test_clust6<-list()
for (i in 1:5){
  set.seed(i)
  id <- sample(seq(nrow(dataTr)), size = ceiling(nrow(dataTr)*8/10))
  test_clust6[[i]] <- train_clust6_imput[-id, 1:8]
  ech_clust6[[i]] <- train_clust6_imput[id, ]
  y_test_clust6[[i]]<- train_clust6_imput[-id, 9]
}


### BENCHARMKING FUNCTION -----
performance_model_benchmark<- function (y_pred, true_values) {
  r2 <- cor(y_pred, true_values)^2
  mae <- mean(abs(y_pred - true_values))
  mse<-mean((y_pred - true_values)^2)
  return(list(r2, mae, mse))
}


### PARTIAL LEAST SQUARE REGRESSION ---------

library(pls)
library(plyr)

#### on 3 clustered groups of variables ----

result<-list()
results_plsr_clust3<-data.frame(row.names = c('R2', 'MAE', 'RSE'))

for (i in 1:5){
  test_mod<-as.data.frame(test_clust3[[i]])
  train_mod<-as.data.frame(ech_clust3[[i]])
  model_train <- plsr(G3~G1+G2+cluster1+cluster2+cluster3, data=train_mod, scale=TRUE, validation="CV")
  summary(model_train)
  validationplot(model_train, val.type="MSEP")
  validationplot(model_train, val.type="R2")
  pcr_pred_test <- predict(model_train, test_mod, ncomp=3)
  result[[i]]<-performance_model_benchmark(pcr_pred_test, y_test_clust3[[i]])
  df<-ldply(result[[i]], data.frame)
  results_plsr_clust3<-cbind(results_plsr_clust3, df)
}

results_plsr_clust3
avg_results_plsr_clust3<-rowMeans(results_plsr_clust3)
avg_results_plsr_clust3


#### using the rounding at the end ----

result<-list()
results_plsr_clust3_round<-data.frame(row.names = c('R2', 'MAE', 'RSE'))

for (i in 1:5){
  test_mod<-as.data.frame(test_clust3[[i]])
  train_mod<-as.data.frame(ech_clust3[[i]])
  model_train <- plsr(G3~G1+G2+cluster1+cluster2+cluster3, data=train_mod, scale=TRUE, validation="CV")
  summary(model_train)
  pcr_pred_test <- predict(model_train, test_mod, ncomp=5)
  pcr_pred_test<-round(pcr_pred_test, digits=0)
  result[[i]]<-performance_model_benchmark(pcr_pred_test, y_test_clust3[[i]])
  df<-ldply(result[[i]], data.frame)
  results_plsr_clust3_round<-cbind(results_plsr_clust3_round, df)
}

results_plsr_clust3_round
avg_results_plsr_clust3_round<-rowMeans(results_plsr_clust3_round)
avg_results_plsr_clust3_round


#### on 6 clustered groups of variables ----

result<-list()
results_plsr_clust6<-data.frame(row.names = c('R2', 'MAE', 'RSE'))

for (i in 1:5){
  test_mod<-as.data.frame(test_clust6[[i]])
  train_mod<-as.data.frame(ech_clust6[[i]])
  model_train <- plsr(G3~G1+G2+cluster1+cluster2+cluster3+cluster4+cluster5+cluster6, data=train_mod, scale=TRUE, validation="CV")
  summary(model_train)
  pcr_pred_test <- predict(model_train, test_mod, ncomp=5)
  result[[i]]<-performance_model_benchmark(pcr_pred_test, y_test_clust6[[i]])
  df<-ldply(result[[i]], data.frame)
  results_plsr_clust6<-cbind(results_plsr_clust6, df)
}

results_plsr_clust6
avg_results_plsr_clust6<-rowMeans(results_plsr_clust6)
avg_results_plsr_clust6

# using the rounding at the end

result<-list()
results_plsr_clust6_round<-data.frame(row.names = c('R2', 'MAE', 'RSE'))

for (i in 1:5){
  test_mod<-as.data.frame(test_clust6[[i]])
  train_mod<-as.data.frame(ech_clust6[[i]])
  model_train <- plsr(G3~., data=train_mod, scale=TRUE, validation="CV")
  summary(model_train)
  pcr_pred_test <- predict(model_train, test_mod, ncomp=5)
  pcr_pred_test<-round(pcr_pred_test, digits=0)
  result[[i]]<-performance_model_benchmark(pcr_pred_test, y_test_clust3[[i]])
  df<-ldply(result[[i]], data.frame)
  results_plsr_clust6_round<-cbind(results_plsr_clust6_round, df)
}

results_plsr_clust6_round
avg_results_plsr_clust6_round<-rowMeans(results_plsr_clust6_round)
avg_results_plsr_clust6_round

### BAGGED CART TREES -----

library(caret)
library(tree)

ctrl <- trainControl(method = "cv",  number = 50) 

result<-list()
results_Treebag<-data.frame(row.names = c('R2', 'MAE', 'RSE'))
for (i in 1:5){
  test_mod<-as.data.frame(test[[i]])
  train_mod<-as.data.frame(ech[[i]])
  
  bagged_cv <- train(
    G3 ~ .,
    data = train_mod,
    method = "treebag",
    trControl = ctrl,
    importance = TRUE
  )

  bagged_cv
  
  plot(varImp(bagged_cv), 20)  
  treeBag_pred_test <- predict(bagged_cv, test_mod)
  result[[i]]<-performance_model_benchmark(treeBag_pred_test, y_test[[i]])
  df<-ldply(result[[i]], data.frame)
  results_Treebag<-cbind(results_Treebag, df)
}

results_Treebag
avg_results_Treebag<-rowMeans(results_Treebag)
avg_results_Treebag



### RANDOM FORESTS -----

#### Using all variables ======

library(randomForest)
library(ggplot2)

result<-list()
results_RF<-data.frame(row.names = c('R2', 'MAE', 'RSE'))

for (i in 1:5){
  test_mod<-as.data.frame(test[[i]])
  train_mod<-as.data.frame(ech[[i]])
  rfModel <- randomForest(G3 ~. , mtry=12, data=train_mod, ntree=5000, importance=TRUE)
  RF_pred_test <- predict(rfModel, test_mod)
  result[[i]]<-performance_model_benchmark(RF_pred_test, y_test[[i]])
  df<-ldply(result[[i]], data.frame)
  results_RF<-cbind(results_RF, df)
}

results_RF
avg_results_RF<-rowMeans(results_RF)
avg_results_RF


#### on clustered variables in 3 groups ====


result<-list()
results_RF_clust3<-data.frame(row.names = c('R2', 'MAE', 'RSE'))

for (i in 1:5){
  test_mod<-as.data.frame(test_clust3[[i]])
  train_mod<-as.data.frame(ech_clust3[[i]])
  rfModel2 <- randomForest(G3 ~. , mtry=12, data=train_mod, ntree=5000, importance=TRUE)
  RF_pred_test <- predict(rfModel2, test_mod)
  result[[i]]<-performance_model_benchmark(RF_pred_test, y_test_clust3[[i]])
  df<-ldply(result[[i]], data.frame)
  results_RF_clust3<-cbind(results_RF_clust3, df)
}

results_RF_clust3
avg_results_RF_clust3<-rowMeans(results_RF_clust3)
avg_results_RF_clust3

### SUPPORT VECTOR MACHINES ------

#### Using 3 clusters of variables ====

library(e1071)
library(caret)
library(kernlab)

tuneGrid <- expand.grid(
  C = c(4, 8, 10, 12),
  sigma = c(0.001, 0.005, 0.01, 0.02)
)

ctrl <- trainControl(
  method = "cv",
  number = 10,
)

result<-list()
results_SVM_clust3<-data.frame(row.names = c('R2', 'MAE', 'RSE'))


for (i in 1:5){
  test_mod<-as.data.frame(test_clust3[[i]])
  train_mod<-as.data.frame(ech_clust3[[i]])
  modelSVM <- train(
    G3 ~ .,
    data = train_mod,
    method = 'svmRadial',
    preProcess = c("center", "scale"),
    trCtrl = ctrl,
    tuneGrid = tuneGrid
  )
  svm_pred_test <- predict(modelSVM, test_mod, ncomp=5)
  result[[i]]<-performance_model_benchmark(svm_pred_test, y_test_clust3[[i]])
  df<-ldply(result[[i]], data.frame)
  results_SVM_clust3<-cbind(results_SVM_clust3, df)
}

results_SVM_clust3
avg_results_SVM_clust3<-rowMeans(results_SVM_clust3)
avg_results_SVM_clust3


#### Using 6 clusters of variables ====

result<-list()
results_SVM_clust6<-data.frame(row.names = c('R2', 'MAE', 'RSE'))


for (i in 1:5){
  test_mod<-as.data.frame(test_clust6[[i]])
  train_mod<-as.data.frame(ech_clust6[[i]])
  modelSVM2 <- train(
    G3 ~ .,
    data = train_mod,
    method = 'svmRadial',
    preProcess = c("center", "scale"),
    trCtrl = ctrl,
    tuneGrid = tuneGrid
  )
  svm2_pred_test <- predict(modelSVM2, test_mod, ncomp=5)
  result[[i]]<-performance_model_benchmark(svm2_pred_test, y_test_clust6[[i]])
  df<-ldply(result[[i]], data.frame)
  results_SVM_clust6<-cbind(results_SVM_clust6, df)
}

results_SVM_clust6
avg_results_SVM_clust6<-rowMeans(results_SVM_clust6)
avg_results_SVM_clust6


# NEURAL NETWORKS --------

library(tensorflow)
library(keras)
library(tidyverse)
library(tidymodels)

#### Using 3 clusters  --------

normalizer <- layer_normalization(axis = -1L)
normalizer %>% adapt(as.matrix(train_clust3_imput[,-6]))

dnn_model2 <- keras_model_sequential() %>%
  normalizer()  %>%
  layer_dense(32, activation = 'relu', kernel_regularizer = regularizer_l2(0.0001)) %>%
  layer_dropout(0.2) %>%
  layer_dense(32, activation = 'relu', kernel_regularizer = regularizer_l2(0.0001)) %>%
  layer_dropout(0.2) %>%
  layer_dense(32, activation = 'relu', kernel_regularizer = regularizer_l2(0.0001)) %>%
  layer_dropout(0.2) %>%
  layer_dense(1)


dnn_model2 %>% compile(
  loss = 'mean_absolute_error',
  optimizer = optimizer_adam(0.001)
)


summary(dnn_model2)

result<-list()
results_dnn_clust3<-data.frame(row.names = c('R2', 'MAE', 'RSE'))

for (i in 1:5){
  test_mod<-as.data.frame(test_clust3[[i]])
  train_mod<-as.data.frame(ech_clust3[[i]])
  y_train_mod<-train_mod[,6]
  train_mod<-train_mod[,1:5]
  
  history2 <- dnn_model2 %>% fit(
    as.matrix(train_mod),
    as.matrix(y_train_mod),
    validation_split = 0.2,
    verbose = 0,
    epochs = 500
  )
  
  dnn_pred <- predict(dnn_model2, as.matrix(test_mod))
  result[[i]]<-performance_model_benchmark(dnn_pred, as.matrix(y_test_clust3[[i]]))
  df<-ldply(result[[i]], data.frame)
  results_dnn_clust3<-cbind(results_dnn_clust3, df)
}

results_dnn_clust3
avg_results_dnn_clust3<-rowMeans(results_dnn_clust3)
avg_results_dnn_clust3


#### Using 6 clusters -------

normalizer <- layer_normalization(axis = -1L)
normalizer %>% adapt(as.matrix(train_clust6_imput[,-9]))

dnn_model3 <- keras_model_sequential() %>%
  normalizer()  %>%
  layer_dense(32, activation = 'relu', kernel_regularizer = regularizer_l2(0.0001)) %>%
  layer_dropout(0.2) %>%
  layer_dense(32, activation = 'relu', kernel_regularizer = regularizer_l2(0.0001)) %>%
  layer_dropout(0.2) %>%
  layer_dense(32, activation = 'relu', kernel_regularizer = regularizer_l2(0.0001)) %>%
  layer_dropout(0.2) %>%
  layer_dense(1)


dnn_model3 %>% compile(
  loss = 'mean_absolute_error',
  optimizer = optimizer_adam(0.001)
)


summary(dnn_model3)

result<-list()
results_dnn_clust6<-data.frame(row.names = c('R2', 'MAE', 'RSE'))

for (i in 1:5){
  test_mod<-as.data.frame(test_clust6[[i]])
  train_mod<-as.data.frame(ech_clust6[[i]])
  y_train_mod<-train_mod[,9]
  train_mod<-train_mod[,1:8]
  
  history2 <- dnn_model3 %>% fit(
    as.matrix(train_mod),
    as.matrix(y_train_mod),
    validation_split = 0.2,
    verbose = 0,
    epochs = 500
  )
  
  dnn_pred2 <- predict(dnn_model3, as.matrix(test_mod))
  result[[i]]<-performance_model_benchmark(dnn_pred2, as.matrix(y_test_clust6[[i]]))
  df<-ldply(result[[i]], data.frame)
  results_dnn_clust6<-cbind(results_dnn_clust6, df)
}

results_dnn_clust6
avg_results_dnn_clust6<-rowMeans(results_dnn_clust6)
avg_results_dnn_clust6

### Methods comparison on clust3 -------

library(ggstatsplot)
library(afex) ## to run ANOVA

all_clust3<-data.frame()

all_clust3<-rbind(t(results_plsr_clust3)[1:5,3])
all_clust3<-rbind(all_clust3, t(results_RF_clust3)[1:5,3])
all_clust3<-rbind(all_clust3, t(results_SVM_clust3)[1:5,3])
all_clust3<-rbind(all_clust3, t(results_dnn_clust3)[1:5,3])


all_clust3<-t(all_clust3)
colnames(all_clust3)<-c('plsr', 'rf', 'svm', 'dnn')
rownames(all_clust3)<-c("s1", "s2", "s3", "s4", "s5")

library(tidyr)
library(data.table)

long<-melt(all_clust3)

p<- ggwithinstats(
  data    = long,
  x       = Var2,
  y       = value,
  grouping.var= Var1,
  xlab            = "",
  ylab            = "Mean square error",
  title   = "Models comparison on 5 bootstrap datasets"
)

p +
  ylim(0, 4)

