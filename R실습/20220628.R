setwd("c:/Rdata")
head(iris)
table(iris)
str(iris)
names(iris)
#과적합(오버피팅)
150*0.7
iris_train=iris[1:105,]#70%
iris_test=iris[106:150,]#30%
table(iris_train$Species)
table(iris_test$Species)
nrow(iris)
idx=sample(1:nrow(iris),size = nrow(iris)*0.7,replace = F)
idx
iris_train=iris[idx,]
iris_test=iris[-idx,]
dim(iris_train)
dim(iris_test)
table(iris_train$Species)
table(iris_test$Species)

install.packages("caret")
library(caret)
install.packages("ipred")
library(ipred)
install.packages("recipes", type = 'binary') 
train_idx=createDataPartition(iris$Species,p=0.7,list=F)
iris_train=iris[idx,]
iris_test=iris[-idx,]
table(iris_train$Species)
iris_train$Species
table(iris_test$Species)
iris_test$Species

library(e1071)
naive_rlt=naiveBayes(iris_train,iris_train$Species,laplace = 1)
naive_pdt=predict(naive_rlt,iris_test,type="class")
table(naive_pdt,iris_test$Species)
confusionMatrix(naive_pdt,iris_test$Species)

library(nnet)
model=multinom(Species~.,data=iris_train)
rlt=predict(model,iris_test)
rlt
table(rlt,iris_test$Species)
confusionMatrix(rlt,iris_test$Species)

library(rpart)
model=rpart(Species~.,data=iris_train)
rlt=predict(model,iris_test,type = "class")
table(rlt,iris_test$Species)
cbind(iris_test$Species,rlt)
rlt
confusionMatrix(rlt,iris_test$Species)

model=nnet(Species~.,data=iris_train,size=2)
rlt=predict(model,iris_test,type = "class")
rlt
cbind(iris_test$Species,rlt)
table(iris_test$Species,rlt)
iris_test$Species=as.factor(iris_test$Species)
rlt=as.factor(rlt)
confusionMatrix(iris_test$Species,rlt)

install.packages("kernlab")
library(kernlab)
model=ksvm(Species~.,data=iris_train,kernel="rbfdot")
rlt=predict(model,iris_test,type="response")
cbind(iris_test$Species,rlt)
table(iris_test$Species,rlt)
confusionMatrix(iris_test$Species,rlt)

install.packages("ranger")
library(ranger)
rf_rlt=ranger(Species~.,data = iris_train,ntree=500)
rf_pdt=predict(rf_rlt,iris_test,type="response")
rf_pdt$predictions
cbind(iris_test$Species,rf_pdt$predictions)
table(iris_test$Species,rf_pdt$predictions)
confusionMatrix(iris_test$Species,rf_pdt$predictions)

mdl_fit=ranger(Species~.,data = iris_train,num.trees = 500,mtry = 2,importance = 'impurity')
mdl_fit
rf_pdt=predict(mdl_fit,iris_test)
table(iris_test$Species,rf_pdt$predictions)
confusionMatrix(iris_test$Species,rf_pdt$predictions)


library(MASS)
idx=sample(1:nrow(Boston),size = nrow(Boston)*0.7,replace = F)
Boston_train=Boston[idx,]
Boston_test=Boston[-idx,]
str(Boston_train)
str(Boston_test)

lm_fit=lm(medv~ .,data = Boston_train) 
summary(lm_fit)
lm_fit2=step(lm_fit,method="both")
summary(lm_fit2)
lm_yhat2=predict(lm_fit2,newdata=Boston_test)
plot(lm_yhat2,Boston_test$medv)
abline(a=0,b=1,col=2)
cbind(Boston_test$medv,lm_yhat2)
kk=mean((lm_yhat2-Boston_test$medv)^2)
sqrt(kk)
