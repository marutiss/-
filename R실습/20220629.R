library(MASS)
idx=sample(1:nrow(Boston),size = nrow(Boston)*0.7,replace = F)
Boston_train=Boston[idx,]
Boston_test=Boston[-idx,]
str(Boston_train)
str(Boston_test)

lm_fit=lm(medv~ .,data = Boston_train) 
summary(lm_fit)
lm_fit2=step(lm_fit,method="both") #forward방식
summary(lm_fit2)

lm_yhat2=predict(lm_fit2,newdata=Boston_test)
plot(lm_yhat2,Boston_test$medv)
abline(a=0,b=1,col=2)
cbind(Boston_test$medv,lm_yhat2) #backward방식식
kk=mean((lm_yhat2-Boston_test$medv)^2)
sqrt(kk)

install.packages("tree")
library(tree)
tree_fit=tree(medv~.,data=Boston_train)
summary(tree_fit)
plot(tree_fit)
text(tree_fit,pretty = 0)
tree_hat=predict(tree_fit,newdata = Boston_test)
kk=mean(tree_hat-Boston_test$medv)^2
kk
sqrt(kk)

library(rpart)
rpart_fit=rpart(medv~.,data = Boston_train)
install.packages("rpart.plot")
library(rpart.plot)
summary(rpart_fit)
rpart.plot(rpart_fit,digits=3,type=0,extra=1,fallen.leaves=F,cex=1)
rpart_hat=predict(rpart_fit,newdata = Boston_test)
kk=mean((rpart_hat-Boston_test$medv)^2)
sqrt(kk)

#인공신경망
#정규화된 데이터가 필요한 경우
nomalize=function(x){return((x-min(x))/(max(x)-min(x)))} #정규화
Boston_train_norm=as.data.frame(sapply(Boston_train,nomalize))
Boston_train_norm
Boston_test_norm=as.data.frame(sapply(Boston_test,nomalize))

library(nnet)
nnet_fit<-nnet(medv~.,data=Boston_train_norm,size=5)
nnet_yhat<-predict(nnet_fit,newdata = Boston_train_norm,type = "raw")
kk=mean((nnet_yhat-Boston_test_norm$medv)^2)
sqrt(kk)

install.packages("neuralnet")
library(neuralnet)
neural_fit=neuralnet(medv~crim+zn+indus+chas+nox+rm+age+dis+rad+tax+ptratio
                     +black+lstat,data = Boston_train_norm,hidden = 5)
neural_rlt=compute(neural_fit,Boston_test_norm[1:13])
neural_yhat=neural_rlt$net.result
kk=mean((neural_yhat-Boston_test_norm$medv)^2)
sqrt(kk)
plot(neural_fit)

#앙상블
set.seed(1234)
rnorm(10)

library(randomforest)

#자유학습 알고리즘
#군집분석 >>자동으로 군집을 분리, 판단은 분석자가 한다.

iris2<-iris[,1:4]
head(iris2)
km_out_withness<-c()
km_out_between<-c()
k=c()
for (i in 2:7) {
  set.seed(1)
  km_out=kmeans(iris2,centers = i)
  km_out_withness[i-1]<-km_out$tot.withinss
  km_out_between[i-1]<-km_out$betweenss
k=c(k,i)
  }
data.frame(k,km_out_withness,km_out_between)
plot(k,km_out_withness,type='b')
plot(k,km_out_between,type='b')
#엘보우포인트, 그래프에서
#갑자기 확 꺾이는 지점(급경사), 가장 적합한 군집갯수

km_out_k3<-kmeans(iris2,centers = 3)
km_out_k3$centers
km_out_k3$size
table(km_out_k3$cluster,iris$Species)
plot(iris2[,1:2],col=km_out_k3$cluster,pch=ifelse(km_out_k3$cluster==1,16,
                                                  ifelse(km_out_k3$cluster==2,17,18)),
     cex=2)
points(km_out_k3$centers,col=1:3,pch=16:18,cex=5)

#차원축소
pc1=princomp(USArrests,cor=T)
summary(pc1)
plot(pc1)
biplot(pc1,cex=1)

#인연관계분석
install.packages("arules")
library(arules)
install.packages("arulesViz")
library(arulesViz)
data("Groceries")
Groceries
data(package="arules")
inspect(Groceries[1:10])
summary(Groceries)
sort(itemFrequency(Groceries,type="absolute"),decreasing = T)
round(sort(itemFrequency(Groceries,type="relative"),decreasing = T))
itemFrequencyPlot(Groceries,topN=10,type="absolute")
itemFrequencyPlot(Groceries,topN=10,type="relative")

apriori(Groceries)
result_rules<-apriori(Groceries,parameter = list(support=0.005,
                                                 confidence=0.5,minlen=2))
summary(result_rules)
inspect(result_rules[1:5])
rules_lift<-sort(result_rules,by="lift",decreasing = T)
inspect(rules_lift[1:5])
rules_conf<-sort(result_rules,by="confidence",decreasing = T)
inspect(rules_conf[1:5])
rhs.milk_rule<-subset(rules_lift,rhs%in% "whole milk")
rhs.milk_rule
inspect(rhs.milk_rule[1:5])
wholemilk_rule<-apriori(Groceries,parameter = list(support=0.005,
                                                   confidence=0.5,minlen=2),appearance = list(default="lhs", rhs="whole milk"))
wholemilk_rule<-sort(wholemilk_rule,by="lift",decreasing = T)
inspect(wholemilk_rule[1:5])
library(arulesViz)
plot(wholemilk_rule[1:10], method="graph" , measure="lift", shading="confidence")
