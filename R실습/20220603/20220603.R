
dbinom(5,22,0.23)
x=rbinom(500,1,0.5)
mean(x)
dbinom(0:2,2,0.5)
dbinom(0:2,22,0.5)
plot(0:3,dbinom(0.3,3,0.1),type = 'h',col=2,lwd=5)

pbinom(1.2,0.5)
#이항분포의 난수추출

#연속확률분포
#연속된 어떤 구간이나 구간들의 집합
x=rnorm(10000,0,1)
y=rnorm(10000,0,2)
par(mfcol=c(1,2))
hist(x)
hist(y)

pnorm(20,15,6)
pnorm(20,15,6,lower.tail = F)
qnorm(0.95,15,6)

#실습예제
n=2131
p=0.362
1.96*sqrt(p*(1-0.362)/2131)
1.96*sqrt(0.5*(1-0.5)/2131)

#----

x=rnorm(100)
y=sample(1:100,80,replace = T) #1-100 80개
shapiro.test(x)
par(mfrow=c(1,2))
hist(x)
hist(y)

x=c(15.5,11.21,12.67,8.87,12.15,9.88,2.06,14.5,0,4.97)
mean(x)
shapiro.test(x)
#andero-daring
install.packages("nortest")
library(nortest)
ad.test(x)
boxplot(x)
t.test(x,u=8.1) #평균검정
t.test(x,u=8.1,alternative = "greater")

#compair t-test
FT=read.csv("FT.csv")
FT
FT$diff=FT$Prewt-FT$Postwt
ad.test(FT$diff)
shapiro.test(FT$diff)


dental=read.csv("dental.csv")
dental
frame()
par(mfcol=c(1,2))
boxplot(resp~treatment, data=dental,col="red",ylab="resp")
#위 그래프를 로그로 표현현
boxplot(log(resp)~treatment, data=dental,col="blue",ylab="log(resp)")
#정규성검사
shapiro.test(log(dental$resp)) #로그로 해야 정규성 확보
var.test(log(dental$resp)~treatment,data=dental)

t.test(log(dental$resp)~treatment,data=dental,var.equal=TRUE)
#p값 0.05이하 귀무가설 기각


#powertransform
install.packages("car")
library(car)
x=dental$resp
shapiro.test(x)
k=powerTransform(x)
k
t=x^-0.7949786
shapiro.test(t)
t

?t.test
