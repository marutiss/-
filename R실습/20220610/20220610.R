#평균에 대한 비교
#질적자료가 관심의 대상>>비율에 대한 검정

#모집단비율에 대한 양측검정
#교통사고 500명 사망,25000명이 부상. 사건의 50퍼는 음주운전 문재
#120의 표본,67건이 음주은전, 유의수준 0.05에서 비율검증
#binom.test() 자료수가 적을때 사용
#prop.test()자료수가 지나치게 많을 경우
#가설:H0 p=p0 >>모집단비율=표본비율
#H1: p와 p0가 다름
#귀무가설 하에 binom을 구해서 표본비율이 귀무가설하에 얼마나 나올값인가를
#p값을 통해 판단.
setwd("C:/Rdata")

x=0:120
y=dbinom(x,120,0.5)
plot(x,y,type = 'h')

binom.test(67,120)#0.05보다 p값이 커서 귀무가설 성립
prop.test(67,120)#p값 0.05이상, 귀무가설 성립

prop.test(c(60,120),c(150,250)) #0.05이상,귀무가설>>인지에 대한 차이가 생김

#적합성검정 뭔테스트?
#관찰값=기대값 이 성립하는지 확인하는 검정
#질적자료에서 카테고리에 속하는 사료비율이 기대와 일치하는가

#a회사는 4가지 모델의 주택이 있다.
#2년간 100채 팔림 1:30% 2:20% 3:35% 4:15% 팔림
#귀무가설: 전부 25%
#대립가설: 다 다름
x=c(30,20,35,15)
chisq.test(x,p=c(0.25,0.25,0.25,0.25)) #귀무가설 기각>>기대값과 차이가 있다.


 #독립성 검정>>두범주형 자료가 연관이 없는 지 확인
#주택의 가격과 스타일이 독립적인 변수인가?
#$99000이하 18 6 19 12
#$99000이상 12 14 16 3  
#귀무가설 : 주택 스타일과 가격은 독립적
#대립가설: 수택스타일과 가격은 독립적이 아님
chisq.test(matrix(c(18,12,6,14,19,16,12,3),ncol=4))
#0.05 보다 작다>> 독립적이지 않다다

matrix(c(48,20,16,40),ncol = 2)

respire=read.csv("respire.csv")
respire
xtabs(count~treat+outcome,data=respire)
respire2=read.csv("respire2.csv")
respire2
tab=xtabs(~treat+outcome,data=respire2)

mosaicplot(tab)
?Titanic
mosaicplot(Titanic,main="Survival on the Titanic")

#상관분석>>4분면 함수
# 상관계수가 +-1에 가까울수록 강력한 상관관계
x=1:10
y=x^2
plot(x,y)
cor(x,y)
cor(x,sqrt(y))
cor(x,y,method = "kendall")
cor(x,y,method = "spearman")


attitude
cov(attitude) #공분산
cor(attitude)

with(attitude,cor.test(rating,complaints))
#p값이 매우 작음>> 강한 상관관계가 있다
cor.test(attitude$rating,attitude$complaints)

#회귀분석
#원인변수(설명변수)에 때른 종속변수의 결과예측
#반순선형회귀식 >1차방정식
#추정식과 실제 값의 차이는 오차항이라 한다

cars=read.csv("cars.csv")

out=lm(dist~speed,data=cars)
summary(out)
#coefficient =절편, p값이 0에 근접할수록 큰 상관관계
#멀티r스퀘어(결정계수) =모델이 얼마나 최적으로 만들어(추정,타당) 졌는가,
#1에 가까울수록 잘만듦

plot(dist~speed,data=cars,col="blue")
abline(out,col="red")

#속도가 0이면 거리도 0인게 당연
out2=lm(dist~speed+0,data=cars)
summary(out2)

#회귀진단: 오차항(잔차) 
par(mfrow=c(2,2)) #4분할
plot(out2)
par(mfrow=c(1,1))
plot(cars$speed,log(cars$dist))
shapiro.test(sqrt(cars$dist))
out3=lm(sqrt(dist)~speed+0,data = cars)
summary(out3)
plot(out3)


new=data.frame(speed=c(10,30))
predict(out3,new)
new
cbind(cars$speed,predict(out3,new))

#자동차 멈추는데 걸리는 거리의 95%예측구간
cbind(cars$speed,predict(out3,new),interval = "confidence")

predict(out3,new,interval = "confidence")
predict(out3,new,interval="prediction")
