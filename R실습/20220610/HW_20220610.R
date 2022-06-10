setwd("c:/Rdata")
sona=read.csv("sonata.csv")

#a.   회귀식을 추정하시오.
out=lm(Price~Odometer,data=sona)
summary(out)
#x=거리 y가격
#Price= -0.066861*Odometer+17.248727

#b. Odometer가3600마일인 소나타의 평균가격을95% 신뢰구간으로 추정하시오.
new=data.frame( Odometer=c(3600))
predict(out,new,interval="confidence")

#c. 여진의 소나타는 현재5000마일의Odometer를 기록하고 있다. 중고차 경매시장에 
#나갔을 때 받을 수 있는 가격을95% 예측구간으로 추정하시오.
new=data.frame( Odometer=c(5000))
predict(out,new,interval="prediction")
