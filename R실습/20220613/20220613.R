#다중회귀모형
#예시 y=연봉, x1=경력,x2=점수
#y=ax1+bx2+c+(엡실론(오차항))
setwd("c:/Rdata")
data=read.csv("salary.csv")
head(data)
model=lm(salary~.,data = data)
summary(model)
#t벨류의pr값 0.05이상? salary와 아무런 상관이 없다. 작을수록 매우 상관있다.
#r스퀘어=타당도>>예시에서 경력연수와 직무적성검사 성적이 연봉의 변동량의 83%를 설명   adjust R스퀘어= 멀티보다 항상작음
#계수의 해석방법주의

#SST(총제곱합)=SSR(회귀제곱합)+SSE(오차제곱합)

#F검정: 종속변수와 모든 독립변수 집합 간에 유의한 관계가 존재하는지를 검정
#가설 H0: 𝛽1 = 𝛽2 = ⋯ = 𝛽𝑝 = 0


par(mfrow=c(2,2))
plot(model)
cbind(data$experience,data$score,data$salary,model$fitted.values)
#>>추정식에 경험+점수 대입하면 model$fitted.values인 y햇 획득, 실제값 data$salary
#


#다중공선성
#독립변수들간 상관 관계>>높다면 각각의 변수x가 y에 개별적으로 미치는 영향을 파악하기 힘듦
attitude
pairs(attitude[,c("rating","complaints","learning")])
cor(attitude[,c("rating","complaints","learning")])
#>>결과를 보면 다중공선성이 강하다.
summary(lm(rating~complaints+learning,data = attitude))
#>>레이팅과 러닝의 t값이 0.05이상 >>독립적이다. >>추정식에 넣으면 안된다
#>진짜 그럴까???>> 둘만 따로 돌려보자
summary(lm(rating~learning,data = attitude))
#>0.05이하>> 엄청 연관있다>> 추정식에필요
#>왜 값이 다른가? >> 독립변수간 관계 때문에 처음 값이 왜곡됨
#>이를 고려하여 추정식을 만들기 위한 방법

#어떤 방법(모형)을 선택해야하는가?
#모형선택 : 1. 이론근거 2. 여러모형을 고려, 적절한 선택

#백워드 모형: 모든 변수를 넣고 기여도 낮은 t값이 높은(0.05이상)부터 제거
out=lm(rating~.,data = attitude)
summary(out)
#크리티컬 제거후 다시 돌려려
out2=lm(rating~.-critical,data=attitude)
summary(out2)
backward=step(out,direction="backward",trace=FALSE)
summary(backward)

both=step(out,direction="both",trace=FALSE)
summary(both)
both$anova

#allsubset 
install.packages("leaps")
library(leaps)
leaps=regsubsets(rating~.,data=attitude,nbest = 5)
summary(leaps)
plot(leaps)
plot(leaps,scale = "Cp")
#>cp로 돌리면 complains learning이 최소값>>베스트 모델
#>complains learning만 추정식에 대입입
plot(leaps,scale = "adjr2")
#complains learning advance만 추청식에 대입입
out3=lm(rating~complaints+learning+advance,data=attitude)
summary(out3)



#로지스틱회귀분석 : 그 사건이 발생할 확률을 예측한다. 일반적으로 종속변수의 범주가 두 개인 경우
data=read.csv("programming.csv")
head(data)
#종속변수 y는 0,1의 값을 갖는다.
model=glm(Success~Experience,family=binomial(logit),data=data)
summary(model)

plot(Success~Experience,data = data)
points(model$fitted~data$Experience,col=2)
#exp결과에서 1.--- 나오면 증가 0.---나오면 감소

coupon=read.csv("coupon.csv")
head(coupon)
model2=glm(cbind(N_redeemed,N-N_redeemed)~Price_reduc,family = binomial,data=coupon)
summary(model2)

data=read.csv("disease.csv")
model3=glm(disease~.,family=binomial,data=data)
summary(model3)

#wald test : 다중회귀분석의 t테스트
#glm돌리세요 z값으로 판단 합니다~
model4=glm(disease~age+sector,family=binomial,data=data)

#모형비교 : Reduced Model과 Full Model의 차이가 유의한지 검정
anova(model3,model4,test = "Chisq")
#두 모델이 서로 독립적인가?
#독립변수를 하나 제거해도 타당한 모델인가?
#만약 빼도 타당성의 문제가 없다면 빼는게 맞다.
#타당성 여부 >>p값으로 구분
#0.05보다 크다? 타당하다. 독립적이다.

#>>0.05보다 낮으면 상관관계가 크다
#>상관관계가 독립적이면 좋은 모델이다>> 상관관계가 크면 나쁘다.
#>0.05보다 크면 둘 다 유익하다 >>둘중하나 아무거나
model4$fitted.values
table(data$disease)
31/98
#0.3163265 미만은 1
model4$fitted>0.316
xtabs(~data$disease+(model4$fitted>0.316))


install.packages("Deducer")
library(Deducer)
rocplot(model4)
#AUC : 에이리어 언더커버, 커브밑의 면적 클수록 이상적?
rocplot(model3)



#다변량 자료의 검색
#스타 차트로 분석하기
crime = read.csv("http://datasets.flowingdata.com/crimeRatesByState-formatted.csv")
head(crime)
rownames(crime)
crime[,1]
rownames(crime)=crime[,1]
rownames(crime)
stars(crime[,2:8])
stars(crime[,2:8],flip.labels = FALSE)
stars(crime[,2:8],flip.labels = FALSE,key.loc = c(15,1))

#나이팅궤일차트
stars(crime[,2:8],flip.labels = FALSE,key.loc = c(15,1),draw.segments = TRUE)

#체르노프페이스
install.packages("aplpack")
library(aplpack)
faces(crime[,2:8])

#평행좌표플롯
education = read.csv("http://datasets.flowingdata.com/education.csv")
str(education)

#책 많이 읽은사람 표시
library(lattice)
parallel(education[,2:7],horizontal.axis=FALSE,col=1)
summary(education$reading)
color=education$reading>523
color+1
parallel(education[,2:7],horizontal.axis=FALSE,col=color+1)

#자퇴율 상위 25%만 색상변경
summary(education$dropout_rate)
color=education$dropout_rate>5.3
color+1
parallel(education[,2:7],horizontal.axis=FALSE,col=color+1)

#주성분 분석
#목적: 차원 축약으로 변수를 제거
data = read.csv("20140528_baseball.csv")
head(data)
model=prcomp(data[,2:6],scale=T)
model
summary(model)
plot(model)
biplot(model)
rownames(data)=data[,1]
rownames(data)
