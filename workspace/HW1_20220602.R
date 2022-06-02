# price_201309.csv자료 활용
# 1. str()을 사용해 자료의 구조를 출력하시오.
# 2. summary()를 사용해 변수의 요약통계량을 출력하시오.
# 3. psych package의 describeBy()를 사용해 각 구별 생필품가격 (A_PRICE)의 요약통계량을 비교하시오.
# 4. aggregate()을 활용해 각 구와 시장형태에 따라 A_PRICE의 평균을 출력하고price라는 이름의 개체에 저장하시오. 
# 5. 위에서 저장한 price를 가격에 대해 오름차순으로 정렬하여 생필품 가격이 가장 저렴한 시장형태*구 조합과 가장 비싼 시장형태*구 조합을 찾으시오.

setwd("c:/Rdata")
getwd()

market=read.csv("price_201309.csv")
str(market)
summary(market)

install.packages("psych")
library(psych)
#describeBy(통계낼 자료, 그룹할 기준)
describeBy(market$A_PRICE,group = market$M_GU_NAME)

price=aggregate(market$A_PRICE,by=list(market$M_GU_NAME,market$M_TYPE_NAME),mean)

price[order(price$x),]
price[order(price$x,decreasing=T),]

