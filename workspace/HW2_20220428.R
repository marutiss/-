# 아래는 자동차 연료별 가격을 나타낸 표입니다.
# fl 연료 종류 가격(갤런당 USD)
# c CNG 2.35
# d diesel 2.38
# e ethanol E85 2.11
# p premium 2.76
# r regular 2.22
# 우선 이 정보를 이용해서 연료와 가격으로 구성된 데이터 프레임을 
# 만들어 보세요.
mpg=data.frame(ggplot2::mpg)
fuel=data.frame(fl=c("c","d","e","p","r"),
                price_fl=c(2.35,2.38,2.11,2.76,2.22),stringsAsFactors = F)
fuel


# • Q1. mpg 데이터에는 연료 종류를 나타낸 fl 변수는 있지만 
# 연료 가격을 나타낸 변수는 없습니다. 위에서 만든 fuel 데이터를 이용해서 
# mpg 데이터에 price_fl(연료 가격) 변수를 추가하세요.
mpg=left_join(mpg,fuel,by="fl")

# • Q2. 연료 가격 변수가 잘 추가됐는지 확인하기 위해서 model, fl, price_fl 
# 변수를 추출해 앞부분 5 행을 출력해 보세요.
mpg %>% select(model,fl, price_fl) %>% head(5)

# • Q3. 제조사별, 연료별로 사용 가격 평균을 가장 큰순서대로 상위 10개, 하위 10개를
# 나타내시오?

qq=data.frame(mpg %>% group_by(manufacturer,fl) %>% 
  summarise(mean_pri=mean(price_fl),count=n())) %>% 
  arrange(desc(mean_pri)) %>% head(10)
qq

pp=data.frame(mpg %>% group_by(manufacturer,fl) %>% 
                summarise(mean_pri=mean(price_fl),count=n())) %>% 
  arrange(desc(mean_pri)) %>% tail(10)
pp

