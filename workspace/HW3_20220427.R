#HW2
#Q1. ggplot2의 패키지의 데이터를 사용할수있도록 불러온뒤
#데이터프레임으로 초기화하고 mpg_tst로 복사본을 만글기
mpg = as.data.frame(ggplot2::mpg) 
mpg_tst=mpg


#Q2. 복사본데이터에서 cty는 city, hwy는 highway로 이름변경
#그리고 boxplot()으로 시각화하여 비교제시
mpg_tst=rename(mpg_tst,city=cty)
mpg_tst=rename(mpg_tst,highway=hwy)
head(mpg_tst)

boxplot(mpg_tst$city,mpg_tst$highway,col=rainbow(2))


#Q3. 도시연비(cty), 고속도로 연비(hwy)를 히스토그램으로 시각화하고 
#각각의 확률밀도를 또한 시각화하여 나타내시오
par(mfrow=c(1,2))
hist(mpg_tst$city,ylim=c(0,100))
hist(mpg_tst$city,ylim=c(0,0.1),probability = T)
lines(density(mpg_tst$city),col=2,type='h',lwd=2)
hist(mpg_tst$highway)
hist(mpg_tst$highway,probability = T)
lines(density(mpg_tst$highway),col=3,type='h',lwd=2)
