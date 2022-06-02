# HW2
# 기본파일로로 올려져 있는 movie_MBA2.csv를 이용해 아래의 질문에 답하시오.
# 1.   함수 aggregate을 사용해 각 장르(genre)별 평균 매출액과 평균 관객수를 구하시오. 
# 2.   아래의 각 변수가 질적변수인지 양적변수인지 구분하시오 summary해서 나오면 양적, 안나오면 질적적
# "week1_sales"     양적
# "week1_seen”    양적
# "nation"    질적
# "production"    질적 
# "distributor"     질적
# "rating"       질적
# "genre"           질적
# "total_seen"     양작
# "total_sales”   양적
# 
# 3.   총관객수 (total_seen)에 대해 평균, 중위수, 사분위수를 구하고 분포의 모양을 유추하시오. (오른쪽 혹은 왼쪽 꼬리가 긴가?)
# 4.   총관객수에 대해 분산, 표준편차, 사분위수범위를 구하시오.
# 5.   총관객수에 대해 box plot과 히스토그램을 그리고 위의 3번에서 유추한 분포의 모양을 확인하시오.
# 6.   영화등급 (rating)을 사용해 도수분포표를 구하고 bar plot과 pie chart를 그리시오.
# 7.   장르와 영화등급을 사용해 분할표를 그리고 그 결과를 사용해 bar plot을 그리시오.
# 

data=read.csv("movie_MBA2.csv")
aggregate(data[,10:11],by=list(data$genre),mean)
mean(data$total_seen)
median(data$total_seen)
quantile(data$total_seen)

var(data$total_seen)
sd(data$total_seen)
IQR(data$total_seen)

barplot(data$total_seen,horizental=T,xlab = "총관객수수")
hist(data$total_seen,probability = T)
lines(density(data$total_seen),col=2)

rate=table(data$rating)
barplot(rate,col=2)

lbl=paste(names(rate),",",round(rate/sum(rate)*100),"%",sep="")
lbl
pie(rate,labels = lbl)

con_table=xtabs(~data$genre+data$rating,data)
barplot(con_table,legend.text = c("장르","등급"),beside = T)
