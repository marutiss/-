a=10
b=20
a+b
x=rnorm(100)
x
hist(x)

hist(x,probability = T)
lines(density(x),col=2,type = 'h',lwd=2)

a=3.14
c=1.5

a+b+c
4/b

var1=c(1,2,3,5,7,8,9)
var1[3]
var2=c(1:5)
var2
1:6
var3=seq(1,5)
var3
var4=seq(1,10,by=2)
var4

str1="a"
str1
str2="tect"
str2
str3="hellowo"
str3
str4=c("a","b","c")
str4
str5=c("owo","uwu")
str5


#함수에대한 이야기
x=c(1,2,3)
x
mean(x)
sum(x)
max(x)
min(x)
sd(x) #표준편차 분산의 내용을 본다
var

#문자를 다루는 함수
str5

paste(str5,collapse = ",") #쉼표를 구분자로 단어들 하나로 합치기 
paste(str5,collapse = "") #아무것도 안쓰면 붙어서 나옴

x
x_mean=mean(x)
x_mean

#패키지사용방법
#ggplot2 패키지-시각화를 위한 전용패카지 >>그래프특화 패키지
installed.packages("ggplot2")
library(ggplot2)
x=c("a","a","b","c")
x
qplot(x)
data(package="ggplot2")
head(mpg)
colnames(mpg)
qplot(data=mpg,x=cty)
class(mpg$drv)
mpg$drv
qplot(data = mpg,x=drv,y=cty)
qplot(data = mpg,x=drv,y=hwy,geom="boxplot")

kk=c(3,5,2,1,9)
kk
median(kk) #중간값
sort(kk) #오름차순정렬
boxplot(kk)
boxplot(kk,horizentall=TRUE,col=2)
quantile(kk)


hw=c(80, 60, 70, 50, 90,88,63,77,82,93)
hw

sd(hw)
var(hw)
boxplot(hw)
quantile(hw)



english=c(90,80,60,70)
english
math=c(50,60,100,20)
math

df_midterm=data.frame(english,math)
df_midterm

class=c(1,1,2,2)
class

df_midterm=data.frame(english,math,class)
df_midterm
mean(df_midterm$english)
mean(df_midterm$math)
df_midterm=data.frame(english=c(92,76,100,85),
                      math=c(99,56,78,67),
                      class=c(1,1,2,2))
df_midterm

pdt=data.frame(제품=c("사과","딸기","수박"),
                 가격=c(1800,1500,3000),
                 판매량=c(24,38,13))
pdt
mean_fruit=mean(pdt$가격)
mean_sale=mean(pdt$판매량)
Q2_ans=c(mean_fruit,mean_sale)
Q2_ans

setwd("c:/Rdata")
getwd()


#외부데이터 이용방법
#엑셀 불러오기

install.packages("readxl")
library(readxl)

df_exam=read_excel("excel_exam.xlsx")
df_exam
nrow(df_exam)
colnames(df_exam)
length(df_exam)
head(df_exam)

avg_subject=c(영어평균=mean(df_exam$english),
수학평균=mean(df_exam$math),
영어평균=mean(df_exam$science))
avg_subject
getwd()
df_exam_novar=read_excel("excel_exam_novar.xlsx",col_names = F)
# >> 필드명이 없다면 새로 부여
df_exam_novar

#시트가 여러개일때
df_exam_sheet=data.frame(read_excel("excel_exam_sheet.xlsx",sheet=3))
df_exam_sheet

#csv 파일 불러오기는 우리가 패키지를 설치할 필요가 없음
df_csv_exam=data.frame(read.csv("csv_exam.csv"))
nrow(df_csv_exam)
length(df_csv_exam)
head(df_csv_exam)
tail(df_csv_exam)
summary(df_csv_exam)

k2=c(4,5,10,7,8,1)
sort(k2)
median(k2)
mean(k2)

class(df_csv_exam$science)

df_csv_exam <- read.csv("csv_exam.csv", stringsAsFactors = F)
df_csv_exam

class(df_midterm)
df_midterm
getwd()
write.csv(df_midterm,"df_mod.csv")
save(df_midterm,file = "df_mid.rda")

#삭제
rm(df_midterm)
df_midterm
#복구
load("df_mid.rda")
df_midterm

View(df_csv_exam)
dim(df_csv_exam)
str(df_csv_exam)
summary(df_csv_exam)

exam=data.frame(read.csv("csv_exam.csv"))
exam
head(exam,3)
tail(exam,2)
summary(exam)
View(exam)
dim(exam)
colnames(exam)
nrow(exam)
str(exam)


data(package="ggplot2")
mpg=as.data.frame(ggplot2::mpg)
head(mpg)
dim(mpg)
tail(mpg,2)
str(mpg)
summary(mpg)
View(mpg)

# 데이터의 속성을 표시하는 명령어
class(exam)


#dplyr-전처리용 패키지
install.packages("dplyr")
library("dplyr") #require(dplyr)
df_raw=data.frame(var1 = c(1, 2, 1),
                  var2 = c(2, 3, 2))

df_raw
df_new=df_raw
df_new
df_new=rename(df_new,v2=var2)
df_new

#par()-그래프를 그릴때 화면을 배분해주는 기능함수
par(mfrow=c(1,2))
x=rnorm(100)
hist(x)
hist(x,probability = T)
lines(density(x),col=2,type='h',lwd=2)
plot.new()#frame()
par(mfrow=c(1,1))
hist(x)
frame()




#파생변수만들기-기존의 내용을 이용해서 새로운변수
df=data.frame(var1=c(4,3,8),
              var2=c(2,6,1))
df
df$var_sum=df$var1+df$var2
df$var_mean=df$var_sum/2
df

#mpg의 total연비를 생성시켜라
mpg=data.frame(ggplot2::mpg)
mpg$total=(mpg$cty+mpg$hwy)/2
hist(mpg$total,probability=T)
lines(density(mpg$total),col=2,type = 'h',lwd=2)
summary(mpg$total)

mpg$test=ifelse(mpg$total>=20,"pass","fail")
yy=table(mpg$test)
tt=barplot(yy,ylim =c(0,140),col=rainbow(2))
text(tt,yy,paste0(yy,"개"),pos = 3,cex = 2,col = 2)


summary(mpg$total)
mpg$grade=ifelse(mpg$total>=35,"S",
                   ifelse(mpg$total>=30,"A",
                          ifelse(mpg$total>=25,"B",
                                 ifelse(mpg$total>=20,"C","D"))))
pp=table(mpg$grade)
uu=barplot(pp,ylim=c(0,130),col=rainbow(5))
text(uu,pp,paste0(pp,"개"),pos = 3,col = 2)

