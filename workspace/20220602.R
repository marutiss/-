setwd("c:/Rdata")
getwd()
speed<-c(4,7,8,9,10,11,12,13,13,14) 
dist=c(2,4,16,10,18,17,24,34,26,26)
speed
speed[3]
summary(speed)
boxplot(speed)
mean(dist)
sd(dist)
var(dist)
cor(speed,dist)
plot(speed,dist,type = 'l',col=1)

sink("output.txt")
mean(dist)
sink()

pdf("plot1.pdf")
plot(dist,speed)
dev.off()

?plot
??ggplot2
install.packages("ggplot2")
library(ggplot2)

#데이터 양식에 따라 처리법이 다르다.
#벡터 c()로 생성
#문자를 넣을 때 "" 또는 '' 필요
#성분에 문자가 있다면 우선권이 있어서 모든 성분이 문자가 된다
#c(3,"1",TRUE,FALSE)
x=c(1,4,NA,5,NULL,8)
x

#Factor 범주형 데이터
#level 팩터가 가질수있는 값들
gender=c("m","f","m","m","f")
f.gender=factor(gender) #전더를 팩터로 변환
gender
f.gender
gender2=c(1,0,1,1,0)
f.gender2=factor(gender2)
gender2
f.gender2

#매트릭스 2차원 배열/동일유형 데이터로 구성
x=matrix(1:9,nrow = 3,ncol=3) #열먼저
x

x=matrix(1:9,nrow = 3,ncol=3,byrow=TRUE)#행먼저 
x

rnames=c("R1","R2","R3")
cnames=c("c1","c2","c3")
z=matrix(1:9,nrow = 3,dimnames = list(rnames,cnames))
z

#어래이
y=array(1:24,c(4,3,2))
y

x1=c(24,28,31,25)
y1=c("F","M","F","F")
xy=data.frame(x1,y1)
xy
xy=data.frame(age=x1,gender=y1)
xy
xy_add=data.frame(age=32,gender="M")
xy=rbind(xy,xy_add)
xy  
xyz=cbind(xy,income=c(200,3000,4000,2500,5000))
xyz
xyz[[1]]  
xyz[1]
xyz[["gender"]]
xyz["age"]
xyz[c(1,3)]
xyz[,c(1,3)]
xyz[c(1,2,5),]

#리스트
a=c("a","b","c")
b=1:10
c=matrix(1:9,3,3)
L=list(vec=a,b,mat=c,xyz)
L

k=airquality
head(k)
tail(k)
str(k)
dim(k)
length(k)
names(k)

x=scan()
24
35
28 21
X
x
xyz=data.frame()
women=edit(xyz)
women
write.table(women,"women1.txt") #열 번호 있음
write.table(women,"women2.txt",row.names = F) #열번호 제거
write.csv(women,"women.csv",row.names = F)

x=1:3
x
x=c(x,4,5)
x
y=c(6,7,8)
y
x=c(x,y)
x
1.1:2.8
1.5:9.5
seq(0,10,2)
seq(0,10,length=20)
rep(1,5) #반복출력
rep(c(1,2),5)
rep(c(10,20),c(2,3))
x=1:5
y=seq(2,10,2)
length(x)
length(y)
x+y
x
x+10
x
x*c(10,20)

paste("big","data", sep="") #seq로 글자 사이에 넣을것 설정
paste0("big","data") #띄어쓰기 제거


x=1:3
y=5:7
cbind(x,y)
rbind(x,y)
A=cbind(x,y)
B=rbind(x,y)
A
B
rownames(A)=c("r1","r2","r3")
A
colnames(B)=c("c1","c2","c3")
B
A=matrix(1:4,2)
B=matrix(5:8,2)
A
B
A*B #성분 곱곱
A%*%B #행렬 연산

data=read.csv("anorexia.csv")
data
head(data)
str(data)
attach(data)
Prewt
detach(data)

diff2=Postwt-Prewt
diff2
data$diff2=diff2
head(data)

data$diff=data$Postwt-data$Prewt
head(data)
data=transform(data,diff3=Postwt-Prewt,mean=(Postwt+Prewt)/2)
data$group[data$diff>0]="Increase"
data$group[data$diff<0]="Decrease"
data$group[data$diff==0]="No Change"

x=data.frame(age)

#order
rnorm(100,70,1)
x=sample(1:5,5)
x
order(x)
sort(x)
median(x)

install.packages("reshape")
library(reshape)
tips
attach(tips)
table(day)
names(tips)
str(tips)
mean(tip)
median(tip)
sort(tip)[c(122,123)]
(2.88+2.92)/2

tip2=tip
max(tip)
tip2[1]=100
tip2[1]
mean(tip2)
mean(tip)
median(tip2)
quantile(tip)
quantile(tip,seq(0.1,0.1))
boxplot(tip,horizontal = T,col=2)

data=read.csv("movie_MBA2.csv")
head(data)
qqnorm(tip)
qqline(tip,col=2)
shapiro.test(tip) #p값이 0.05보다 커야 정규성이 있있다.

mytable=table(day)
mytable
tt=barplot(mytable,las=2,col=rainbow(4))
text(tt,mytable,label=paste0(mytable,"개"),cex=1.5,col=2)

#파이차트
lbl=paste(names(mytable),",",round(mytable/sum(mytable)*100),"%",sep="")
lbl
pie(mytable,labels = lbl)

mytable2=xtabs(~sex+day,tips)
mytable2
barplot(mytable2,legend.text = c("feamale","male"),beside = T)
