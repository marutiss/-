library(ggplot2)
library(plotly)
library(readxl)
library(rvest)
library(stringr)
library(RColorBrewer)
library(wordcloud)
library(wordcloud2)
library(KoNLP)
#animation pkg
install.packages("animation")
library(animation)
ani.options(interval=1) #1초의 타이머
while (TRUE) {
  x=rnorm(100)
  hist(x,probability = TRUE)
  lines(density(x),type='h',col=2,lwd=2)
  ani.pause()
}

while(TRUE){
  y=runif(5,0,1) #0~1사이의 데이터 5개 추출
  barplot(y,ylim=c(0,1),col=rainbow(5))
}
setwd("C:/Rdata")
#이미지파일을 전시하는 효과

install.packages("png")
install.packages("jpeg")
library(png)
library(jpeg)
ani.options(interval=2)
while(TRUE){
  plot.new()
  rect(0,0,1,1,col = "white")
  for(i in 1:3)
    img=paste0("C://Rdata//img",i,".jpeg")
    imp=readJPEG(img)
    rasterImage(img,0,0,1,1)
    ani.pause()
}



#지도 시각화
install.packages("ggmap")
library(ggmap)
register_google(key='AIzaSyD3K88he9E4RMO4AQTGt4l82IdJzPV0U2o')
gc=geocode("los angeles,ca,usa") #구글맵에서 검색
gc
cen=as.numeric(gc) #경도와 위도를 숫자로 재정리
cen
map=get_googlemap(center=cen)#위도와 경도를 중심으로 지도정보 반환
ggmap(map)#지도상의 위치를 표현


getmap=get_googlemap("seoul")
ggmap(getmap)

#서울시구청 위치정보(위도/경도)
getmap=get_googlemap("seoul") #서울시 정보 받기
loc=read.csv("서울시구청위치정보.csv",header=T)
kor=get_map("seoul",zoom = 11,maptype = "hybrid") #세계지도 1, 시,구=10,11
ggmap(kor) #지도상의 위치표현
plot.new()
kor.map=ggmap(kor)+geom_point(data=loc,aes(x=LON,y=LAT),size=3,alpha=1,col="red")
aa=kor.map+geom_text(data=loc,aes(x=LON,y=LAT+0.005,label=loc$구청명,col="red"),size=0.3)
library(plotly)
ggplotly(aa) #커서와 상호작용

#서울시 장난감 위치정보 지도시각화
loc=read.csv("서울시장난감도서관위치현황.csv",header=T)
doll=get_map("seoul",zoom = 11,maptype = "roadmap")
ggmap(doll)
doll_map=ggmap(doll)+geom_point(data = loc,aes(x=LON,y=LAT),size=2,alpha=1)+
  geom_text(data = loc,aes(x=LON,y=LAT+0.0002,label="이름"),size=2)
ggplotly(doll_map)

#단양 8경을 지도위에 표시
library(ggmap)
library(ggplot2)
names=c("1.도담삼봉/석문","2.구담/옥순봉","3.사인암","4.하선암","5.중선암",
        "6.상선암")
names
addr=c("충청북도 단양군 매포읍 삼봉로 644-33",
       "충청북도 단양군 단성면 월악로 3827",
       "충청북도 단양군 대강면 사인암2길 42",
       "충청북도 단양군 단성면 선암계곡로 1337",
       "충청북도 단양군 단성면 선암계곡로 868-2",
       "충청북도 단양군 단성면 선암계곡로 790")
gc=geocode(addr)
gc
df=data.frame(name=names,lon=gc$lon,lat=gc$lat)
df
cen=c(mean(df$lon),mean(df$lat))
cen
map=get_googlemap(center=cen,zoom = 11,maptype = "roadmap",marker=gc)
ggmap(map)


#단양8경 이름출력
gmap=ggmap(map)
gmap=geom_text(data = df,
               aes(x=lon,y=lat),
               size=5,#출력 문자열크기
               label=df$name)

#set.seed()
set.seed(1234)
runif(5,0,1)
set.seed(4621) #암호?
rnorm(10)
