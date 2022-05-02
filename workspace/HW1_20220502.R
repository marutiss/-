install.packages("animation")
install.packages("png")
install.packages("jpeg")

library(animation)
library(png)
library(jpeg)
frame()
ani.options(interval=1.5)
while(TRUE){
  
  rect(0,0,1,1,col = "white")
  for(i in 1:6) {
    img=paste0("C://Rdata//img",i,".jpeg")
  img=readJPEG(img)
  rasterImage(img,0,0,1,1)
  ani.pause()
  }
}


#서울지하철위경도 정보에 대한 위피파악으로 지도시각화로 나타내시오
loc=read.csv("서울지하철2호선위경도정보.csv",header=T)
subw2=get_map("seoul",zoom = 11,maptype = "roadmap")
ggmap(subw2)
subw2_map=ggmap(subw2)+
  geom_path(data = loc,aes(x=LON,y=LAT),lwd=2,col="green")+
  geom_point(data = loc,aes(x=LON,y=LAT),size=2,alpha=1,col="red")+
  geom_text(data = loc,aes(x=LON,y=LAT+0.0002,label=loc$역명),size=3)
ggplotly(subw2_map)


loc=read.csv("서울지하철3호선역위경도정보.csv",header=T)
subw3=get_map("seoul",zoom = 11,maptype = "satellite")
ggmap(subw3)
subw3_map=ggmap(subw3)+
  geom_path(data = loc,aes(x=LON,y=LAT),lwd=2,col="orange")+
  geom_point(data = loc,aes(x=LON,y=LAT),size=2,alpha=1,col="red")+
  geom_text(data = loc,aes(x=LON,y=LAT+0.0002,label=loc$역명),size=3)
ggplotly(subw3_map)
setwd("C:/Rdata")
