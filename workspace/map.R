library(ggmap)
library(ggplot2)
names=c("군자역","동대문역사문화공원","공덕역","양등포구청역","화곡역")
loc=geocode(location=names,
            output="latlon",
            source="google")

df=data.frame(name=names,lon=loc$lon,lat=loc$lat)
df
cen=c(mean(df$lon),mean(df$lat))
cen
map=get_googlemap(center=cen,zoom = 11,maptype = "roadmap")
kk=ggmap(map)+
  geom_path(data = df,aes(x=lon,y=lat),lwd=2)+
  geom_point(data = df,aes(x=lon,y=lat),col=2,size=5)+
  geom_text(data = df,aes(x=lon,y=lat+0.002),
            label=df$name,size=3,alpha=1,col=2)

ggplotly(kk)
