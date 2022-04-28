# 1.'noh.txt'파일을 t/m으로 진행하여 일단 상위 단어개수인
# 20개를 추출하여 시각화 하시오?
txt_HW3=readLines("noh.txt")
spch=sapply(txt_HW3,extractNoun,USE.NAMES = F)
spch=unlist(spch)
class(spch)
spch=Filter(function(x){nchar(x)>=2},spch)
write(spch,"noh2.txt")
outp=read.table("noh2.txt")
wc=table(outp)
hh=head(sort(wc,decreasing = T),20)
pp=barplot(hh,ylim = c(0,30),las=2,col=rainbow(20))
text(pp,hh,paste0(hh,"개"),pos=3,cex=2,col=2)


wordcloud(
  names(wc),
  freq = wc,
  scale = c(5,0.5),
  rot.per = 0.25,
  min.freq = 2,
  random.order=F,
  random.color=T,
  colors=palate)

wordcloud2(
  data=wc,
  size=0.4,
  shape = "diamond" #circle, triangle...
  
)

#   2.'hong.txt'파일을 t/m으로 진행하여 일단 상위 단어개수인
# 20개를 추출하여 시각화 하시오?
xtx_HW3=readLines("hong.txt")
spch1=sapply(xtx_HW3,extractNoun,USE.NAMES = F)
spch1=unlist(spch1)
class(spch1)
spch=Filter(function(x){nchar(x)>=2},spch)
write(spch1,"hong2.txt")
outp1=read.table("hong2.txt")
wc1=table(outp1)
hh1=head(sort(wc1,decreasing = T),20)
pp1=barplot(hh1,ylim = c(0,40),las=2,col=rainbow(20))
text(pp1,hh1,paste0(hh1,"개"),pos=3,cex=2,col=2)

wordcloud(
  names(wc1),
  freq = wc1,
  scale = c(5,0.5),
  rot.per = 0.25,
  min.freq = 2,
  random.order=F,
  random.color=T,
  colors=palate)

wordcloud2(
  data=wc1,
  size=0.4,
  shape = "diamond" #circle, triangle...
  
)
