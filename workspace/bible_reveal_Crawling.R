#Crawling for Bible
getwd()
library(rvest)
library(stringr)
library(dplyr)
library(ggplot2)
library(wordcloud)
library(wordcloud2)
library(KoNLP)

cnt=c()
url="https://www.bskorea.or.kr/bible/korbibReadpage.php?version=GAE&book=rev&chap="

for (i in 1:22) {
  base_url=paste0(url,i)
  t_css="#tdBible1 span"
  hdoc=read_html(base_url,encoding = "UTF-8")
  n_css=html_nodes(hdoc,t_css)
  cnt_part=html_text(n_css)
  cnt_part=gsub("\\d+","",cnt_part)
  cnt_part#확인용출력
  cnt_part=str_trim(cnt_part,side = "both")
  cnt=c(cnt,cnt_part)
  
}
txt=sapply(cnt, extractNoun,USE.NAMES = F)
txt=unlist(txt)#백터화 >>분석가능
class(txt)

count=Filter(function(x){nchar(x)>=2},txt)
word=table(count)
tt=head(sort(word,decreasing = T),20)
kk=barplot(tt,ylim=c(0,70),col=rainbow(20))
text(kk,tt,paste0(tt,"개"),pos=3,las=2,col=2,cex=1)
plot.new()

library(RColorBrewer)
display.brewer.all()
palate=brewer.pal(12,"Set3")
wordcloud(names(word),
          freq = word,
          min.freq = 2,
          scale=c(5,0.2),
          random.order = F,
          random.color = T,
          colors = palate
)

wordcloud2(data=word,
           size=0.4,
           shape="cloud")
