#Crawling for Bible
getwd()
library(rvest)
library(stringr)
library(dplyr)
library(ggplot2)
library(wordcloud)
library(wordcloud2)
library(KoNLP)

cnt=c() #특정값을 저장할 주머니
url="https://www.bskorea.or.kr/bible/korbibReadpage.php?version=GAE&book=gen&chap="
  
for (i in 1:50) {
    base_url=paste0(url,i) #페이지의 변환을 위해서사용
    t_css="#tdBible1 span" #끌어올 문장, 사용도구:SelectorGadget
    
    hdoc=read_html(base_url,encoding = "UTF-8") 
    #url의 html소스코드를 인코딩해서 가져온다
    #키보드 한글 입력을 위한 인코딩, UTF-8로 인코딩해서 가져온다
    
    n_css=html_nodes(hdoc,t_css)
    #url의 소스코드와 끌어올 문장의 소스코드 및 속성을 추출
    
    cnt_part=html_text(n_css) #컨텐츠파트 텍스트만 추출
    cnt_part=gsub("\\d+","",cnt_part) #숫자를 blank처리, gsub함수
    cnt_part#확인용출력
    cnt_part=str_trim(cnt_part,side = "both") #끌어온 문장의 앞뒤의 공백제거
    cnt=c(cnt,cnt_part)
    
}
txt=sapply(cnt, extractNoun,USE.NAMES = F)
txt=unlist(txt)#백터화 >>분석가능
class(txt)

count=Filter(function(x){nchar(x)>=2},txt)
word=table(count)
tt=head(sort(word,decreasing = T),20)
kk=barplot(tt,ylim=c(0,300),col=rainbow(20))
text(kk,tt,paste0(tt,"개"),pos=3,las=2,col=2,cex=2)
plot.new()

library(RColorBrewer)
display.brewer.all()
palate=brewer.pal(12,"Set3")
wordcloud(names(word),
          freq = word,
          min.freq = 2,
          scale=c(5,0.2),
          random.order = F, #빈도가 높을수록 중앙출현
          random.color = T,
          colors = palate
          )

wordcloud2(data=word,
           size=0.4,
           shape="star")
