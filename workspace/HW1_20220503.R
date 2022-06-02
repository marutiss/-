#Movie rates Crawling
library(rvest)
library(stringr)
library(dplyr)
library(ggplot2)
library(wordcloud)
library(wordcloud2)

name=c()
rate=c()
#영화평점 사이트
url="https://movie.naver.com/movie/point/af/list.naver?&page="

for (i in 1:100) {
  base_url=paste0(url,i) #페이지 이동
  n_css=".color_b" #영화제목
  r_css="#old_content em" #영화평점
  
  hdoc=read_html(base_url,encoding = "UTF-8") #url의 html소스코드 인코딩
  
  n_node=html_nodes(hdoc,n_css)
  r_node=html_nodes(hdoc,r_css)
  #태그들의 소스코드 및 속성 추출
  
  n_part=html_text(n_node)
  r_part=html_text(r_node)
  #텍스트속성만 추출
  
  name=c(name,n_part)
  rate=c(rate,r_part)
  
}

Movie=data.frame(name,rate)
txt=sapply(cnt, extractNoun,USE.NAMES = F)
txt=unlist(txt)#백터화 >>분석가능
class(txt)
