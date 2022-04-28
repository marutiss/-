setwd("c:/Rdata")
getwd()
install.packages("dplyr")
x=rnorm(100)

#probability(백분률화),break
hist(x,probability = T,breaks = 5)
shapiro.test(x)# p>=0.05이면 대칭형종형구조의 데이터형태이다.(정규성이 있다)

ht=rnorm(100,175,1) #평균 175,표준편차 1
hist(ht,probability = T)
shapiro.test(ht)
lines(density(ht),col=2,type = 'h',lwd=2)

require(dplyr) #==library()
exam=read.csv("csv_exam.csv")
head(exam,3)

#ctrl+shift+m 입력하면 %>%
exam %>% filter(class==1)
exam %>% filter(class==2)
exam %>% filter(class!=1)
exam %>% filter(math>=60&english>=60) #and
exam %>% filter(math>=90|english>=90|science>=90) #or
exam %>% filter(class==1|class==3|class==5)
exam %>% filter(class%in% c(1,3,5)) #윗줄과 동일기능

exam %>% select(math)
exam %>% select(english)
exam %>% select(math,science,english)
exam %>% select(-math) #수학만 빼고 
exam %>% select(-math,-science) #수학 과학 빼고
#examㅇ에서 1반에서 영수 고르기
exam %>% 
  filter(class==1) %>% 
  select(math,english)
exam %>% 
  select(id,math) %>% 
  head(5)

#arrange()에 대한내용
exam %>% 
  arrange(math) #수학점수기준으로 오름차순으로 
exam %>% 
  arrange(desc(math)) %>% 
  head(10)

exam %>% select(id,english) %>% 
  arrange(desc(english)) %>% 
  head(10)

#파생변수인 mutate()에 대한 내용임
exam %>% 
  mutate(total=math+english+science) %>% 
  head(5)
#그래프를 지우는 방법
#1. plot.new()
#2. frame()
plot.new()

kk=exam %>% 
  mutate(total=math+english+science,mean=round(total/3,1), #소수점 1자리까지
         test=ifelse(mean>=60,"pass","fail")) %>% 
  head(10)

#현재과목별 평균 60점 이상의 합격률을 그래프로 시각화 하여 나타내시오
k3=table(kk$test)
tt=barplot(k3,ylim=c(0,20),col=c("red","blue"))
text(tt,k3,paste0(k3,"명"),pos=3,cex=2,col="red")

#summarize(se)/group_by()
exam %>% summarise(mean_math=mean(math),
                   mean_eng=mean(english),
                   mean_sci=mean(science))

#반별로 수학 영어 과학평균을 나타내시오
data.frame(exam %>% 
              group_by(class) %>% 
              summarise(mean_math=mean(math),
                   mean_eng=mean(english),
                   mean_sci=mean(science),
                   count=n()))
#자동차 제조회사들 중에서 고속도로 연비와 도시연비의 평균을 출력
mpg=data.frame(ggplot2::mpg)

kk2=data.frame( mpg %>% 
  group_by(manufacturer,drv) %>% 
  summarise(mean_cty=mean(cty),
            mean_hwy=mean(hwy),
            count=n()))

kk2


#데이터 합치기 (가로/세로)
# 중간고사 데이터 생성
test1 <- data.frame(id = c(1, 2, 3, 4, 5),
                    midterm = c(60, 80, 70, 90, 85))
# 기말고사 데이터 생성
test2 <- data.frame(id = c(1, 2, 3, 4, 5),
                    final = c(70, 83, 65, 95, 80))
total=left_join(test1,test2,by="id")
total

name <- data.frame(class = c(1, 2, 3, 4, 5),
                   teacher = c("kim", "lee", "park", "choi", "jung"))
name
str(exam)
exam_new=left_join(exam,name,by="class")
exam_new

# 학생 1~5 번 시험 데이터 생성
group_a <- data.frame(id = c(1, 2, 3, 4, 5),
                      test = c(60, 80, 70, 90, 85))
# 학생 6~10 번 시험 데이터 생성
group_b <- data.frame(id = c(6, 7, 8, 9, 10),
                      test = c(70, 83, 65, 95, 80))
group_a
group_b
bind_rows(group_a,group_b)

table(mpg$fl)


install.packages("multilinguer")
library(multilinguer)
install_jdk()
install.packages(c('stringr', 'hash', 'tau', 'Sejong', 'RSQLite', 'devtools'), type = "binary")
install.packages("remotes")
#리모트를 응용해 깃헙접속
remotes::install_github('haven-jeon/KoNLP', upgrade = "never", INSTALL_opts=c("--no-multiarch"))
library(KoNLP)
extractNoun("이성우 입니다")

install.packages("wordcloud")
install.packages("wordcloud2")
library("wordcloud")
library("wordcloud2")


#txt파일 읽어오기~readLines로 여러줄 읽기
txt=readLines("park.txt")

#txt에서 extractNoun을 반복,USE.NAMES>>첫줄을 칼럼으로 사용여부
nouns=sapply(txt,extractNoun,USE.NAMES = F)
class(nouns)
#unlist로 내용물을 벡터로 바꾼다
nouns=unlist(nouns)
class(nouns)
#사용자정의함수의 응용 x=nouns
nouns=Filter(function(x){nchar(x)>=2},nouns)
#데이터 저장, 파일로
write(nouns,"park2.txt")
#인덱싱화 된 데이터 읽어오기
rev=read.table("park2.txt")
rev
wordcount=table(rev)
#sort(정렬할거,decreasing = 트루폴스) 숫자자체 정렬 ,디폴트는 오름차순
tt=head(sort(wordcount,decreasing = T),20)
tt
kk=barplot(tt,ylim = c(0,50),las=2,col=rainbow(20))
text(kk,tt,paste0(tt,"개"),pos=3,cex=2,col=2) #
#text(x,y,출력할문자,pos글씨위치 cex글씨크기)
#paste0(tt,"개") 원소를 공백없이 이어준다.

library(RColorBrewer)
display.brewer.all()
palate=brewer.pal(9,"Set1")
#워클: 단어내용,숫자, sc단어위치,회전,2개이상부터만 올리기,숫자가 많을수록 중앙
#컬러랜덤,사용컬러)
wordcloud(
  names(wordcount),
  freq = wordcount,
  scale = c(5,0,5),
  rot.per = 0.25,
  min.freq = 2,
  random.order=F,
  random.color=T,
  colors=palate)
    
wordcloud2(
  data=wordcount,
  size=0.4,
  shape = "diamond" #circle, triangle...
  
  )
