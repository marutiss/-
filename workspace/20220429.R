setwd("c:/Rdata")
getwd()

#NA NULL? NA=결측치 NULL=빈 값
x=c(3,4,5,NA,7)
x
x1=c(3,4,5,NULL,7)
x1
df <- data.frame(sex = c("M", "F", NA, "M", "F"),
                 score = c(5, 4, 3, 4, NA))
df
table(is.na(df)) #결측치 확인 +테이블 표현
table(is.na(df$sex)) #결측치 빈도확인
sum(df$score) #결측치가 있다면 계산이 안됨
mean(df$score)

library(dplyr)

df %>% 
  filter(!is.na(score)) %>% #na가 아닌걸 보여라
 summarise(mean_sc=mean(score),
           sum_sc=sum(score),
           sd_sc=sd(score))
kk=df %>% 
  filter(!is.na(score))
kk
mean(kk$score)
na.omit(df) #결측치 제거>>결측치가 있는 행을 통으로 제거

df
mean(df$score,na.rm = T) #na가 있으면 삭제좀!(true)
sum(df$score,na.rm = T)# na가 있는 칸만 제거


exam=read.csv("csv_exam.csv")
exam
exam[c(3,8,15),"math"]=NA #math의 3,8,15에 na 넣기
exam %>% 
  summarise(mean_math=mean(math,na.rm=T),
            median_math=median(math,na.rm = T),
            var_math=var(math,na.rm = T),
            max_math=max(math,na.rm = T))

#결측치는 보통 다른값(대표값(평균,최빈값))으로 대체
summary(exam)#요약 잘써보자 추천 평균과 na개수도 보여줌 
exam$math=ifelse(is.na(exam$math),55,exam$math) #na가 true면 55대입 아님 값유지
exam

library(ggplot2)
mpg=data.frame(ggplot2::mpg)#노이즈 제거를 위한 초기화
mpg
table(is.na(mpg$drv))
table(is.na(mpg$hwy))
data.frame(mpg %>% 
  filter(!is.na(hwy)) %>%
  group_by(drv) %>% 
  summarise(mean_hwy=round(mean(hwy),1)))

#abnormal data처리방법
outlier <- data.frame(sex = c(1, 2, 1, 3, 2, 1),
                      score = c(5, 4, 3, 4, 2, 6))

outlier
outlier$sex=ifelse(outlier$sex==3,NA,outlier$sex)
outlier$score=ifelse(outlier$score==6,NA,outlier$score)
data.frame(outlier %>% 
             filter(!is.na(sex)&!is.na(score)) %>% 
             group_by(sex) %>% 
             summarise(mean_sc=mean(score)))

#이상치제거하기
#1. 박스플롯으로 그래프 모양을 살핀다
#2. 박스플롯의 벗어난 정도의 수치값의 범위를 추정한다
#3. 추정된 값으로 NA값을 만들고 이를 사용함

x=c(4,7,10,5,3)
jj=boxplot(x,horizontal = T,col=2)
jj$stats
quantile(x)
#결과상으로 3이하, 10이상이면 이상치로 생각 >>종형그래프에서 
#25%이하 75이상인 부분으; 데이터


IQR(x) #데이터가 몰려있는 부분의 데이터
uu=boxplot(mpg$hwy,horizontal = T,col = 2) #상한치 부분에 이상치 존재
uu$stats
mpg$hwy=ifelse(mpg$hwy>37|mpg$hwy<12,NA,mpg$hwy)
table(is.na(mpg$hwy))

data.frame(mpg %>% group_by(drv) %>% 
  summarise(mean_hwy=round(mean(hwy,na.rm=T),1)))

#---------------------------------------------------------
#레이어 활용
#그리프그리기(ggplot2 pkg)
tt=ggplot(data = mpg, aes(x = displ, y = hwy))+  #좌표평면생성
  geom_point()+ #산점도 추가
  xlim(3,6)+ #x범위 설정
  ylim(10,30) #y범위 설정

#plotly(): 인터랙티브pkg
install.packages("plotly")
library(plotly)
ggplotly(tt) #마우스를 대면 좌표위치 보여줌

df_mpg=data.frame(mpg %>%
                    filter(!is.na(drv)) %>% 
    group_by(drv) %>% 
      summarise(mean_hwy=round(mean(hwy))))

df_mpg
ggplot(data = df_mpg, aes(x =reorder(drv,-mean_hwy), y = mean_hwy))+ 
  #좌표평면생성+x순서 정렬 
  geom_col(fill=c("red","blue","orange"))+ #막대 추가+색추가(rainbow도 있음)
  geom_text(aes(label=paste0(mean_hwy,"km")),vjust=-0.2,col=2,cex=8)+
  coord_flip() #y=x대칭

kk=ggplot(data = economics, aes(x = date, y = unemploy)) + geom_line()
kk #선형 그래프

for(i in 1:10){
  print(i)} #하나 출력할때마다 줄바뀜

kk=c() #주머니먼저 만들고
for(i in 1:10){
  kk=c(kk,i)}
kk

install.packages("foreign")
library(foreign) #spss파일 받는다
library(dplyr) #전처리
library(ggplot2)#시각화
install.packages("readxl")
library(readxl)

#-------------------------
#공공데이터 분석
raw_welfare=read.spss(file ="Koweps_hpc10_2015_beta1.sav",
                      to.data.frame = T)
welfare=raw_welfare

#엑셀표 참고
welfare <- rename(welfare,
                  sex = h10_g3, # 성별
                  birth = h10_g4, # 태어난 연도
                  marriage = h10_g10, # 혼인 상태
                  religion = h10_g11, # 종교
                  income = p1002_8aq1, # 월급
                  code_job = h10_eco9, # 직종 코드
                  code_region = h10_reg7) # 지역 코드

head(welfare)
table(welfare$sex)


#성별에 따라 월급이 다를까?

class(welfare$sex)
table(welfare$sex)
summary(welfare$sex) #NA값 없음 확인
welfare$sex=ifelse(welfare$sex==1,"male","female") #1,2>>male,female로 변경
tt=table(welfare$sex)
qplot(welfare$sex)
pp=barplot(tt,ylim = c(0,12000),col=c("pink","skyblue"))
text(pp,tt,paste0(tt,"명"),pos=3,cex=2,col=2)

class(welfare$income)
summary(welfare$income)
welfare$income=ifelse(welfare$income%in%c(0,9999),NA,welfare$income)
table(is.na(welfare$income))

gender_income=data.frame(welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(sex) %>% 
  summarise(mean_income=round(mean(income),1),count=n()))
gender_income

#시각화로 마무리 해야함
ggplot(data = gender_income,
       aes(x=reorder(sex,-mean_income),y=mean_income))+
  geom_col(fill=c("orange","red"))+
  geom_text(aes(label=paste0(mean_income,"만원")),
            vjust=-0.2,col=2,cex=6)


#나이와 월급의 
class(welfare$birth)
summary(welfare$birth)
welfare$age=2015-welfare$birth+1
summary(welfare$age)
table(is.na(welfare$income))
age_income=data.frame(welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(age) %>% 
  summarise(mean_income=round(mean(income),1),
            count=n()))
age_income

#마무리는 그래프로 보여준다
oo=ggplot(data=age_income,aes(x=age,y=mean_income))+
  geom_line(col="red")
ggplotly(oo)



#연령대별 월급차이
welfare$ageg=ifelse(welfare$age<30,"young",
                    ifelse(welfare$age<60,"middle","old"))
cc=table(welfare$ageg)
ii=barplot(cc,col=rainbow(3),ylim = c(0,7000))
text(ii,cc,paste0(cc,"명"),pos=3,col=2,cex=3)
frame()

k2=data.frame(welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(ageg) %>% 
  summarise(mean_income=round(mean(income),1),
            count=n()))
k2

#마무리는 그래프로 표시
ggplot(data=k2,aes(x=reorder(ageg,-mean_income),y=mean_income))+
  geom_col(fill=c("pink","orange","cyan"))+
  geom_text(aes(label=paste0(mean_income,"만원")),
            vjust=-0.2,col=2,cex=3)

#crawling for daum breakingnews
#rvest(read_html/html_node(s)/html_text())
install.packages("rvest")
install.packages("stringr")
library(rvest)
library(stringr)

title=c()
press=c()
time=c()
body=c()
url=c()

for(i in 1:2){ #반복을 위한 for문
urla="https://news.daum.net/breakingnews?page="

base_url=paste0(urla,i)
t_css="#mArticle .tit_thumb .link_txt"
pt_css=".info_news"
b_css=".desc_thumb .link_txt"
#추출하고자 하는 각 요소들의 SelectorGadget로 추출한 태그


hdoc=read_html(base_url)
t_node=html_nodes(hdoc,t_css)
pt_node=html_nodes(hdoc,pt_css)
b_node=html_nodes(hdoc,b_css)
#태그들의 소스코드 및 속성 추출: html_node(html, '#main')


title_part=html_text(t_node)
pt_part=html_text(pt_node)
#텍스트속성만 추출
pt_part

time_part=str_sub(pt_part,-5)
time_part
press_part=str_sub(pt_part,end=-9)
press_part
#문자열 추출 뒤에서 5개 추출

b_part=html_text(b_node)
b_part
body_part=gsub("\n","",b_part)#\n를 blank처리,
body_part=str_trim(body_part,side = "both")# 끌어온 문장의 앞뒤의 공백제거
body_part
url_part=html_attr(t_node,"href") #href속성 추출 href: 실제로 가야 할 URL

title=c(title,title_part)
press=c(press,press_part)
time=c(time,time_part)
body=c(body,body_part)
url=c(url,url_part)
news=data.frame(title,press,body,url)
}
View(news)
write.csv(news,"news.csv")


