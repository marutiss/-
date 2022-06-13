flueshot=read.csv("flushot.csv")
model1=glm(flushot~.,data=flueshot,family=binomial)
summary(model1)

exp(0.072)
exp(-0.098)
exp(0.433)
 exp(-1.177+0.072*55-0.098*60+0.433*1)
 0.06966899/(0.06966899+1)

model2=glm(flushot~.-gender,data=flueshot,family=binomial) 
summary(model2)
anova(model1,model2,test = "Chisq") 

model2=glm(flushot~gender,data=flueshot,family=binomial) 
summary(model2)

xtabs(~flueshot$flushot+(model2$fitted>0.1))
xtabs(~flueshot$flushot)#no 135 yes 24

jang <- function() {
  x <- seq(0.01,0.5,0.01)
  
  
  n <- length(x)
  
  error_min <- vector(length=n)
  sens <- vector(length=n)
  spec <- vector(length=n)
  
  for(i in 1:n) {
    tab = xtabs(~flueshot$flushot+(model1$fitted>x[i]))
    
    res = c(민감도=tab[2,2] / sum(tab[2,]) , 특이도=tab[1,1] / sum(tab[1,]) , ErrorRate=(tab[1,2] + tab[2,1]) / sum(tab) )
    
    error_min[i] = (tab[1,2] + tab[2,1]) / sum(tab)  #ErrorRate
    sens[i] = tab[2,2] / sum(tab[2,])
    spec[i] = tab[1,1] / sum(tab[1,])
    
    print(res)
  }
  
  print(error_min)
  print(paste("최소의 ErrorRate는 " , min(error_min) , "이다.")) 
  index = which(error_min<=min(error_min))
  
  #print(index)
  
  print(paste("해당하는 민감도=",sens[min(index)],"이다."))
  print(paste("해당하는 특이도=",spec[min(index)],"이다."))
  print(paste("해당하는 에러율=",error_min[min(index)],"이다."))
  print(paste("해당하는 cutoff=",x[min(index)],"이다."))
  
  plot(1-spec,sens,type='b')
}
jang()
