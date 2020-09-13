#4일차 수업 - 20191027(일)

# 조건문, 반복문, 함수 생성

#2. 반복문 - for, while, repeat 
#2-1. 반복문 - for문
s <- 0
for(i in 1:10) {
  s <- s+i  
}
s

#2-2. 반복문 - while문
s <- 0; i <- 1
while(i <= 10){
  s <- s+i
  i <- i+1
}
s

#2-3. 반복문 - repeat문 : 조건 반복이 아니라 무한 반복문. 조건문과 break문을 통해 repeat 반복문을 탈출한다.
s<-0; i<-1
repeat{
  s<-s+i
  i<-i+1
  
  if(i>10){
    break;
  }
}
s

###for문 활용 연습
#1.1부터 10까지를 출력

for(i in 1:10){
  print(i)
  #cat(i)
}

#2. 문자 벡터의 값을 차례대로 출력
t<-c("John","Mark","Smith","Mary", "Ruby")
for(i in t){
  print(i)
}

#3. 1에서 10까지의 정수 중에서 홀수를 출력
# / :  나누기, %%  : 나머지, %/% : 몫
# 2로 나누어서 나머지가 1인 수를 출력
for (i in 1:10){
  if(i%%2!=0){
    print(i)
  }
}

#4. 1에서 100까지의 정수 중에서 3의 배수를 출력
for (i in 1:100){
  if(i%%3==0){
    print(i)
  }
}

#5. 1에서 100까지의 정수 중에서 3의 배수이면서 4의 배수를 출력
for (i in 1:100){
  if(i%%3==0 & i%%4==0){
    print(i)
  }
}

#6. 1부터 10까지의 정수 중에서 7이 되면 반복문을 탈출
for(i in 1:10){
  if(i==7){
    break;
  }
  print(i)
}

#7. 1부터 10까지의 정수 중에서 5를 제외한 값을 출력
for(i in 1:10){
  if(i!=5){
    print(i)
  }
}

#8. 1부터 10까지의 정수 중에서 5를 제외한 값을 출력
# next 문을 사용 : 반복문의 끝지점으로 이동, next와 반복문의 끝지점 사이에 있는 실행문을 생략하게 된다.
for(i in 1:10){
  if(i==5){
    next;
  }
  print(i)
}

### 함수를 생성하고 활용하는 연습
#1. 1부터 n까지를 출력하는 함수를 생성하고 호출
n_print <- function(){
  n<-scan(what="")
  for(i in 1:n){
    print(i)
  }
}
n_print()

fn_for1<-function(n){
  for(i in 1:n){
    print(i)
  }
}
fn_for1(9)

###############################

# 패키지 정리
#1. readxl - 엑셀 파일을 읽기 위한 패키지
#2. ggplot2 - 시각화를 위한 다양한 함수들을 포함, 통계 데이터  set을 포함하고 있는 패키지
#3. dplyr - 데이터를 정제하기 위한 다양한 함수를 포함하는 패키지 (rename, join 등의 함수가 포함되어 있음)

library(readxl)
library(ggplot2)
library(dplyr)

#midwest 데이터셋 - 미국 중서부 지역의 인구 통계 자료
midwest <- as.data.frame(ggplot2::midwest)
midwest
View(midwest)

#1. poptotal 변수를 total, popasian 변수를 asian으로 변수명을 변경하시오.
midwest<- rename(midwest, total=poptotal, asian=popasian)

#2. total, asian 변수를 이용하여 '전체 인구 대비 아시아 의 인구 백분율'을 나타내는 파생변수를 만들고, 히스토그램을 만들어 보시오. (ratio)
midwest$ratio <- (midwest$asian/midwest$total)*100
hist(midwest$ratio)

#3. 아시아 인구 백분율 전체 평균을 구하고, 이 평균을 초과하면 "large", 그 외에는 "small"이라는 값을 대입하는 파생변수를 만드시오. (average, group)

midwest$average <- mean(midwest$ratio)

midwest$group <- ifelse(midwest$ratio > average, "large", "small")

#4. "large"와 "small"에 해당하는 지역이 얼마나 되는지 빈도표의 빈도막대 그래프를 만드시오.
table(midwest$group)
qplot(midwest$group)


############################
### 데이터 가공(데이터 전처리) - dplyr 패키지 활용
# 1. filter() 함수 - 구하고자 하는 행 추출
exam <- read_excel("c:/rstudy/data1/excel_exam.xlsx")
exam
View(exam)

# dplyr 내 연산자 - %>%(파이프 연산자, 체인 연산자)
exam %>% filter(class==3)
exam %>% filter(math >= 60)
exam %>% filter(class !=1)
exam %>% filter(class == 2 | class == 3| class ==4|class==5)

#%in% - 매칭 연산자
exam %>% filter(class ==2 | class==3 | class ==4)
exam %>% filter(class %in% c(2,3,4))

exam %>% filter(math >= 60 & science >=70)

# filter() 함수를 이용해서 새로운 data를 생성하시오.
class_1 <- exam %>% filter(class ==1)

# 수학 60점 이상, 영어 60점 이상, 과학 60점 이상이 pass 한것 
class_pass <- exam %>%  filter(math>=60 & english >= 60 & science >= 60)
class_pass


mpg <- as.data.frame(ggplot2::mpg)
View(mpg)
### filter() 함수 확인 학습
# 1. 자동차 배기량에 따라 고속도로 연비가 다른지 확인하고자 할 때, 배기량(displ)이 4 이하인 자동차와 5이상인 자동차 중에서 어떤 자동차의 고속도로 연비가 평균적으로 더 높은지 확인하시오.
displ_4 <- mpg %>% filter(displ <= 4)
displ_5 <- mpg %>% filter(displ >= 5)

result_displ <- ifelse(mean(displ_4$hwy)>mean(displ_5$hwy), "배기량이 4 이하인 자동차의 고속도로 연비가 더 높다", 
                       "배기량이 5 이상인 자동차의 고속도로 연비가 더 높다")
result_displ

mean(displ_4$hwy) # 25.96319
mean(displ_5$hwy) # 18.07895

#2. 자동차 제조회사에 따라 도시 연비가 다른지 확인하고자 할 때, "audi"와 "toyota" 중에서 어느 자동차 제조회사의 도시연비가 평균적으로 더 높은지 확인하시오.
manu_audi <- mpg %>% filter(manufacturer == "audi")
manu_toyota <- mpg %>% filter(manufacturer == "toyota")

result_cty <- ifelse(mean(manu_audi$cty)>mean(manu_toyota$cty), "audi의 도시연비가 더 높다", "toyota의 도시연비가 더 높다")
result_cty

mean(manu_audi$cty) # 17.61111
mean(manu_toyota$cty) # 18.52941

#3. "chevrolet", "ford", "honda" 자동차의 고속도로 연비 평균을 확인하고자 할 떄, 이 회사들의 데이터를 추출하여 저장한 후, 고속도로 연비의 전체 평균을 구하시오.
hwy_cfh <- mpg %>% filter(manufacturer %in% c("chevrolet", "ford", "honda"))
mean(hwy_cfh$hwy) # 22.50943


###2. select() 함수 - 구하고자 하는 열(변수) 추출
exam %>% select(math)
exam %>% select(math, science)
exam %>% select(class, math, english)

exam %>% select(-english, -id)
exam %>% select(class, id, math)


### filter()와 select() 함수를 조합하여 사용하는 방법
#1반 2반, 수학점수 빼고
(exam %>% filter(class %in% c(1,2))) %>% select(-math) 

exam %>% filter(class %in% c(1,2,3)) %>% 
  filter(math >= 60 & science >= 60) %>% 
  select(class, math, science)

exam_data<-exam %>% filter(class %in% c(1,2,3)) %>% 
  filter(english >=80) %>% 
  select(class, id, english)

exam_data
head(exam_data,3)
#동일한 뜻으로, 뒤에  %>% head(3), %>% tail(3)을 쓰면 됨!
#exam %>% filter(class %in% c(1,2,3)) %>% 
#  filter(english >=80) %>% 
#  select(class, id, english) %>%  tail(3)

### 3. arrange() - 행을 정렬하는 함수 (오름차순, 내림차순 정렬)
exam %>% filter(class %in% c(1,2,3) & english >= 80) %>% 
  select(class, id, english) %>% 
  arrange(-english) %>% 
  head(5)

exam %>% filter(class !=3 & math >= 60 & science >= 60) %>% 
  select (-english) %>% 
  arrange(-math, -science) %>% 
  head(2)

### filter(), select(), arrange() 함수를 조합하여 활용하는 문제
#1. 자동차 종류(class), 도시연비(cty) 변수를 추출하여 새로운 데이터를 저장하시오.
mpg_cc<-mpg %>% select(class, cty)

#2. 자동차 종류(class)가 "suv"인 자동차와 "compact"인 자동차 중에서 어떤 자동차의 도시 연비의 평균이 더 높은지 확인하시오.
mpg_suv <-mpg %>% filter(class=="suv")
mpg_compact <- mpg %>% filter(class=="compact")

mean(mpg_suv$cty) # 13.5
mean(mpg_compact$cty) #20.12766

cty_compare = ifelse(mean(mpg_suv$cty)>mean(mpg_compact$cty), "suv의 도시연비가 더 높다", "compact의 cty가 더 높다")
cty_compare

#3. "audi"에서 생산한 자동차 중에서 어떤 자동차 모델의 고속도로 연비가 높은지 1~5 위에 해당하는 자동차의 데이터를 출력하시오.
mpg %>% filter(manufacturer == "audi") %>% 
  arrange(-hwy) %>% 
  head(5)


### 4. mutate() 함수 - 파생변수를 생성
exam<-exam %>% mutate(total = math+science+english, average = total /3)

exam<-exam %>% mutate(test=ifelse(average >= 60, "PASS", "FAIL"))


### filter(), select(), arrange(), mutate() 함수를 조합하여 활용하는 문제
# 1. 도시 연비와 고속도로 연비의 복합 연비를 total이라는 파생변수로 생성하고, 복합연비가 가장 높은 자동차 3종의 데이터를 출력하시오.
mpg_5 <- mpg %>% mutate(total = (cty+hwy)/2) %>% 
  arrange(-total) %>% 
  head(5)

mpg_5

# %>% mutate(total = (cty+hwy)/2) %>% 
#  head(3)


### 5. group_by(), summarise() - 그룹별 평균이나 그룹별 빈도와 같은 각 그룹을 요약한 값을 확인하는 함수
# 차종(class) 별로 도시 연비의 평균을 높은 순으로 확인
mpg %>% group_by(class) %>% 
  summarise(mean_cty = mean(cty)) %>% 
  arrange(-mean_cty)

exam %>% group_by(class) %>%
  summarise(mean_math = mean(math)) %>% 
  arrange(-mean_math)



