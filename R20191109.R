#7일차 수업 - 2019109(토)

library(readxl)
library(dplyr)
library(ggplot2)

#변수의 타입, 변환 함수
#연속 변수
v1 <- c(1,2,3,3,2)
v1
class(v1)

v2 <- 1:5
v2

v3 <- seq(3,5)
v3

v4 <- seq(1,10,2)
v4

#범주의 변수
# 특히 문자열에 대해서,factor 변수로 되어있으면 분석 잘못됨
f1 <- factor(c(1,2,3,1,2))
f1
class(f1)

# 1. as.numeric 변환함수 - factor를 numeric으로 변환
v5 <- as.numeric(f1)
v5
class(v5)

# 2. as.character 변환함수 - numeric을 character로 변환
v6 <- as.character(v1)
v6
class(v6)

# 3. as.factor 변환함수 - numeric을 factor로 변환
f2 <- as.factor(v1)
f2
class(f2)

# 4. as.Date 변환함수 - character를 Date(날짜)로 변환
day1 <- c("2011-01-01","2012-07-15","2019-11-09")
day1
class(day1)

day2 <- as.Date(day1)
day2
class(day2)

# 5. as.data.frame 변환함수 - dataset을 dataframe으로 변환
midwest <- as.data.frame(ggplot2::midwest)

# 데이터 타입의 종류
# numeric : 실수, integer : 정수, complex : 복소수, character : 문자, logical : 논리(TRUE, FALSE)
# factor : 범주, Date : 날짜

#########################

#spss 가져오기 위한 패키지
#foreign 패키지 =  spss를 데이터 프레임으로 가져올 수 있도록 하는 패키지
#한국복지패널에서 다운받은 data

install.packages("foreign")
library(foreign)

raw_welfare <- read.spss("c:/rstudy/data1/Koweps_hpc13_2018_beta1.sav",to.data.frame = T)
welfare <- raw_welfare #복사본으로 가공하겠다
View(welfare)

# 분석할 변수 - 성별(sex), 출생년도(birth), 결혼여부(marriage), 종교유무(religion), 월급(income), 직업(code_job), 지역(code_region)
# h13_g3, h13_g4, h13_g10, h13_g11,p1302_8aq1, h13_eco9, h13_reg7)

#7개의 변수만 추출해서 이름 바꿔서 저장하겠다!
# 1. 필요한 데이터만 잘라내기 : raw_welfare에서 사용할 변수 7개의 변수명을 변경
welfare <- rename(welfare, sex = h13_g3, birth = h13_g4, marriage = h13_g10, religion = h13_g11, income = p1302_8aq1, code_job = h13_eco9, code_region = h13_reg7) 

View(welfare)

#2. welfare 7개의 변수를 추출하여 저장
welfare <- welfare %>% select(sex, birth, marriage, religion, income, code_job, code_region)
View(welfare)

#3. 가공한 데이터 프레임을 파일로 저장
write.table(welfare, "c:/rstudy/work/welfare.txt") #txt file
write.csv(welfare, "c:/rstudy/work/welfare.csv") #csv file
library(xlsx)
write.xlsx(welfare, "c:/rstudy/work/welfare.xlsx") #excel file

#######################
### 정제된 데이터를 분석 작업
class(welfare) #data.frame이어야 함
dim(welfare)
str(welfare)
summary(welfare)






#################################
# < 1번째 프로젝트 - 성별에 따른 월급 차이>
# [1단계] 변수 검토 및 전처리 (성별, 월급)
# 1-1. 성별 변수 확인(결측치, 이상치 확인)
# 성별 - 1:남성, 2:여성
# 성별의 빈도 - 1:6749, 2:8174
table(welfare$sex)
#성별의 결측치 확인 - false :14923, true : 0 -> 성별에 결측치, 이상치 데이터는 존재하지 않는다.
table(is.na(welfare$sex)) #table은 빈도를 나타내준다. - true 없으니까 결측치 없다.

#만약에 이상치 데이터가 존재한다고 가정하면, na로 바꿔주자!
welfare$sex <- ifelse(welfare$sex %in% c(1,2), welfare$sex, NA) #%in%는 매칭연산자

#데이터를 알아보기 쉽게 바꿔보자! : 성별 변수의 값을 변경 - 1 > male, 2 > female
welfare$sex <- ifelse(welfare$sex ==1, "male", "female")

table(welfare$sex)
qplot(welfare$sex)

# 1-2. 월급 변수 확인(결측치, 이상치 확인)
class(welfare$income)
summary(welfare$income) #min : 0, max : 4800
table(welfare$income) #월급의 빈도. 크게 의미는 없다
#월급의 결측치 데이터 확인
table(is.na(welfare$income)) # false:4563, true:10360

#월급의 이상치 데이터 처리 - 0, 9999 -> NA
welfare$income <- ifelse(welfare$income %in% c(0,9999), NA, welfare$income)
table(is.na(welfare$income)) # false:4556, true:10367

table(welfare$sex) #female : 8174, male:6749

View(raw_welfare)

# [2단계] 분석표(통계요약표)
# 2-1. 성별 월급 평균 분석표
sex_income <- welfare %>% filter(!is.na(income)) %>%
  group_by(sex) %>% 
  summarise(mean_income = mean(income))

sex_income # 월급의 평균 - female : 179, male:347

# [3단계] 시각화(그래프)
# 3. 성별에 따른 월급 그래프 - 막대그래프
ggplot(data=sex_income, aes(x=sex, y=mean_income)) + geom_col()

# [4단계] 분석 결과
# ~~~ 남성이 여성의 두 배 가까운 월급을 받고 있었다.





################################
#<2번째 프로젝트 - 나이에 따른 월급 차이>
#[1단계] 변수 검토 및 전처리(나이, 월급)
#1-1. 나이 변수 확인(결측치, 이상치 확인)
class(welfare$birth)
table(welfare$birth) # 출생연도의 빈도-크게 의미 없음
#출생연도의 통계자료 확인
summary(welfare$birth) #min:1907, median:1967, mean:1969, max:2017

#출생년도 변수의 결측치 확인
table(is.na(welfare$birth))

#만약 출생년도에 결측치 데이터가 있다면,
welfare$birth <- ifelse(welfare$birth ==9999, NA, welfare$birth)

#출생년도를 통해서 나이를 계산하여 age라는 파생변수 생성 : 2018 - birth + 1
welfare$age <- 2018- welfare$birth +1
summary(welfare$age) #min : 2, median : 52, mean : 50.19, max : 112

#1-2. 월급 변수 확인(결측치, 이상치 확인)
#1번째 프로젝트에서 이미 생성함

#[2단계] 분석표(통계요약표)
#2. 나이에 따른 월급 평균 분석표
age_income <- welfare %>% filter(!is.na(income)) %>% 
  group_by(age) %>% 
  summarise(mean_income =mean(income))

View(age_income)
dim(age_income)

#[3단계] 시각화(그래프)
#3. 나이에 따른 월급 평균 그래프 - 선(시계열) 그래프
ggplot(data=age_income, aes(x=age, y=mean_income)) + geom_line(color = "blue") + geom_point(color = "skyblue", size=1)

#[4단계] 분석결과




###############################
#<3번째 프로젝트 - 연령대에 따른 월급의 차이>
#[1단계] 변수 검토 및 전처리 (연령대, 월급)
#1-1. 연령대 변수 전처리 - 연령대 변수 생성
#age(나이)변수로부터 파생변수 ageg(연령대)를 생성
#연령대의 구분 - 초년 : 30세 미만, 중년 : 30~60세 미만, 노년 : 60세 이상
#ageg - young: 초년, middle: 중년, old: 노년
welfare <- welfare %>% 
  mutate(ageg = ifelse(age < 30, "young", ifelse(age<60, "middle", "old")))

welfare$ageg <- ifelse(welfare$age < 30, "young",
                       ifelse(welfare$age < 60, "middle", "old"))

#연령대 빈도 확인
table(welfare$ageg) # young : 3679, middle : 5223, old:6021

#1-2. 월급 변수 전처리
#1번째 프로젝트에서 이미 처리함.

#[2단계] 분석표(통계요약표)
#2. 연령대에 따른 월급의 평균 분석표
ageg_income <- welfare %>% filter(!is.na(income)) %>% 
  group_by(ageg) %>% 
  summarise(mean_income = mean(income))

ageg_income # young : 188, middle : 320, old : 135

#[3단계] 시각화(그래프)
#1.연령대에 따른 월급의 평균 그래프 - 막대 그래프
ggplot(data=ageg_income, aes(x=ageg, y=mean_income)) +
  geom_col(fill="skyblue")

#x축의 변수의 이름에 따라 정렬하고 싶다!(young->middle->old 순으로 하고 싶다!)
ggplot(data=ageg_income, aes(x=ageg, y=mean_income)) +
  geom_col(fill="skyblue", alpha=0.5) +
  scale_x_discrete(limits = c("young", "middle", "old"))


#[4단계] 분석 결과
# 초년에는 188만원, 중년에는 320만원, 노년에는 135만원의 평균 월급을 받게 됨을 알 수 있다.






#################################
#<개인>
min_welfare <- read.spss("c:/rstudy/data1/Koweps_p13_2018_beta1.sav",to.data.frame = T)
View(min_welfare)
#high : 고등학교 종류, life : 삶의 사다리 점수, final: 최종학력
min <- rename(min_welfare, high = p1307_3aq2, high_region = p1307_3aq4, major = p1307_3aq5, university = p1307_3aq7, suicide_yn = p1305_6aq1, suicide_age = p1305_6aq2, life = p1305_12aq1, final = p1307_3aq1)
min <- min %>% select(high, high_region, major, university, suicide_yn, suicide_age, life, final)
View(min)
write.xlsx(min, "c:/rstudy/work/min.xlsx")

min_mean <- min %>% filter(!is.na(life)) %>% 
  group_by(high) %>% 
  summarise(mean_life = mean(life))

View(min_mean)

write.xlsx(min_select, "c:/rstudy/work/min_select.xlsx")



young <- welfare %>% filter(age == 19) 
young