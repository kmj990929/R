#5일차 수업 - 20191102(토)

library("readxl")
library("dplyr")
library("ggplot2")

### 5. group_by(), summarise() - 그룹별 평균이나 그룹별 빈도와 같은 각 그룹을 요약한 값을 확인
#summarize(): 함수 안에서 사용할 수 있는 함수 -mean(): 평균, sum(): 합계, max():최댓값, min(): 최솟값,median(): 중앙값, sd(): 표준편차, n(): 빈도

exam <- read_excel("c:/rstudy/data1/excel_exam.xlsx")
exam
View(exam)
head(exam)
tail(exam)
class(exam)
str(exam)
dim(exam)

exam %>% group_by(class) %>% 
  summarise(mean_math = mean(math),
            sum_math=sum(math),
            max_math=max(math),
            min_math=min(math),
            count_math=n()) #n은 빈도만 구하는 거기 때문에 괄호 안에 뭐 안써준다.


mpg<-as.data.frame(ggplot2::mpg)
mpg_copy <- mpg
View(mpg_copy)
View(mpg)
###dplyr 패키지의 기능 - 데이터 전처리 기능 - 확인 학습
#1. 회사별로 그룹을 나눈 후, 다시 구동방식별로 나누어서 도시 연비의 평균을 확인하시오.
mpg_1<-mpg %>% group_by(manufacturer, drv) %>% 
  summarise(mean_cty = mean(cty))

View(mpg_1)

#2. 회사별로 그룹을 나눈 후, suv의 데이터를 추출하고, 도시 연비와 고속도로 연비의 평균을 구해서 복합연비 파생변수를 생성 후, 복합 연비의 평균의 정보를 높은 순으로(내림차순으로 정렬하여) 5건을 확인하시오.
mpg_2<-mpg %>% group_by(manufacturer) %>% 
  filter(class=="suv") %>% 
  mutate(total=(cty+hwy)/2) %>% 
  summarise(mean_tot = mean(total)) %>% 
  arrange(-mean_tot) %>% 
  head(5)

View(mpg_2)

#3. class별 도시 연비의 평균을 높은 순으로 구해보시오.
mpg %>% group_by(class) %>% 
  summarise(mean_cty = mean(cty)) %>% 
  arrange(-mean_cty)

#4. 고속도로 연비가 가장 높은 회사 3곳의 정보를 확인하시오.
mpg %>% group_by(manufacturer) %>% 
  summarise(mean_hwy = mean(hwy)) %>% 
  arrange(-mean_hwy) %>% 
  head(3)

#5. 회사별 'compact' 차종의 수를 내림차순으로 정렬하여 확인하시오.
mpg %>% group_by(manufacturer) %>% 
  filter(class == "compact") %>% 
  summarise(num_compact = n()) %>% 
  arrange(-num_compact)

##################################

# 결측치 데이터를 다루는 방법
# 결측치(missing value) - 확인할 수 없는 값, 존재하지 않는 값
# NA - not available

df <- data.frame(sex = c("M","F",NA,"M","F"),
                  score = c(5,4,3,4,NA))
df

mean(df$score) #결측치가 있는 데이터를 끌고와서 분석하면 na가 나온다.

#결측치 데이터를 확인하는 함수
is.na(df)
is.na(df$sex)
is.na(df$score)

#결측치 데이터의 빈도를 확인
table(is.na(df))
table(is.na(df$sex))
table(is.na(df$score))

#결측치 데이터가 있는 데이터를 분석하는 방법
#1. 결측치 데이터가 있는 행을 제거 - 데이터의 양이 충분히 많을 때
df %>% filter(is.na(score)) #결측치가 있는 행을 선택
df %>% filter(!is.na(score)) #결측치가 아닌 행을 선택

df_1 <- df %>% filter(!is.na(score))
df_1
mean(df_1$score)

#2. 결측치 데이터가 여러 개의 변수에 있을 때 행을 제거
df_2<-df %>%  filter(!is.na(sex) & !is.na(score))
df_2

#3. 모든 변수에 대해서 결측치 데이터가 있는 행을 제거
df_3 <- na.omit(df)
df_3

# 결측치 데이터를 제거하지 않고 분석하는 방법
#1. 결측치 데이터를 제외하고 평균, 합계를 구하는 방법
mean(df$score) #NA
mean(df$score, na.rm = T)

sum(df$score) #NA
sum(df$score, na.rm =T)

exam <- read_excel("c:/rstudy/data1/excel_exam.xlsx")
View(exam)

# 결측치 데이터를 만들어 넣는 방법
exam[c(3,8,15),"math"] <- NA

#2. 결측치 데이터를 활용하는 방법 - 결측치를 평균값으로 대체하는 방법
mean(exam$math) #NA
mean(exam$math, na.rm = T) #55.23529

exam[c(3,8,15),"math"] <- 55

exam$math <- ifelse(is.na(exam$math),55,exam$math) # 한꺼번에 분석해서 na인거 바꿔주는 방법



######################

#이상치 데이터를 다루는 방법
#이상치(outlier) - 존재할 수 없는 값, 극단적인 값(정상적인 범위를 벗어나는 값)
#1단계 : 이상치 판별
#2단계 : 이상치 데이터는 결측치 데이터로 변경한다.
#3단계 : 결측치 데이터로 처리

#1단계 : 이상치 판별
# 기준 - sex : 남=1, 여=2, score : 1~5
#sex에서 3은 outlier, score에서 6은 outlier
outlier <- data.frame(sex = c(1,2,1,3,2,1),
                      score = c(5,4,3,4,2,6))
outlier
table(outlier$sex)
table(outlier$score)

#2단계 : 이상치 데이터를 결측치(NA)로 변경
outlier$sex <- ifelse(outlier$sex != 1 & outlier$sex !=2, NA, outlier$sex)
outlier

outlier$score <- ifelse(outlier$score >5 |outlier$score <0, NA, outlier$score)
outlier

#3단계 : 결측치 데이터로 처리
#분석 작업 - 성별에 따른 점수의 평균

outlier %>% filter(!is.na(sex)) %>% 
  group_by(sex) %>% 
  summarise(mean_score = mean(score, na.rm=T))



### 결측치 확인 학습
mpg <- as.data.frame(ggplot2::mpg)
View(mpg)

#결측치 데이터 삽입
mpg[c(65,124,131,153,212),"hwy"]<-NA
table(is.na(mpg$hwy))

#1. 구동방식별로 고속도로 연비 평균이 어떻게 다른지 확인하려고 할 때, 결측치 데이터를 확인하시오.
table(is.na(mpg$hwy)) #NA : 5
table(is.na(mpg$drv))

#2. 어떤 구동 방식의 고속도로 연비의 평균이 높은지 확인하시오.
mpg %>% group_by(drv) %>%
  summarise(mean_hwy =mean(hwy, na.rm=T))
##solution2)
mpg %>% filter(!is.na(hwy)) %>% 
  group_by(drv) %>% 
  summarise(mean_hwy =mean(hwy)) %>% 
  arrange(-mean_hwy)

###1단계 처리 : 이상치를 판별
#박스플롯(그림상자) - 데이터를 4등분해서 그림으로 표현해주는 그래프
#밑에서부터 하의 극단치 경계 0%, 1분위수(25%), 2분위수(50%):중앙값/중위수, 3분위수(75%), 상위 극단치(100%)
#하위극단치와 상위극단치 경계를 벗어나는 지점의 값 : 이상치
boxplot(mpg$hwy)
#5가지의 통계치를 확인 -> 이상치 데이터를 확인
boxplot(mpg$hwy) $stats

###2단계 처리 : 이상치 처리 -> 결측치로 변경
mpg$hwy <- ifelse(mpg$hwy <12 | mpg$hwy >37, NA, mpg$hwy)

###3단계 처리 : 결측치를 제외하고 분석
#구동방식별 고속도로 연비의 평균을 확인
table(is.na(mpg$drv))
table(is.na(mpg$hwy))

mpg %>% filter(!is.na(hwy)) %>% 
  group_by(drv) %>% 
  summarise(mean_hwy = mean(hwy)) %>% 
  arrange(-mean_hwy)


########################
mpg <- as.data.frame(ggplot2::mpg)
#시각화 - ggplot2 패키지에 있는 다양한 함수들을 통해서 시각화 학습

# 1. 산점도 - x축, y축에 점으로 데이터를 표현, 변수와 변수와의 관계를 나타낼 때 사용하는 가장 일반적인 그래프 중 하나.
#배기량(displ)별 고속도록 연비(hwy)를 산점도 표현
#x축 : 배기량(displ), y축 : 고속도로 연비(hwy)
ggplot(data = mpg, aes(x=displ, y=hwy)) +  #데이터, 축
  geom_point() + #그래프의 종류
  xlim(3,6) + #x축의 범위
  ylim(10,30) #y축의 범위

#2-1.(값을 나타내는) 막대그래프 - 데이터의 크기를 막대로 표현, 그룹간의 차이를 나타낼 때 사용
#구동방식(drv)별 고속도로 연비(hwy)의 평균을 막대그래프로 표현
mpg_dh <- mpg %>% group_by(drv) %>% 
  summarise(mean_hwy = mean(hwy)) %>% 
  arrange(-mean_hwy)
mpg_dh

#그래프에 정렬이 되지 않은 상태
ggplot(data=mpg_dh, aes(x=drv, y=mean_hwy)) +
  geom_col()

#그래프에 정렬이 된 상태 - 고속도로 연비의 평균을 기준으로 오름차순 정렬
ggplot(data=mpg_dh, aes(x=reorder(drv,mean_hwy), y=mean_hwy)) +
  geom_col()

#그래프에 정렬이 된 상태 - 고속도로 연비의 평균을 기준으로 내림차순 정렬
ggplot(data=mpg_dh, aes(x=reorder(drv,-mean_hwy), y=mean_hwy)) +
  geom_col()
  
# 2-2. 막대그래프를 활용 - 빈도를 나타내는 막대그래프
# 구동방식에 따른 자동차의 빈도수를 막대그래프로 표현
ggplot(data=mpg, aes(x=drv)) +
  geom_bar()

### 막대그래프 : 값을 나타내는 것, 빈도를 나타내는 것을 구분하려면?
# geom_col() : 값, 요약된 정보(통계정보)일 때. -ex)최댓값, 평균값 등
# geom_bar() : 빈도수, 원재료의 정보를 그대로 표현. 

