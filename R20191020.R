# 2일차 수업 - 20191020(일)

### 3. Array(배열) - 3차원 이상의 격자형 데이터 구조 (같은 타입)
x <- 1:6
a1 <- array(x, c(2, 2, 3)); a1
a2 <- array(x, c(2, 3, 4)); a2

dim(a1)
dim(a2)

### 4. List(리스트) - 1차원이고, 여러 가지 타입을 가지는 데이터 구조, 리스트의 원소가 다시 리스트가 될 수 있기 때문에 계층적 구조를 가질 수 있음.
list1 <- list(c(1, 2, 3), c("John", "Mark", "Smith")); list1

class(list1)
dim(list1)

### 5. Dataframe(데이터프레임) - 2차원 형태(직사각형, 테이블, 표)의 데이터 구조 (여러 가지 타입), 실제로 데이터 분석을 위한 데이터 구조

## 데이터프레임을 생성하는 방법 1
# A클럽의 회원의 정보를 생성
id <- 1:10
sex <- c("M", "F", "M", "F", "F", "M", "F", "M", "F", "F")
area <- c("제주", "대구", "인천", "서울", "부산", "전주", "광주", "대구", "서울", "부산")
age <- c(20, 35, 65, 45, 33, 32, 33, 71, 18, 22)
member <- data.frame(id, sex, area, age)
member

dim(member)
class(member)

# 데이터프레임을 확인하는 방법
member
View(member) # 데이터프레임을 테이블 형식의 새로운 창을 띄워 확인 
head(member) # 데이터를 처음부터 6건 확인
head(member, 3) # 데이터를 처음부터 3건 확인
tail(member) # 데이터를 뒤에서부터 6건 확인
tail(member, 4) # 데이터를 뒤에서부터 4건 확인
summary(member) # 요약 통계정보를 확인

member$area # member 데이터프레임의 area 변수를 확인
member$age # member 데이터프레임의 age 변수를 확인

member_age <- member$age
member_age

## 데이터프레임을 생성하는 방법 2
fruit_shop <- data.frame(fruit = c("사과", "딸기", "수박", "포도", "바나나"),
                         price = c(1500, 5000, 15000, 4500, 3000),
                         volume = c(30, 25, 13, 27, 40))
fruit_shop

dim(fruit_shop)
class(fruit_shop)

# 통계함수
sum(fruit_shop$volume) # 과일의 갯수 총합계
mean(fruit_shop$price) # 과일 가격의 평균
max(fruit_shop$volume) # 과일 재고량의 최댓값
min(fruit_shop$price) # 과일 가격의 최솟값

# ggplot2 패키지 - 시각화를 위한 다양한 함수를 내장하는 패키지, 다양한 데이터셋(Data Sets)을 포함하는 패키지

# 패키지 설치/부착/확인/제거
install.packages("ggplot2") # 패키지 설치
library(ggplot2) # 패키지 부착

detach("package:ggplot2") # 패키지 탈착
installed.packages() # 설치된 모든 패키지 확인
update.packages() # 패키지 업데이트

mtcars # R에 내장된 데이터셋(Datasets)
View(mtcars)

mtcars <- mtcars
# mpg를 cyl갯수로 나눈 결과를 저장
mt1 <- mtcars$mpg / mtcars$cyl; mt1
mt2 <- with(mtcars, mpg/cyl); mt2
mt3 <- within(mtcars, x <- mpg/cyl) # 데이터프레임에 새로운 열을 생성하여 저장
View(mt3)

# 도움말 보기
help(mtcars)
?mtcars

## 생성된 데이터프레임에 열(변수) 추가/삭제/수정
# 회원의 정보를 저장한 데이터프레임을 생성
df1 <- data.frame(name = c("Kim", "Lee", "Park", "Kang", "Han"),
                  age = c(33, 25, 41, 52, 29),
                  height = c(180, 178, 172, 176, 185), stringsAsFactors = F)
df1
View(df1)

df1[3, 2] # 3행 2열의 정보를 확인
df1[4, 3] <- 178 # 4행 3열의 정보를 수정

df1[5,] # 5행의 정보를 확인
df1[5,] <- c("Hong", 30, 175) # 5행의 정보를 수정

df1[6,] <- c("Choi", 45, 188) # 6행의 정보를 추가

df1[-3,] # 3행을 제외하여 확인
df1 <- df1[-3,] # 3행을 제거하고 저장

df1[,3] # 3열의 정보를 확인
df1[,-2] # 2열을 제외한 정보를 확인
df1[, c(1,3)] # 1열과 3열의 정보를 확인

df1[-2, -2] # 2행과 2열을 제외한 정보를 확인

# 패키지에 포함된 데이터셋을 데이터프레임으로 저장하는방법
mpg <- as.data.frame(ggplot2::mpg)
mpg
View(mpg)
help(mpg)

### 연산자(Operator)
x1 <- 1:5
x2 <- 6:10
x3 <- 1:7

# 1. 산술 연산자 : +, -, *, /, 몫(%/%), 나머지(%%), 승수(**, ^)
x1 + 3
x2 - 3
x3 * 4
x2 / 2
x2 %/% 2 # 나눗셈의 몫
x2 %% 2 # 나눗셈의 나머지
x2 ** 3 # 승수
x2 ^ 2 # 승수

# 2. 비교 연산자 : >, <, >=, <=, ==, != 
# 비교 연산자의 결과 : TRUE(T, 참), FALSE(F, 거짓)
x4 <- c(3, 2, 5, 4, 1)
x1 > x4
x1 >= x4
x1 < x4
x1 == x4
x1 != x4

# 3. 논리 연산자 : &(AND), |(OR)
x1 > 3 & x1 < 5
x1 > 3 | x1 < 3

####################

### 데이터 파일 읽기/쓰기
# 1. 엑셀(xls, xlsx) 파일 읽기/쓰기
# readxl : 엑셀 패키지, 읽기만 가능
install.packages("readxl")
library(readxl)

df_exam1 <- read_excel("c:/rstudy/data1/excel_exam.xlsx")
df_exam1
View(df_exam1)
head(df_exam1, 5)
tail(df_exam1, 3)

dim(df_exam1)
class(df_exam1)

df_exam2 <- read_excel("c:/rstudy/data1/excel_exam_novar.xlsx")
df_exam2
dim(df_exam2)

df_exam3 <- read_excel("c:/rstudy/data1/excel_exam_novar.xlsx", col_names = F)
df_exam3
dim(df_exam3)

# xlsx : 엑셀 패키지, 읽기/쓰기가 가능
install.packages("xlsx")
library(xlsx)

# 2. csv 파일 읽기/쓰기
df_csv_exam1 <- read.csv("c:/rstudy/data1/csv_exam.csv", stringsAsFactors = F)
df_csv_exam1

score <- df_csv_exam1 # 복사
score <- within(score, total <- math + english + science)
score
write.csv(score, "c:/rstudy/data1/csv_score.csv")





















