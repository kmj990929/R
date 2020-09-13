#3일차 수업 - 20191026(토)
#공유파일 접속 법 -> 윈도우+, \\192.168.5.254

### 데이터 파일 읽기/쓰기
#1. 엑셀(xls, xlsx) 읽기, 쓰기
#1.1 read_x1 -> 엑셀파일 읽는 데에만 활용 가능한 패키지
#엑셀파일 불러오기
install.packages("readxl")
# r에서는 설치 후 부착을 해야 함(library 명령어)
library(readxl)

df_exam1 <- read_xlsx("c:/rstudy/data1/excel_exam.xlsx");

df_exam1
head(df_exam1) # 앞에서 6개
head(df_exam1,3)
tail(df_exam1)
tail(df_exam1,4)
View(df_exam1)

dim(df_exam1) # 데이터의 행과 열을 파악하는 함수
class(df_exam1) #tbl : table의 준말 / 데이터 구조를 간략하게 보여준다.
str(df_exam1) #데이터의 구조를 상세하게 보여준다.
summary(df_exam1) #데이터를 통계적으로 살펴보는 것, 최소, 최대, 평균, 분위값 / 데이터의 정보 확인(통계를 이용하여)

#1-2. xlsx 패키지 활용 <- 엑셀 파일에 대해 읽기, 쓰기 모두 가능
install.packages("xlsx")
install.packages("rJava") # java 실행파일인 jdk를 설치하고 실행한다.
library(xlsx)

df_exam2 <- read.xlsx("c:/rstudy/data1/excel_exam.xlsx", sheetIndex = 1) # 파일의 어떤 시트인지를 알려주어야 함. 시트 구분 가능하니read xl보다 장점이기도 하다.
df_exam2

df_exam3 <- read.xlsx("c:/rstudy/data1/excel_exam_novar.xlsx", sheetIndex = 1)
dim(df_exam3)

df_exam4 <- read.xlsx("c:/rstudy/data1/excel_exam_novar.xlsx", sheetIndex = 1,header = F)
df_exam4
dim(df_exam4)

#쓰기
write.xlsx(df_exam1, file = "c:/rstudy/data1/score.xlsx")

#2. csv 파일 읽기/쓰기
df_csv_exam1 <- read.csv("c:/rstudy/data1/csv_exam.csv", stringsAsFactors = F)
df_csv_exam1
class(df_csv_exam1)

write.csv(df_csv_exam1, "c:/rstudy/data1/csv_score2.csv")

#3.  메모장( txt)  파일 읽기/쓰기 -메모장은 공백이나 컴마로 구분해서 데이터를 주어야 한다. (구분은 해야 되니까!)
df_txt_data1 <- read.table("c:/rstudy/data2/data_ex.txt", header=T)
df_txt_data1
dim(df_txt_data1)
class(df_txt_data1)

write.table(df_txt_data1, "c:/rstudy/data2/txt_data.txt")


# 데이터 set을 데이터 프레임으로 가져오는 방법
##ggplot2 패키지의 mpg data set
install.packages("ggplot2")
library(ggplot2)

mpg <- as.data.frame(ggplot2::mpg)
mpg
View(mpg)

score <- read_excel("c:/rstudy/data1/excel_exam.xlsx", col_names=T)
class(score)
dim(score)
score
View(score)
score$math


##############################


#dplyr 패키지 - R에서 dataframe을 분석하는 다양한 함수들을 포함
install.packages("dplyr") #디플라이어
library(dplyr)

#변수명 변경
score <- rename(score, mat = math)
score
score <- rename(score, eng = english)
score <- rename(score, sci = science)
score

# 파생 변수(derived variable) : 존재하는 변수로부터 새롭게 만들어진 변수
score$tot <- score$mat +score$eng + score$sci # 만드는 것 뿐만 아니라 저장까지 하는거! tot라는 애를 mat+eng+sci로 하겠다!
score

score$ave <- score$tot / 3
score

score$test <-ifelse(score$ave >= 60, "Pass", "Fail")
score

score$grade <- ifelse(score$ave >=90, "A", 
                      ifelse(score$ave >=80, "B", 
                             ifelse(score$ave>=70, "C",
                                    ifelse(score$ave>=60, "D", "F"))))
score

# 1. mpg에서 cty와 hwy를 이용한 복합연비 tot를 생성하시오.
mpg$tot <- (mpg$cty+mpg$hwy)/2
mpg

# 2.mpg에서 cty를 city로, hwy를 highway로 변수명을 변경하시오.
mpg <- rename(mpg, city = cty, highway = hwy)
mpg

# 3. 복합연비의 효율을  eff로 생성하시오. (복합연비가 30 이상이면 "Excellent", 20 이상이면 "Good", 20 미만이면 "Bad")
mpg$eff <- ifelse(mpg$tot >=30, "Excellent",
                  ifelse(mpg$tot >= 20, "Good","Bad"))
mpg



####################################


### 데이터 결합 - dplyr 패키지 안에 있는 함수들을 활용
# 1. 가로결합 - left_join() 함수: 가로 결합 시에 가장 많이 사용하는 함수
test1 <- data.frame(id=1:5,
                    miterm=c(60, 80, 70, 90,85))
test2 <- data.frame(id=1:5,
                    finalterm=c(70, 83, 65, 95, 80))
test1
test2

test3 <- left_join(test1, test2, by="id")
test3

#2.  세로결합 -  bind_rows() 함수: 세로 결합 시에 가장 많이 사용하는 함수
group1 <- data.frame(id=1:5,
                     test = c(60, 80, 70, 90, 85))
group2 <- data.frame(id=6:10,
                     test = c(70, 83, 65, 95, 80))
group1
group2

group_all <- bind_rows(group1, group2)
group_all

#######################
#1. 세로 조인하는 다른 방법들
a <- data.frame(A = c("a", "b", "c"),
                B = c("t", "u", "v"),
                C = c(1,2,3),stringsAsFactors = F)
a
b <- data.frame(A=c("a", "b", "c"),
                B=c("t", "u", "w"),
                C=c(1,2,4),stringsAsFactors = F)
b

# 1-1. bind_rows()를 이용하는 경우 / data1 아래에 data2를 세로로 합쳐줌.
ab1 <- bind_rows(a,b)
ab1

#1-2. intersect() / data1과 data2 중에서 같은 data만 합쳐줌.
ab2<-intersect(a,b)
ab2

#1-3. setdiff() / data1을 기준으로 data2와 비교해서 서로 다른 data만 세로로 합쳐줌.
ab3<- setdiff(a,b)
ab3

###2. 가로조합하는 다른 방법들
A <- data.frame(id=c(1,2,3),
                locale = c("seoul", "busan", "daegu"),stringsAsFactors = F)

B <- data.frame(id=c(1,2,4),
                sex = c("male", "female", "male"), stringsAsFactors = F)
A
B

#2-1. left_join()함수를 이용하는 경우/ 왼쪽 데이터를 기준으로 오른쪽 데이터를 기준 컬럼(변수)으로 합쳐줌.
AB1 <- left_join(A,B, by="id")
AB1

#2-2. right_join()
AB2 <- right_join(A,B,by="id")
AB2

#2-3. inner_join() - data1과 data2에서 기준 컬럼(변수)이 같은 것만 합쳐줌.
AB3 <-inner_join(A,B,by="id")
AB3

#2-4. full_join() - data1과 data2에서 기준 컬럼(변수)으로 모든 것을 합쳐줌.
AB4 <- full_join(A,B,by="id")
AB4

#2-5. bind_cols() - data1의 오른쪽에 data2를 합쳐줌.
AB5 <- bind_cols(A,B)
AB5


str(mpg)
summary(mpg)
table(mpg$fl) #data의 빈도를 확인하는 함수
dim(mpg)

# 문제. mpg에서 fl에 해당하는 연료의 가격을 갖는 price1이라는 파생변수를 만드시오.
# c:2.35$, d:2.38$, e:2.11$, p:2.76$, r:2.22$
#sol1 : ifelse() 함수를 활용
mpg$price1 <- ifelse(mpg$fl == "c", 2.35,
                     ifelse(mpg$fl == "d", 2.38,
                            ifelse(mpg$fl == "e", 2.11,
                                   ifelse(mpg$fl == "p", 2.76, 2.22))))
#sol2 : 데이터 결합 함수를 활용
fuel <- data.frame(fl = c("c","d","e","p","r"),
                   price2 = c(2.35, 2.38, 2.11, 2.76, 2.22), stringsAsFactors = F)
#이게 가로결합해야되니까 기준 컬럼과 같은 이름으로 만들어야 됨. dimension 생각하지 말고 변수별로 묶어서 data frame 만들자. 가로결합인거는 그냥 형식인듯! 이거에 얽매여서 data frame을 덜 효율적으로 짜면 안되자나

mpg <- left_join(mpg,fuel,by="fl")


##############################
### 조건문, 반복문, 함수 생성

# 1. 조건문
# 1-1. 조건문 - if문

x <- 75

# print() 함수 - 콘솔에 출력하는 함수
# scan() 함수 - 콘솔로부터 입력받는 함수
x <- scan(what = "")

#콘솔에 출력하는 함수
if(x >= 90){
  print("A학점")
} else if(x >= 80){
  print("B학점")
} else if(x >= 70){
  print("C학점")
} else if(x >= 60){
  print("D학점")
} else {
  print("F학점")
}

#1-2. 조건문 - switch문
x <- "tdfe"

switch(x,
       one = 1,
       two = 2,
       three = 3,
       4)


# 함수 생성하는 방법
fn_if1 <- function(x) {
  if(x >= 90){
    print("A학점")
  } else if(x >= 80){
    print("B학점")
  } else if(x >= 70){
    print("C학점")
  } else if(x >= 60){
    print("D학점")
  } else {
    print("F학점")
  }
}

fn_if1(85)
fn_if1(78)
