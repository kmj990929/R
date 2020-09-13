#1일차 수업 - 20191116(토)
#빅데이터 분석 심화

library(readxl)
library(ggplot2)
library(dplyr)

########################
### < 워드클라우드(word cloud) 프로젝트 1 >
### 1. 워드 클라우드를 생성하기 위한 전처리 부분
#KoNLP(Korea Natural  Language Processing) 패키지 - 한글 자연어 분석 패키지
#KoNLP 패키지는 JAVA 언어로 생성 - jdk (JAVA Development kit) 설치

install.packages("rjava") #java를 r에서 사용할 수 있도록 하는 패키지
install.packages("memoise")
install.packages("Rcpp") #c++를  r에서 사용할 수 있도록 하는 패키지
install.packages("KoNLP")

library(rJava)
library(memoise)
library(Rcpp)
library(KoNLP)

# JAVA 환경변수 설정
Sys.setenv(JAVA_HOME = "C:/Program Files/Java/jdk1.8.0_221")

#useSystemDic() #한글 283949 단어를 포함하고 있음
#useSejongDic() #한글 370957 단어를 포함
useNIADic() #한글 983012 단어를 포함

hiphop <- readLines("c:/rstudy/data1/hiphop.txt") #readlines : 한줄씩 잘라서 가져오기
View(hiphop)
class(hiphop) #character

#특수문자 제거
install.packages("stringr")
library(stringr)
hiphop <- str_replace_all(hiphop, "\\W", " ")#\\W(대문자): 모든 특수문자라는 뜻. hiphop내 모든 특수문자를 공백으로 대체하겠다. 

#명사 추출
noun <- extractNoun(hiphop) #KoNLP 내장 함수로, 명사만 추출해줌
View(noun)
class(noun) #list type

#리스트 데이터를 벡터 데이터로 변환(분석은 벡터 or 데이터 프레임일때 가능하기 때문) -> 워드카운트(단어(명사)의 빈도표) 생성
wordcount <- table(unlist(noun))
View(wordcount)
class(wordcount) #table(vector) type
dim(wordcount) #3008개의 data - vector type이므로 열은 무조건 1개로 봐서 출력 안됨

#벡터 데이터를 데이터 프레임 데이터로 변환
df_wc <- as.data.frame(wordcount, stringsAsFactors = F) #string을 factor 타입으로 가져오면 분석이 안됨
View(df_wc)
class(df_wc) #dataframe type으로 바뀌어 있음
dim(df_wc) # 3008 2 라고 나옴, 3008개의 행, 열 2개

#변수명 수정
df_wc <- rename(df_wc, word = Var1, freq = Freq)

# 두 글자 이상인 단어를 추출 -> 한 글자 단어를 제거
df_wc2 <- filter(df_wc,nchar(word) >= 2)
#df_wc2 <- df_wc %>% filter() 이렇게 써도 되는데 파이프연산자를 써서 연결할 생각이 아니라면 그냥 위처럼 적는게 편함.
#nchar(word) : word의 글자수
View(df_wc2)
class(df_wc2) #data frame type
dim(df_wc2) #2508 2

#빈도수를 기준으로 상위 100개의 단어를 추출해서 빈도표를 생성
df_wc2_top100 <- df_wc2 %>% arrange(-freq) %>% head(100)

View(df_wc2_top100)
class(df_wc2_top100) #data.frame type
dim(df_wc2_top100) # 100 2


### 2. 워드 클라우드 생성(전처리된 데이터프레임을 통해서)
install.packages("wordcloud")
library(wordcloud)

# 색상 목록 설정
pal <- brewer.pal(8,"Dark2") #Dark2라는 색상 모음에서 8가지 색상을 쓰겠다
pal

#난수(random number) 생성
set.seed(123456)

#워드클라우드 생성
wordcloud(words = df_wc2_top100$word, #단어는 무엇인가
          freq = df_wc2_top100$freq, #빈도는 무엇인가
          min.freq = 5, #최소 단어 빈도 
          max.words = 500, #최대 표현 단어수
          random.color = T, #color를 랜덤하게 하겠다 랜덤 여부, F: 고빈도 단어 중앙 배치..?
          rot.per = 0.1, #10%의 단어들만 돌려서 표현하겠다
          scale = c(4,0.3), #단어 크기 범위 0.3~4포인트
          colors = pal) #색상 목록


#############################
### < 워드클라우드(word Cloud) 프로젝트 2 >
twitter <- read.csv("c:/rstudy/data1/twitter.csv", header=T, stringsAsFactors = F, fileEncoding = "utf-8")
#웹에서 긁어온 데이터의 경우 utf-8로 인코딩 되어있는 경우가 일반적
View(twitter)
class(twitter) #data.frame
dim(twitter) #3743 5

#변수명 변경
twitter <- rename(twitter, no = 번호, id = 계정이름, date = 작성일, content = 내용)

# 명사 추출
tw_noun <- extractNoun(twitter$content)
View(tw_noun)
class(tw_noun) #list
dim(tw_noun) #null

#리스트 데이터를 벡터 데이터로 변경 -> 워드카운트(단어의 빈도표)를 생성
tw_wordcount <- table(unlist(tw_noun))
View(tw_wordcount)
class(tw_wordcount) #table(vector) type
dim(tw_wordcount) # 10677

#벡터 데이터를 데이터 프레임으로 변경
df_tw <- as.data.frame(tw_wordcount, stringsAsFactors = F)
View(df_tw)
class(df_tw) #data.frame
dim(df_tw) #10677 2

#변수명 변경
df_tw <- rename(df_tw, word=Var1, freq = Freq)

#두 글자 이상의 단어 추출 -> 한 글자 단어를 제거
df_tw2 <- filter(df_tw, nchar(word)>=2)
View(df_tw2)
class(df_tw2) #data.frame
dim(df_tw2) #10158 2

#빈도수를 기준으로 상위 100개 단어만 추출해서 데이터 프레임으로 생성
df_tw2_top100 <- df_tw2 %>% arrange(-freq) %>% head(100)
View(df_tw2_top100)
class(df_tw2_top100) #data.frame
dim(df_tw2_top100) # 100 2

### 2. 워드클라우드 생성(전처리된 데이터 프레임을 통해서)

#색상 목록 설정
pal2 <- brewer.pal(9, "Blues")[5:9] #파란 계열의 색
pal2

# 난수 생성
set.seed(456789)

#워드 클라우드 생성
wordcloud(words = df_tw2_top100$word, #단어는 무엇인가
          freq = df_tw2_top100$freq, #빈도는 무엇인가
          min.freq = 10, #최소 단어 빈도 
          max.words = 1000, #최대 표현 단어수
          random.color = T, #color를 랜덤하게 하겠다 랜덤 여부, F: 고빈도 단어 중앙 배치..?
          rot.per = 0.2, #10%의 단어들만 돌려서 표현하겠다
          scale = c(5,1), #단어 크기 범위 0.3~4포인트
          colors = pal) #색상 목록

####################
### <워드 클라우드(word cloud) 프로젝트 3>
### 1. 워드 클라우드를 생성하기 위한 전처리
national_song <- readLines("c:/rstudy/data2/애국가(가사).txt")
View(national_song)
class(national_song) #character
dim(national_song) #Null

#명사 추출
n_noun <- extractNoun(national_song)
View(n_noun)
class(n_noun) #list
dim(n_noun) #null

#리스트 data를 벡터 데이터로 변경 -> 워드카운트를 생성
n_wc <- table(unlist(n_noun))
View(n_wc)
class(n_wc) #table(vector) type
dim(n_wc) #54

#벡터 데이터를 데이터 프레임 데이터로 변경
df_n <- as.data.frame(n_wc, stringsAsFactors = F)
View(df_n)
class(df_n) #data.frame

#변수명 변경
df_n<-rename(df_n, word=Var1, freq=Freq)

#두 글자 이상의 단어 추출
df_n2 <- filter(df_n, nchar(word)>=2)
View(df_n2)
class(df_n2) #data.frame
dim(df_n2) #32 2

###2. 워드클라우드 생성(전처리된 데이터 프레임을 통해서)
install.packages("wordcloud2")
library(wordcloud2)

#1.기본
wordcloud2(df_n2)

#2. 글자색, 배경색 변경
wordcloud2(df_n2, color="random-light", backgroundColor = "#FFFFFF")
wordcloud2(df_n2, color="random-dark", backgroundColor = "yellow")
wordcloud2(df_n2, color="random-dark", backgroundColor = "pink")
wordcloud2(df_n2, color="random-dark", backgroundColor = "cyan")

#3. 글꼴, 크기 변경
wordcloud2(df_n2, fontFamily="궁서체", size= 1.2, color = "random-dark", backgroundColor = "cyan")
#size는 1이 기준으로 크거나 작아진다.

#4. 모양 변경
wordcloud2(df_n2, color = "random-light", backgroundColor = "black", shape="circle") #기본형이 circle
wordcloud2(df_n2, color = "random-light", backgroundColor = "black", shape="star")
wordcloud2(df_n2, color = "random-light", backgroundColor = "black", shape="diamond")
wordcloud2(df_n2, color = "random-light", backgroundColor = "black", shape="triangle")
wordcloud2(df_n2, color = "random-light", backgroundColor = "black", shape="triangle-forward")
wordcloud2(df_n2, color = "random-light", backgroundColor = "black", shape="pentagon")
wordcloud2(df_n2, color = "random-light", backgroundColor = "black", shape="cardioid")

#5. 내장된 데이터셋을 활용 - demoFreq 데이터셋
View(demoFreq)
class(demoFreq) #data.frame
wordcloud2(inf_df2, size=1.2, color=rep_len(c("red","orange","yellow","green","blue","navy", "purple"), nrow(inf_df2)), shape="circle")

# 6. 워드 클라우드 활용
wordcloud2(demoFreq, minRotation=pi/6, maxRotation = pi/6, rotateRatio = 1, shape="star")






#############
### 개인 프로젝트
infinite <- readLines("c:/rstudy/data1/infinite.txt")
View(infinite)
class(infinite) #character

#특수문자 제거
infinite <- str_replace_all(infinite, "\\W", " ")

#명사 추출
inf_noun <- extractNoun(infinite) #KoNLP 내장 함수로, 명사만 추출해줌
View(inf_noun)
class(inf_noun) #list type

#리스트 데이터를 벡터 데이터로 변환(분석은 벡터 or 데이터 프레임일때 가능하기 때문) -> 워드카운트(단어(명사)의 빈도표) 생성
inf_wordcount <- table(unlist(inf_noun))
View(inf_wordcount)
class(inf_wordcount) #table(vector) type

#벡터 데이터를 데이터 프레임 데이터로 변환
inf_df <- as.data.frame(inf_wordcount, stringsAsFactors = F) #string을 factor 타입으로 가져오면 분석이 안됨
View(inf_df)
class(inf_df) #dataframe type으로 바뀌어 있음
dim(inf_df) # 3008 2 라고 나옴, 3008개의 행, 열 2개

#변수명 수정
inf_df <- rename(inf_df, word = Var1, freq = Freq)
View(inf_df)
class(inf_df) #data frame type
dim(inf_df) #2508 2

#두 글자 이상의 단어 추출 -> 한 글자 단어를 제거
inf_df2 <- filter(inf_df, nchar(word)>=2)
View(inf_df2)
class(inf_df2) #data.frame
dim(inf_df2) #10158 2

#빈도수를 기준으로 상위 100개의 단어를 추출해서 빈도표를 생성
inf_df_top100 <- inf_df2 %>% arrange(-freq) %>% head(100)

View(inf_df_top100)






#######################
######### 통계학(statistics) ##########
#모집단(population) - 우리가 알고자 하는 대상 전체, 조사 범위 대상
#표본(sample) - 모집단으로부터 조사하기 위해서 선택된 대상

#총조사, 전수조사 - 모집단을 구성하는 대상 전부를 조사하는 것. ex) 우리나라에서 5년마다 인구주택총조사
#표본조사 - 총조사 비용과 시간이 많이 들고, 경우에 따라서는 총조사 자체가 불가능한 경우도 있어서 일반적으로는 표본을 대상으로 조사하는 표본조사를 실시
#표본추출 - 모집단으로부터 조사에 사용될 표본을 선택하는 것

# < 통계학의 종류 >
# 1. 기술통계학 - 자료를 수집 및 정리하여 자료의 특성으로 자료를 요약하는 통계학의 분야 - 통계자료분석의 기초

# 2. 추측통계학 - 과학적인 방법으로 표본의 특성을 통해서 모집단의 특성을 추론하는 통계학의 분야
# 수집된 자료가 모집단 전체를 조사한 전수조사라면 기술통계학을 통해서 모집단의 특성을 바로 관찰할 수 있지만, 표본을 통해서 자료를 조사할 수 밖에 없는 경우라면 기술통계학을 통해서는 표본의 특성만을 나타낼 뿐이고 이는 우리가 알고 싶어하는 모집단의 특성이 아닐 수도 있음.