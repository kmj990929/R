#8일차 수업 - 20191110(일)

library(dplyr)
library(readxl)
library(ggplot2)

# <4번째 프로젝트 - 연령대에 따른 월급 차이2>
# [1단계] 변수 검토 및 전처리 (연령대, 월급)
# 1-1. 연령대 변수 전처리
# 연령대 변수는 나이 변수로부터 파생변수 생성
# 20대: 20~29, 30대:30~39, ... 80대: 80~89세

# 7개의 그룹 연령대 파생변수 생성
welfare$ageg2 <- ifelse(welfare$age>=20 & welfare$age<30, "20대",
                        ifelse(welfare$age>=30 & welfare$age< 40, "30대",
                               ifelse(welfare$age>=40 & welfare$age<50, "40대",
                                      ifelse(welfare$age>=50 & welfare$age<60, "50대",
                                             ifelse(welfare$age>=60 & welfare$age<70, "60대",
                                                    ifelse(welfare$age>=70 & welfare$age<80, "70대",
                                                           ifelse(welfare$age>=80 & welfare$age<90, "80대",NA)
))))))
  
# 1-2. 월급 변수 전처리
# 첫번째 프로젝트를 참고

# [2단계] 분석표(요약통계표)
# 2. 연령대에 따른 월급의 평균 분석표
ageg2_income <- welfare %>% filter(!is.na(ageg2), !is.na(income)) %>% 
  group_by(ageg2) %>% 
  summarise(mean_income = mean(income))

ageg2_income
View(ageg2_income)

# [3단계] 시각화(그래프)
# 3. 연령대에 따른 월급의 평균 그래프 - 막대그래프
ggplot(data=ageg2_income, aes(x=ageg2, y=mean_income)) + geom_col()

# [4단계] 분석결과






########################
# <5번째 프로젝트 - 나이별 성별 월급 차이 >
# [1단계] 변수 검토 및 전처리(나이, 성별, 월급)
# 1-1. 나이변수 전처리
# 2번째 프로젝트 참고

# 1-2. 성별변수 전처리
# 1번째 프로젝트 참고

# 1-3. 월급변수 전처리
# 1번째 프로젝트 참고

# [2단계] 분석표(통계요약포)
# 2. 나이별 성별 월급의 평균의 차이
age_sex_income <- welfare %>% filter(!is.na(income)) %>% # age랑 sex는 NA 없어서 안해준것 뿐!
  group_by(age, sex) %>% 
  summarise(mean_income = mean(income))

View(age_sex_income)

# [3단계] 시각화(그래프)
# 3. 나이별 성별 월급의 평균 차이 - 선(시계열)그래프
ggplot(data = age_sex_income, aes(x=age,y=mean_income, col = sex)) +geom_line() #정렬하는 기준이 하나 더 있을 경우!

# [4단계] 분석 결과




#####################
# <6번째 프로젝트 - 연령대별 성별 월급의 평균의 차이> -1
# 연령대 : 3단계로 구분 (초년, 중년, 노년)
# [1단계] 변수 검토 및 전처리(연령대, 성별, 월급)
# 1-1. 연령대 변수 전처리
# 3번째 프로젝트 참고

# 1-2. 성별 변수 전처리
# 1번째 프로젝트 참고

# 1-3. 월급 변수 전처리
# 1번째 프로젝트 참고

# [2단계] 분석표(통계요약표)
# 2. 연령대별 성별에 따른 월급 평균의 차이
ageg_sex_income <- welfare %>% filter(!is.na(ageg) & !is.na(sex) & !is.na(income)) %>% 
  group_by(ageg, sex) %>%
  summarise(mean_income = mean(income))

ageg_sex_income

# [3단계] 시각화(그래프)
# 3. 연령대별 성별에 따른 월급 평균 차이 - 막대 그래프
# 누적 막대 그래프
ggplot(data=ageg_sex_income, aes(x=ageg, y=mean_income, fill=sex)) + geom_col(alpha = 0.5)
#막대그래프일때는 보기가 좋게 하기 위해서 col이라고 쓰는 것 대신 fill이라고 써주자!

# 성별을 따로 분리해서 막대 그래프로 표현(position="dodge")
ggplot(data=ageg_sex_income, aes(x=ageg, y=mean_income, fill=sex)) + 
  geom_col(position="dodge") +
  scale_x_discrete(limits = c("young","middle","old"))

# [4단계] 분석 결과




#
###################
# <7번째 프로젝트 - 연령대별 성별 월급 평균의 차이> -2
# 연령대 7단계로 구분: 20~80대
# [1단계] 변수 검토 및 전처리 (연령대, 성별, 월급)
# 1-1. 연령대 변수 전처리
# 3번째 프로젝트 참고

# 1-2. 성별 변수 전처리
# 1번째 프로젝트 참고

# 1-3. 월급 변수 전처리
# 1번째 프로젝트 참고

# [2단계] 분석표(통계요약표)
# 2. 연령대별 성별에 따른 월급 평균의 차이
ageg2_sex_income <- welfare %>% filter(!is.na(sex) & !is.na(income) & !is.na(ageg2)) %>% 
  group_by(ageg2, sex) %>% 
  summarise(mean_income = mean(income))

ageg2_sex_income
# 20대  female       178. , male         207. 
# 30대  female       238. , male         324. 
# 40대  female       230. , male         431. 
# 50대  female       193. , male         431. 
# 60대  female       126. , male         275. 
# 70대  female        52.6 , male         114. 
# 80대  female        29.5 , male          36.4

# [3단계] 시각화(그래프)
# 3. 연령대별 성별 월급 평균의 차이 - 막대그래프
ggplot(data=ageg2_sex_income, aes(x=ageg2, y=mean_income, fill = sex)) +
  geom_col(position="dodge")

# [4단계] 분석 결과






######################
# <8번째 프로젝트 - 직업별 월급 차이>
# 직업별 월급의 상위 10개, 하위 10개
# [1단계] 변수 검토 및 전처리 (직업코드, 월급)
# 1-1. 직업 변수 전처리
class(welfare$code_job)
# 직업의 빈도 확인
table(welfare$code_job)
# 직업의 결측치 확인 
table(is.na(welfare$code_job)) # FALSE : 6983, TRUE(결측치) : 7940

# 직종코드표를 데이터 프레임으로 생성
list_job <- read_excel("c:/rstudy/data1/Koweps_Codebook.xlsx", col_names=T, sheet=2)
View(list_job)

# 두 개의 데이터 프레임을 결합(join - 직종코드표를 welfare에 가로결합)
#welfare에 list_job을 넣을거다. id를 기준으로!
welfare <- left_join(welfare, list_job, id= "code_job")

table(welfare$job)
table(is.na(welfare$job)) # 위와 동일

# 1-2. 월급 변수 전처리
# 1번째 프로젝트 참고

# [2단계] 분석표(통계요약표)
# 2. 직업에 따른 월급 평균의 차이 (상위 10개, 하위 10개)
# 2-1. 상위 10개의 월급 평균의 차이
job_income <- welfare %>% filter(!is.na(job) & !is.na(income)) %>% 
  group_by(job) %>% 
  summarise(mean_income = mean(income))
  
job_income_top10 <- welfare %>% filter(!is.na(job) & !is.na(income)) %>% 
  group_by(job) %>% 
  summarise(mean_income = mean(income)) %>% 
  arrange(-mean_income) %>% 
  head(10)

View(job_income_top10)

# 1	보험 및 금융 관리자	821.8889
# 2	의회의원 고위공무원 및 공공단체임원	765.2000
# 3	인사 및 경영 전문가	752.0000
# 4	연구 교육 및 법률 관련 관리자	735.5000
# 5	제관원 및 판금원	695.3333
# 6	의료진료 전문가	686.5000
# 7	항공기 선박 기관사 및 관제사	633.5000
# 8	법률 전문가	631.3333
# 9	문화 예술 디자인 및 영상 관련 관리자	607.4000
# 10	통신 및 방송송출 장비 기사	586.3333

job_income_bottom10 <- welfare %>% filter(!is.na(job) & !is.na(income)) %>% 
  group_by(job) %>% 
  summarise(mean_income = mean(income)) %>% 
  arrange(mean_income) %>% 
  head(10)

View(job_income_bottom10)
# 1	기타 서비스관련 단순 종사원	79.70130
# 2	청소원 및 환경 미화원	85.54810
# 3	가사 및 육아 도우미	90.98851
# 4	의료 복지 관련 서비스 종사자	109.39844
# 5	축산 및 사육 관련 종사자	112.50000
# 6	음식관련 단순 종사원	121.21739
# 7	판매관련 단순 종사원	141.57500
# 8	농립어업관련 단순 종사원	142.30000
# 9	큐레이터 사서 및 기록물관리사	150.00000
# 10	문리 기술 및 예능 강사	153.83333

# [3단계] 시각화(그래프)
# 3. 직업별 월급 평균의 차이 - 막대그래프
#상위 10개
#세로 막대 그래프 - x축에 변수 이름이 길어지면 변수명 구분 불가능;-;
ggplot(data=job_income_top10, aes(x=job,y=mean_income)) + geom_col() 
#가로 막대 그래프 - 이름이 길어서 잘리니까 가로막대 그래프로 만들어주자!
ggplot(data=job_income_top10, aes(x=reorder(job, mean_income),y=mean_income)) + geom_col(fill="green", alpha=0.5) + coord_flip()

#하위 10개
ggplot(data=job_income_bottom10, aes(x=reorder(job,-mean_income), y=mean_income)) + geom_col(fill="yellow", alpha=0.5) +coord_flip()

# [4단계] 분석 결과
# 가장 많은 월급을 받는 ~~





################
#<9번째 프로젝트 - 성별 직업의 빈도 차이>
#성별 직업의 빈도 상위 10개
#[1단계] 변수 검토 및 전처리(직업, 성별)
#1-1. 직업 변수 전처리
#8번째 프로젝트 참고

#1-2. 성별 변수 전처리
#1번째 프로젝트 참고

#[2단계] 분석표(통계요약표)
#2. 성별 직업의 빈도 차이 상위 10개
#2-1. 남성 직업의 빈도 상위 10개
job_male_count <- welfare %>% filter(!is.na(job) & sex == "male") %>% 
  group_by(job) %>% 
  summarise(count = n()) %>% 
  arrange(-count) %>% 
  head(10)

job_male_count  
# 1 작물재배 종사자            523
# 2 자동차 운전원              237
# 3 경영관련 사무원            221
# 4 매장 판매 종사자           147
# 5 영업 종사자                124
# 6 청소원 및 환경 미화원      101
# 7 건축마감관련 기능 종사자    87
# 8 건설 및 광업 단순 종사원    86
# 9 경비원 및 검표원            74
# 10 제조관련 단순 종사원        74

#2-2. 여성 직업의 빈도 상위 10개
job_female_count <- welfare %>% filter(!is.na(job) & sex == "female") %>% 
  group_by(job) %>% 
  summarise(count = n()) %>% 
  arrange(-count) %>% 
  head(10)

job_female_count  
# 1 작물재배 종사자                514
# 2 청소원 및 환경 미화원          275
# 3 매장 판매 종사자               208
# 4 제조관련 단순 종사원           169
# 5 회계 및 경리 사무원            166
# 6 의료 복지 관련 서비스 종사자   132
# 7 음식관련 단순 종사원           118
# 8 음식서비스 종사자              115
# 9 가사 및 육아 도우미            114
# 10 기타 서비스관련 단순 종사원    110

# [3단계] 시각화(그래프)
# 3. 성별 직업의 빈도 차이 - 막대 그래프
# 3-1. 남성 직업 빈도 상위 10개
ggplot(data=job_male_count, aes(x=reorder(job,count), y=count)) + geom_col() + coord_flip()


#민주가 해본 count, mean_income 같이 표현하기
job_male_min <- welfare %>% filter(!is.na(job) & sex == "male" & !is.na(income)) %>% 
  group_by(job) %>% 
  summarise(count = n()) %>%
  arrange(-count) %>% 
  head(10)

job_male_min <- left_join(job_male_min, welfare, id= "job")

job_male_min <- job_male_min %>% filter(!is.na(income)) %>% 
  group_by(job) %>% 
  summarise(mean_income = mean(income), count = n())

View(job_male_min)

job_male_merge <- cbind(job_male_min$mean_income, job_male_min$count)
View(job_male_merge)

ggplot(data = job_male_min, aes(x=reorder(job,mean_income), y=mean_income)) + geom_col() + coord_flip()

# 3-2. 여성 직업 빈도 상위 10개
ggplot(data=job_female_count, aes(x=reorder(job,count), y=count)) + geom_col() + coord_flip()

# [4단계] 결과 분석





####################
# <10번째 프로젝트 - 종교 유무에 따른 이혼율>
# 종교가 있다면 이혼율이 더 낮을까?
# [1단계] 변수 검토 및 전처리 (종교, 결혼(이혼))
# 1-1. 종교 변수 전처리
#1 : 종교 있음, 2: 종교 없음, 9: 모름
class(welfare$religion)
table(welfare$religion) # 1: 6694, 2: 8229
table(is.na(welfare$religion)) #false : 14923

#종교 변수의 데이터를 변경 : 1->yes, 2->no
welfare$religion <- ifelse(welfare$religion == 1, "Yes", "No")
table(welfare$religion) 

# 1-2. 결혼 변수 전처리
#0: 비해당, 1: 유배우, 2:사별, 3:이혼, 4:별거, 5:미혼, 6:기타(사망 등)
#위에서 결혼, 이혼 유무가 필요 - 1:결혼유지 -> marriage, 3:이혼 -> divorce, 그 나머지 -> NA
table(welfare$marriage) # 0:2291, 1:7469, 2:2003, 3:705, 4:75, 5:2360, 6:20

welfare$marriage <- ifelse(welfare$marriage %in% c(0,2,4,5,6),NA,
                           ifelse(welfare$marriage == 1, "marriage", "divorce"))

# [2단계] 분석표(통계요약표)
# 종교 유무에 따른 결혼 유지율
religion_marriage <- welfare %>% filter(!is.na(marriage) & !is.na(religion)) %>% 
  group_by(religion, marriage) %>% 
  summarise(count = n()) %>% 
  mutate(total = sum(count)) %>% 
  mutate(ratio = round(count/total*100,1))

religion_marriage
# No       divorce    393  4301   9.1
# No       marriage  3908  4301  90.9
# Yes      divorce    312  3873   8.1
# Yes      marriage  3561  3873  91.9

# [3단계] 시각화(그래프)
ggplot(data=religion_marriage, aes(x=religion, y=ratio, fill=marriage)) + 
  geom_col(position="dodge")

# [4단계] 결과 분석
# 종교를 가진 사람이 그렇지 않은 사람보다 이혼률이 1% 더 낮았다. 
# (종교가 있을 때의 이혼률 8.1%, 없을 때의 이혼률 9.1%)