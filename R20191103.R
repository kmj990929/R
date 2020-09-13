#6일차 수업 - 20191103(일)

library(readxl)
library(dplyr)
library(ggplot2)

mpg <- as.data.frame(ggplot2::mpg)
midwest <- as.data.frame(ggplot2::midwest)
View(mpg)
View(midwest)

# 산점도 확인학습
# 1. 도시 연비와 고속도로 연비 간에 어떤 관계가 있는지 알아보려고 합니다. 산점도 그래프로 확인하시오. (x:cty, y:hwy)
ggplot(data=mpg, aes(x=cty, y=hwy)) +
  geom_point()

#2.  midwest에서 전체 인구와 아시아 인구 간에 어떤 관계가 있는지 알아보려고 합니다. x축은 전체 인구, y축은 아시아인구로 구성된 산점도 그래프를 만들어 확인하시오. 전체 인구는 50만명 이하, 아시아인 인구는 1만명 이하인 지역만 산점도에 표시하시오.
ggplot(data=midwest, aes(x=poptotal, y=popasian)) +
  geom_point() +
  xlim(0,500000) +
  ylim(0,10000)

#막대 그래프 확인학습
#1. 어떤 회사에서 생산한 'suv' 차종의 도시 연비가 높은지 확인하려고 합니다. 'suv' 차종을 대상으로 도시 연비의 평균이 가장 높은 회사 5곳을 막대그래프로 표현하시오. 막대는 연비가 높은 순으로 정렬하여 표시하시오.
suv_cty <- mpg %>% filter(class == "suv") %>% 
  group_by(manufacturer) %>% 
  summarise(mean_cty = mean(cty)) %>% 
  arrange(-mean_cty) %>% 
  head(5)

View(suv_cty)

ggplot(data=suv_cty, aes(x=reorder(manufacturer,-mean_cty), y=mean_cty))+
  geom_col()

#2. 자동차 중에서 어떤 차종이 많은지 확인하려고 합니다. 자동차 종류별 빈도를 표현한 막대그래프를 만들어 확인하시오.
ggplot(data=mpg, aes(x=class))+
  geom_bar()

table(mpg$class) # 빈도 나타내주는 함수


##############

#3. 선 그래프 - 시간에 따라 달라지는 데이터를 표현 할 때 주로 사용, 시계열 그래프라고도 부른다. ex)날씨, 주식, 환율 ...

eco <- economics #r내장함수라서 as.data.frame이런거 안써도 된다
View(eco)

# 시간에 따라 실업자 수의 증감 추이를 선 그래프로 표현
ggplot(data = eco, aes(x=date,y=unemploy)) +
  geom_line(size=1, color = "blue")

#시간에 따라 개인별 저축률의 증감추이를 선 그래프로 표현
ggplot(data=eco, aes(x=date,y=psavert)) +
  geom_line(color="blue") +
  geom_point(color ="red", size=1)

# 4. ggplot 내 boxplot(상자 그림) 그래프 - 데이터의 분포를 직사각형 형태의 상자모양으로 표현
# 상자그림으로 데이터를 확인하면 평균을 볼 때보다 좀더 데이터의 특징을 명확하게 파악할 수 있다.

# 구동방식 별 고속도로 연비를 상자 그림으로 표현해보자.
ggplot(data=mpg, aes(x = drv, y= hwy)) +
  geom_boxplot() #r 내장 boxplot보다는 조금 더 많은 데이터를 표현해준다

# 선그래프 확인학습
View(airquality)
air <- airquality  

# 1. 날짜에 따라 온도의 변화를 선 그래프로 표현하시오.
#air <- air %>% mutate(date = Month+"-"+Day)
ggplot(data=air, aes(x=Day, y=Temp)) +
  geom_line(color = "darkgray") +
  geom_point(color = "red")

# 상자 그림 확인학습
#1. 자동차 종류가 'compact', 'subcompact', 'suv'인 자동차의 도시 연비가 어떻게 다른지 비교하려고 합니다. 이 3가지 차종의 도시 연비를 상자그림으로 나타내보시오.
mpg_css <- mpg %>% filter(class %in% c("compact","subcompact","suv"))
View(mpg_css)
  
ggplot(data=mpg_css, aes(x=class, y=cty)) +
  geom_boxplot()

### 응용 그래프 

# 4. 누적 막대 그래프
cars<-mtcars
View(cars)

# 실린더 종류별 빈도를 파악하여, 기어를 누적 막대 그래프로 표현
# x축 : 실린더(cyl), y축 : gear

ggplot(data = cars, aes(x=cyl))  +#빈도만 누적하는 거니까 실린더만 써준다.
  geom_bar(aes(fill = factor(gear)))

# 5. 선버스트 그래프
ggplot(data = cars, aes(x=cyl)) +
  geom_bar(aes(fill = factor(gear))) +
  coord_polar()

# 6. 원형 그래프
ggplot(data = cars, aes(x=cyl)) +
  geom_bar(aes(fill = factor(gear))) +
  coord_polar(theta = "y")

# 7. 히스토그램 - 도수 분포를 기둥 모양의 그래프로 표현
# 온도의 변화를 도수 분포로 표현
ggplot(data=air, aes(x=Temp))+
  geom_histogram(binwidth = 1, color = "red", fill = NA) #밑변 넓이..?

# 8. 선 그래프의 응용
# 시간에 따른 저축률의 변화 - 기울기와 y절편을 활용
# intercept : y절편, slope : 기울기
ggplot(data=eco, aes(x=date, y=psavert)) + geom_line() +
  geom_abline(intercept = 12.18671, slope = -0.000544)
#?????? 이거 값 어떻게 구했는지? intercept가 12.18인데 실제 그래프에서 12.5 이상인 부분이 조금 이상하다

# 시간에 따른 저축률의 변화 - 저축율 평균 수평선을 활용
ggplot(data=eco, aes(x=date, y=psavert)) + 
  geom_line() +
  geom_hline(yintercept = mean(eco$psavert), color = "red")
#?????? 여기서는 yintercept라고 쓰는데 왜 위에서는 intercept 만으로 y절편이라는 뜻이 되는지

#시간에 따른 저축률의 변화 - 저축률이 가장 낮은 날짜
x_inter <- filter(eco, psavert == min(eco$psavert))$date #dplyr 꺼 안써도 된다.
#이거랑 같은 의미다 (이건 dplyr 내 함수 사용하는 것)
x_inter2 <- eco %>% filter(psavert == min(psavert)) %>% select(date)
x_inter
x_inter2$date #x_inter2의 경우 1,1의 table type이기 때문에 xintercept에 바로 넣으면 안됨. 그래서 date로 따로 변수만 뜯어서 보내주는것!

ggplot(data=eco, aes(x=date, y=psavert)) +
  geom_line()+
  geom_vline(xintercept = x_inter, color = "red")

#9. 그래프에 텍스트를 추가하는 방법
#hjust(옆)와 vjust(위아래) : +는 왼쪽, 아래쪽을 나타내고, -는 오른쪽, 위쪽을 나타낸다.
ggplot(data=air, aes(x=Day, y=Temp)) + 
  geom_point() +
  geom_text(aes(label = Temp), hjust = 0, vjust=-2, color = "red", size = 3) #위치 안옮기면 점과 겹쳐져 나타나서 보기에 좋지 않음. 아래쪽, 왼쪽으로 움직인다.

# 10. 그래프를 강조하는 방법 - 도형, 화살표, 텍스트 이용
# 자동차 wt(무게)에 따른 mpg(효율)를 산점도로 표현
ggplot(data=cars, aes(x=wt, y=mpg)) +
  geom_point() +
  annotate("rect",xmin=3, xmax=4,ymin=12, ymax=21, fill = "skyblue", alpha = 0.5,color ="red") + #alpha가 반투명, fill을 na로 주면 투명해짐. color는 테두리 말하는 듯
  annotate("segment", x=2.5, xend=3.7, y=10, yend=17, color = "red", arrow=arrow(), size =1) #시점과 끝점. arrow() 안넣으면 화살표 머리 없음

ggplot(data=cars, aes(x=wt, y=mpg)) +
  geom_point() +
  annotate("rect",xmin=3, xmax=4,ymin=12, ymax=21, fill = "pink", alpha = 0.5,color ="red") + #alpha가 반투명, fill을 na로 주면 투명해짐. color는 테두리 말하는 듯
  annotate("segment", x=2.5, xend=3.7, y=10, yend=17, color = "red", arrow=arrow(), size =1) + #시점과 끝점. arrow() 안넣으면 화살표 머리 없음
  annotate("text", x = 2.5, y=10, label="강조", size=5, color = "red") +
  annotate("text", x=3.8, y= 18, label = "팰리세이드", color = "blue")

# 11. 그래프를 강조하는 방법 2 - 막대그래프에서 활용
# 기어 갯수 별 자동차 수를 표현
ggplot(data = cars, aes(x=gear)) +
  geom_bar() +
  labs(x="기어수", y="자동차수", title = "기어수별 자동차수")

ggplot(data = cars, aes(x=gear)) +
  geom_bar() +
  labs(x="기어수", y="자동차수", title = "기어수별 자동차수") +
  theme_bw()

#12. 인터렉티브 그래프를 만드는 방법1
#plotly - ggplot2에서 생성한 그래프를 인터렉티브하게 만들어주는 패키지
install.packages("plotly")
library(plotly)  #ggplot으로 만든 그래프를 인터렉티브하게 만들어주는 패키지

#mpg에서 배기량(displ)에서 구동방식(drv)을 추가하여 고속도로 연비(hwy)와의 상관 관계를 산점도 그래프로 확인
p <- ggplot(data=mpg, aes(x=displ, y=hwy, col = drv)) +geom_point()
ggplotly(p) #커서를 가져다 대면 정보가 나타난다.webpage- html로 저장해서 크롬으로 열면 됨

View(diamonds)
dia <- diamonds

#다이아몬드의 커팅에 따른 선명도를 누적해서 표현
d1<-ggplot(data=dia, aes(x=cut, fill=clarity)) +geom_bar()

#다이아몬드의 커팅에 따른 선명도를 따로 분리해서 표현
d2<-ggplot(data=dia, aes(x=cut, fill=clarity)) +geom_bar(position = "dodge")

ggplotly(d1)
ggplotly(d2)

#13. 인터렉티브한 그래프를 만드는 방법2
# dygraphs - 시계열 그래프를 인터렉티브하게 만들어주는 그래프
install.packages("dygraphs")
library(dygraphs)

class(eco)
str(eco)

#dygraphs에서 사용할 때 
#데이터 시간 순서의 속성을 가지려면, xts라는 데이터 타입으로 설정되어 있어야만 한다.
install.packages("xts")
library(xts)

#시간에 따른 실업률을 xts 형식으로 저장
eco2 <- xts(eco$unemploy, order.by = eco$date) 
eco2
View(eco2)

dygraph(eco2) %>% dyRangeSelector()

#시간에 따른 저축률을 xts 형식으로 저장
eco_save <- xts(eco$psavert, order.by=eco$date)
eco_save

dygraph(eco_save) %>% dyRangeSelector() #dyRangeSelector는 밑에 있는 범위 설정자를 만드는 거다

### 시간에 따른 실업률과 저축률을 하나의 그래프에 표현
# 실업률과 저축률의 y축 단위를 일치시켜야 한다.
# 실업률을 1000으로 나누어 다시 저장한다.
eco_unemploy <- xts(eco$unemploy/1000, order.by = eco$date)
View(eco_unemploy)

#실업률과 저축률을 하나의 그래프에 표현하는 작업
eco_merge <- cbind(eco_unemploy, eco_save)
View(eco_merge)

#컬럼명 변경
colnames(eco_merge) <-c("실업률", "저축률")
dygraph(eco_merge) %>% dyRangeSelector()
