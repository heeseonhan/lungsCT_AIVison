#########################################################  
#(BOOK TIELE) : 빅데이터 분석을 위한 데이터 마트 구축  
# (BOOK published in Korea)
#http://cafe.naver.com/dataan
#https://github.com/heeseonhan/bigDataMart

#########################################################

I. 분석 데이터 마트이해와 교차표                               
1. 데이터 마트, 요약변수, 파생변수
2. 교차표 이해
3. 두 속성의 값 구성 파악을 위한 교차표
>table(c("a", "b", "b", "b", "c", "c", "d"))
a b c d
1 3 2 1

4. 명목형 속성 2개, 숫자형 속성 1개의 교차표
> d <- data.frame(x=c("1", "2", "2", "1"),
                  type=c("A", "B", "A", "B"),
                  num=c(3, 5, 8, 7))
> d

> xt <- xtabs(num~x + type, data =d)
> xt

tapply(d$num,list(d$x,d$type),sum)
5. 두 개의 컬럼으로 구성된 데이터에서, 한 개의 컬럼에 대해서만 
교차표작성하기
> col <- data.frame (c1=c("A", "A", "A", "B", "B"), c2 =c(1, 1, 1, 2, 2))
> col
xtabs(~c1,col)

6. length 교차표
>data(mtcars)
>str(mtcars)
tapply(mtcars$mpg, list(mtcars$gear, mtcars$cyl), length)


7. 평균 교차표: 기어의개수와 실린더의 개수별 평균 연비교차표

> xtabs(mpg ~ gear+cyl,data=mtcars)
>tapply(mtcars$mpg, list(mtcars$gear, mtcars$cyl), length)
> tapply(mtcars$mpg, list(mtcars$gear, mtcars$cyl), mean)
> mtcar_tap<-tapply(mtcars$mpg, list(mtcars$gear, mtcars$cyl), mean)
> rownames(mtcar_tap)
> xtabs(mpg ~ gear+cyl,data=mtcars)/tapply(mtcars$mpg, list(mtcars$gear, mtcars$cyl), length)

8. 교차표의 열이름 읽기
> a <- letters[1:3]
> a_sa<-table(a, sample(a))                       
a   a b c
a 0 0 1
b 0 1 0
c 1 0 0
> colnames(a_sa)
[1] "a" "b" "c“

> a_sa_d0<-table(a, sample(a), deparse.level = 0) 
 names(a_sa_d0)
colnames(a_sa_d0)
> a_sa_d2<-table(a, sample(a), deparse.level = 2) # dnn is c("a", "sample(a)")
colnames(a_sa_d2)


9. 결측치(NA)가 집계된 교차표
> a <- rep(c(NA, 1/0:3), 10)
> a
table(a, exclude = NULL)


10. 교차표에  합과 비율  표시하기
> xt<-matrix(c(3,7,8,5),2)
>matrix(c(3,7,8,5),col=2)
> prop.table(xt , 1)

> prop.table(xt , 1)
 prop.table (xt , 2)
prop.table(xt)
> m <- matrix(1:4, 2)
> m
[,1]   [,2]
[1,]    1    3
[2,]    2    4

# 행기준 계산
> prop.table(m, 1)
[,1]      [,2]
[1,] 0.2500000 0.7500000
[2,] 0.3333333 0.6666667

# 열 기준 계산
> prop.table(m, 2)
[,1]      [,2]
[1,] 0.3333333 0.4285714
[2,] 0.6666667 0.5714286

> prop.table(m, c(1,2))


11. 자동차 연비에 영향을 미치는 변수 탐색 교차표 따라하기
str(cars_feature)
sapply(cars_feature, table)
> cars_feature$cyl_f<-factor(cars_feature$cyl)
> cars_feature$vs_f<-factor(cars_feature$vs)
> cars_feature$am_f<-factor(cars_feature$am)
> cars_feature$gear_f<-factor(cars_feature$gear)
> cars_feature$carb_f<-factor(cars_feature$carb)
 xtabs(mpg ~cyl+gear+am,data=cars_feature)
pairs(cars_feature[,c(1,2,8,9,10,11)],pch=as.integer(factor(cars_feature$vs)))

pairs(mtcars[,c(1,2,8,9,10,11)],panel=panel.smooth,
      pch=as.integer(factor(mtcars$vs)))

library(lattice)
>xyplot(cars_feature$mpg ~ cars_feature$cyl | paste(factor(cars_feature$gear),"gear") * paste(factor(cars_feature$am), "am"), data=cars_feature, main="연비와 실린더 개수, 오토 여부와 기어개수")


corrgram(cor(cars_feature), type ="corr", upper.panel = panel.conf, cor.method = "spearman",main="비선형" )
> par(mfrow=c(1,3))
> hist(mtcars$mpg, freq=FALSE,main="연비(mpg)",xlab="연비(mpg)")

#f(x)선그림
> lines(density(mtcars$mpg))

#평균을 녹색 점선으로 덧그림
> abline(v=mean(mtcars$mpg),  lty="dotted",col="green",lwd=3)
> text(20.5, 0.075, "평균", adj = 0,col="green")

#중위수를 빨강색 점선으로 덧그림
> abline(v=median(mtcars$mpg),    col="red",lty="dotted",lwd=3)
> text(16, 0.075, "중위수", adj = 0,col="red")

> hist(mtcars$disp, freq=FALSE,main="배기량(disp)",xlab="배기량(disp)")

#f(x)선그림
> lines(density(mtcars$disp))

#평균을 녹색 점선으로 덧그림
> abline(v=mean(mtcars$disp),  lty="dotted",col="green",lwd=3)
> text(250, 0.004, "평균", adj = 0,col="green")

#중위수를 빨강색 점선으로 덧그림
> abline(v=median(mtcars$disp),    col="red",lty="dotted",lwd=3)
> text(158, 0.004, "중위수", adj = 0,col="red")

#drat
> hist(mtcars$drat, freq=FALSE,main="후방차축 비율(drat)",xlab="후방차축 비율(drat)")

#f(x)선그림
> lines(density(mtcars$drat))

#평균을 녹색 점선으로 덧그림
> abline(v=mean(mtcars$drat),  lty="dotted",col="green",lwd=3)
> text(3.3, 0.77, "평균", adj = 0,col="green")
#중위수를 빨강색 점선으로 덧그림
> abline(v=median(mtcars$drat),    col="red",lty="dotted",lwd=3)
> text(3.8, 0.77, "중위수", adj = 0,col="red")


II. 결측값과 이상값 처리                                           
1. 데이터 검증 이해
2. 결측치 처리 방법
3. 잘못 입력된 값 찾기
> getwd()
> setwd("e:/00-R")
> valida<-read.csv("dataValidation.csv",header=TRUE,encoding = "UTF-8")
> str(valida)
> names(valida)<-c("gender","group","score")
> names(valida)
> valida[valida[,1]>2|valida[,1]<0,]
valida[valida[,2]>3,]
valida[valida[,3]>5,]

4. 특정값 결측으로 설정하기
vScore <- c( 3, 1, 4, 5, 2, 2, 1, 2, 5)
> factor(vScore)
fScore2 <- factor(vScore, labels="f", exclude=c(2, 4), ordered=is.ordered(vScore))

> fScore2 
> fScore3 <- factor(vScore, labels="f", exclude=c(2, 4), ordered=T)

> fScore3
'
#5. 데이터 프레임에서 속성별로 NA 개수 파악
vX1 <- c(1,2,3,NA,5,-9,-8,1,2,3)
> vX2 <- c(1,2,3,4,NA,NA,-9,1,2,-8)
> dTemp <- data.frame(vX1, vX2)
>edit(dTemp)
apply(apply(dTemp,2,is.na),2,sum)


6. 결측치(NA)가 존재하는 행 모두 출력
> diris <- iris
> diris[c(10 , 20, 25,30, 32,40,50 ) , 3] <- NA
> diris[c(33 , 66,100 , 123,133) , 1] <- NA
diris[! complete.cases(diris) ,]

#7. 결측치가 있는 행 삭제
x <- data.frame (a=c(1, 2, 3) , b=c("a", NA , "c"), c=c("a", "b", NA))
na.omit (x)

 dSample <- data.frame(x = c(1, 2, 3), y = c(0, 10, NA))
na.omit(dSample)

x <- data.frame (a=c(1, 2, 3) , b=c("a", NA , "c"), c=c("a", "b", NA))
> na.pass (x)

10. 결측치와 이상값이 있는 케이스에  NA 표시를 남기고 데이터셋
vX1 <- c(1,2,3,NA,5,-9,-8,1,2,3)
> vX2 <- c(1,2,3,4,NA,NA,-9,1,2,-8)
> dX <- data.frame(vX1, vX2)
> dX
 dX <- dX[!dX$vX1<0,]
dX <- dX[!dX$vX2<0,]

#9. 두 속성 중에 하나라도 결측(NA) 값을 가진 행 모두 삭제

> vX1 <- c(1,2,3,NA,5,-9,-8,1,2,3)
> vX2 <- c(1,2,3,4,NA,NA,-9,1,2,-8)
> dX <- data.frame(vX1, vX2)
dX_namoit_df <- dX[!is.na(dX$vX1) & !is.na(dX$vX2),]
> dX_namoit_df

10. 결측(NA)을 중앙값(median)으로 치환
 replacement<- data.frame ( val =c(1, 2, 3, 4, NA , 5, NA))
> replacement
replacement<- within(replacement, {
    val <- ifelse (is.na(val), median (val , na.rm = TRUE ), val)
    })


11. 붓꽃 종별로 결측(NA)을 종별 중앙값으로 치환
iris_repl<-iris
> iris_repl[1, 1] = NA
> head(iris_repl)
 replasement_df <- sapply(split(iris_repl$Sepal.Length,iris_repl$Species),median, na.rm = TRUE )
iris_repl <- within(iris_repl, {
    Sepal.Length <- ifelse( is.na(Sepal.Length), replasement_df[Species], Sepal.Length )
})
replasement_df[iris_repl$Species]

12. 성별 구분 코드 값 이외 값 모두 NA로 변경하기

> dD <- data.frame(id=c("a","b","d","e","f","g"),gender=c(1,2,2,1,9,NA))

> dD
> dD$gender[!(dD$gender==1 | dD$gender==2)] <- NA


III. 중복 관리 : 케이스 및 속성                                
1. 중복된 행 식별하기
> set.seed(123)
> ss <- round(rnorm(20, 10, 5))
> ss
> duplicated(ss)
> ss[duplicated(ss)]
 unique(ss)
ss[!duplicated(ss)]
2. DataFrame에서 모든 속성값 동일한 행 삭제하기
 tmp2<- expand.grid(letters[1:2], 1:3)
tmp2[3,2]<-1
tmp2[!duplicated(tmp2),]

3. 중복이 하나라도 있는지 확인
> x <- c(9:20, 1:5, 3:7, 0:8)
> x
[1]  9 10 11 12 13 14 15 16 17 18 19 20  1  2  3  4  5  3  4  5  6  7  0  1  2  3  4  5  6
[30]  7  8

출력결과서 18은 18번째 데이터가 중복이 있다라는 표시이다.
> anyDuplicated(x)
[1] 18
anyDuplicated(iris)

4. 두 Object가 동일한지 확인
> library(reshape2)

> smiths
 (m <- melt(id =1:2 , smiths ))
 (x <- dcast (m, subject + time ~ ... ))
identical (x, smiths )



IV. 정렬 및 그룹화                                           
1. 순위(order)와 정렬(sort) 비교 
> vScore <- c( 30, 10, 40, 50, 20, 20, 10, 20, 50)
> fScore <- ordered(vScore)
> ordered(fScore)
order(vScore)
vScore[order(vScore)]

vScore_d <- c( 30, 10, 40, 50, 20, 20, 10, 20, 50)

> order(vScore_d)

2. 여러 개의 정렬기준 설정 : 붓꽃 종류 데이터에 Petal.Length 속성과 
Petal.Width 속성기준으로 정렬하기
(iris[order(iris$Petal.Length,iris$Petal.Width), ])

library(doBy)
> orderBy(~ Sepal.Width , iris)
orderBy(~ Species + Sepal.Width , iris)
orderBy(~ Species + Sepal.Width+Petal.Length , iris)

3. 영화 데이터 읽어와서 영화명을 기준으로 정렬하기
ordbytable<-read.csv("D:/0-R/orderbytable.csv",header=T)
ordedtable<-ordbytable[order(ordbytable$영화명),]
>write.csv(ordedtable,“ordedtable.csv”)

4. 붓꽃 종류 데이터에서 정렬 기준 속성 찾기
iris[order(iris$Sepal.Width,iris$Sepal.Length), ]
 iris[order(iris$Petal.Length,iris$Petal.Width), ]
pairs(iris[,1:4],col=as.integer(factor(iris$Species)))

5. 그룹화 – airquality 데이터
 library(reshape2)
> airquality
names(airquality) <- tolower(names(airquality))
aqm <- melt(airquality, id=c("month", "day"), na.rm=TRUE)
>acast(aqm, variable ~ month, mean, subset = .(variable == "ozone"))
acast(aqm, variable ~ month, mean, subset = .(variable == "solar.r"))
            5        6        7        8        9
solar.r 181.2963 190.1667 216.4839 171.8571 167.4333


# wind의 월별 평균
> acast(aqm, variable ~ month, mean, subset = .(variable == "wind"))
5        6        7        8     9
wind 11.62258 10.26667 8.941935 8.793548 10.18


# temp의 월별 평균
> acast(aqm, variable ~ month, mean, subset = .(variable == "temp"))


6. 그룹화 따라하기 : 팁을 가장 많이 지불한 그룹 고객 특성
str(tips)
summary(tips)
melt(tips)
dcast(melt(tips), sex ~ smoker, mean, subset = .(variable == "total_bill"))
dcast(melt(tips), sex ~ smoker, mean, subset = .(variable == "tip"))


7. 동물 몸무게데이터셋에 따라  동물 종데이터셋 정렬하기 
>vAnimal <- c("Goat","Horse","Rabbit","Dog")
＞vWeight <- c(60, 550, 3, 10)                  
＞order(vWeight)
vAnimal[order(vWeight)]


V. 데이터 필터링                                   
1. 데이터 타입별로 속성 추출하기
sapply(iris,is.numeric)
 sapply(iris,is.factor)
sapply(iris,is.character)
risSa<- iris[,sapply(iris,is.numeric)] 
risSanotnum<- iris[,!(sapply(iris,is.numeric))] 
> risSanotnum
iris_fact<-iris[,sapply(iris,is.factor)]  
2. 수치형 타입의 컬럼에만 추출해서 집계(summary)하기
 lapply(chickwts, is.numeric)    
sapply(chickwts, is.numeric)
sapply(chickwts, mean)      

3. 조건과 매칭되는 첫 번째 위치탐색하기
>vv1<-c(10,20,30,40)
>vX1 <- c(20,30)
> match(vv1,vX1)
>  vv<-c(10,20,30,20,40)
> match(20,vv)
dX <- data.frame(id=c(1,1,2,15), value=c(NA,NA,NA,NA))

> dLookup <- data.frame(id=c(0,1,2,2,3), value=c(100,101,112,102,104))

4. 조건에 매칭되는 케이스 추출하기
which(mtcars$wt > 5 )    
 which( mtcars$wt > 5 ,arr.ind = T)

5. 여러개의 조건 설정하여 일치하는 데이터 추출하기
 mtcars$wt>5 | mtcars$mpg<12
which(mtcars$wt>5| mtcars$mpg<12)
>mtcars_wt5u_mpg12d<-mtcars[mtcars$wt>5 | mtcars$mpg<12,]
>write.csv(mtcars_wt5u_mpg12d,“파일명.csv”)
> mtcars[mtcars$wt>5 | mtcars$mpg<12,]
which(mtcars$wt>5 & mtcars$mpg<12)

6. 조건에 일치하는 subset 추출하기
subset(mtcars, mtcars$wt>5 | mtcars$mpg<12)



7.data.table에서 조건에 적합한 케이스 추출하기
>library(hflights)
> str(hflights)
> hflights_dataframe<-hflights
> library(dplyr)
> head(filter(hflights_dataframe, Month == 1, DayofMonth == 1))

8. 인종 level중에 백인과 흑인만 추출해서 시각화하기
>library(boot)
>adult <- read.csv("e:/00-R/biz_08adult.data", header = FALSE, strip.white = TRUE)
>names(adult) <- c('age', 'workclass', 'fnlwgt', 'education',
'education_num', 'marital_status', 'occupation',
'relationship', 'race', 'sex',
'capital_gain', 'capital_loss',
'hours_per_week', 'native_country',
'wage')
> library(tibble)
> glimpse(adult)

9. 두 데이터를 비교해서 존재하지 않는 항목 탐색하기
vv1<-c(10,20,30,40)
> vX1 <- c(20,30)
> vv1%in% vX1

> vv1<-c(10,20,30,40)
> vX1 <- c(20,30)
> !(vv1%in% vX1)


10. 짝수 위치 탐색하기
(1:12)%%2 == 0
which((1:12)%%2 == 0) 
which((1:12)%%2 == 0,arr.ind = TRUE)

VI. 파생 변수                   
1. 파생변수 이해 및 관련 함수
2. transform()과 mutate() 비교 따라하기

> data("airquality")
> names(airquality)
[1] "Ozone"   "Solar.R" "Wind"    "Temp"    "Month"   "Day"  

> library(plyr)
> head(mutate(airquality, Ozone = log(Ozone)))
head(mutate(airquality, new = mean(airquality$Temp), new2 = (Temp- new) / sd(airquality$Temp)) )

3. 온도와 오존값 변환하기
airquality_derived<-mutate(airquality, ozone_log = log(Ozone), Temp_nomal = (Temp - mean(airquality$Temp)) /sd(airquality$Temp))

airquality_derived_t<-transform(airquality, ozone_log = log(Ozone), Temp_nomal = (Temp - mean(airquality$Temp)) /sd(airquality$Temp))

4. 비행 주행 빠르기 정도를 나타내는 파생 변수 생성하기
library(hflights)
> str(hflights)
 library(dplyr)
>  hflights_df <- tbl_df(hflights)
> hf_df_deri<-mutate(hflights_df, Flig_Level = ArrDelay - DepDelay)
> hf_df_deri[1:10,c(1:2,5,8,12:15,21:22)]
5. 비율척도에서 서열척도 파생변수 생성
vAge <- 51:80
> table(cut(vAge, breaks=c(0,55,60,65,70,99), include.lowest=TRUE, right=FALSE))


6. 차량 연비를 낮음, 중간, 높음로 파생변수 생성하기
vMPG <- mtcars$mpg
> vMPG
> cut(vMPG, breaks=c(0,20,30,50), labels=c("low","medium","high"))
>VMPG$cut_level<-cut(vMPG, breaks=c(0,20,30,50), labels=c("low","medium","high"))

7. 야구 선수 아이디별로 데뷔 차수를 나타내는 파생 변수 생성하기
>library(plyr)
> head(baseball,10)
>ball<-baseball

> head (ddply(ball , .(id), mutate ,
+             debut = year - min(year) + 1, log_debut =log(debut)),10)


8. 조건의 일치하는지 여부를 나타내는 파생변수 생성 
> library(plyr)

> ddply (iris,.( Species , Sepal.Length>5.0),
                                     function (sub) {
                                       data.frame ( sepal.width.mean = mean (sub$ Sepal.Width ))
                                     })


9. Sepal.Length 가 5.0이상이고 Species가 Setosa인지 여부로 파생변
수 생성하기 
> library(plyr)
> head(adply(iris[,1:4], 1, function(x) { row_mean=rowMeans(x) }))


VIII. 리모델링   
1. 변수명이 level이 되게 리모델링
> mcex <- data.frame ( medicine =c("a", "b", "c"),ctl=c(5, 3, 2),exp=c(4, 5, 7))

> mcex
medicine ctl exp
1        a   5   4
2        b   3   5
3        c   2   7

> library(doBy)

> summaryBy(values ~ ind ,stacked_mcex)
ind values.mean
1 ctl    3.333333
2 exp    5.333333

unstack(stacked_mcex, values ~ ind )


2. 3점 척도로 구성된 질문 응답 데이터, 리모델링하기
be<-read.csv("c:/00-R/behavior.csv")
be_q_ac<-stack(be_q)
xtabs(~values+ind,be_q_ac)

3. 속성 내 값을 다른 속성 값과 일대일 대응관계로 확장하기
require(utils)

expand.grid(height = seq(60, 80, 5),              weight = seq(100, 300, 50),
+             sex = c("Male","Female"))

4. 닭을 살찌우면서 안전한 닭의 모이는 무엇인가
str(ChickWeight)
names(ChickWeight) <- tolower(names(ChickWeight))
library(reshape2)
>chick_m <- melt(ChickWeight, id=2:4, na.rm=TRUE)
dcast(chick_m, time ~ variable, mean) 
>library(reshape2)
>chick_m <- melt(ChickWeight, id=2:4, na.rm=TRUE)
>dcast(chick_m, diet ~ variable, mean) 
aggregate(ChickWeight$weight,by=list(ChickWeight$Diet),mean, na.rm=TRUE)
>library(data.table)
> DT_CW <- as.data.table(ChickWeight)
> DT_CW[, mean(weight), by="Diet"]
>names(ChickWeight) <- tolower(names(ChickWeight))
> chick_m <- melt(ChickWeight, id=2:4, na.rm=TRUE)
(6)  time마다의 chicks의 마리수 

각 시간의 실험을 한 닭의 마리수가 몇 마리인지 확인하여 실험닭의 마리 수가 균형이 있엇는지 확인한다. acast()함수에 length를 사용한다.

>names(ChickWeight) <- tolower(names(ChickWeight))
> chick_m <- melt(ChickWeight, id=2:4, na.rm=TRUE)






5. 영화 장르 리모델링 따라하기


IX. 요약변수                                 
1. 요약 변수의 이해와 생성하기
2. 월별로 평균을 산출
3. 요약 값 계산시 NA 연산 불가 메세지 해결


