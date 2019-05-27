# bigDataMart

#################################################################3
#빅데이터 분석을 위한 데이터 마트 구축
#http://cafe.naver.com/dataan
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
5. 연습문제

IV. 정렬 및 그룹화                                           
1. 순위(order)와 정렬(sort) 비교 
2. 여러 개의 정렬기준 설정 : 붓꽃 종류 데이터에 Petal.Length 속성과 
Petal.Width 속성기준으로 정렬하기
3. 영화 데이터 읽어와서 영화명을 기준으로 정렬하기
4. 붓꽃 종류 데이터에서 정렬 기준 속성 찾기
5. 그룹화
6. 그룹화 따라하기 : 팁을 가장 많이 지불한 그룹 고객 특성
7. 동물의 몸무게 데이터에 따라  동물의 종 데이터를 정렬하기


V. 데이터 필터링                                   
1. 데이터 타입별로 속성 추출하기
2. 수치형 타입의 컬럼에만 추출해서 집계(summary)하기
3. 조건과 매칭되는 첫 번째 위치탐색하기
4. 조건에 매칭되는 케이스 추출하기
5. 여러 개의 조건으로 일치하는 데이터 추출하기
6. 조건에 일치하는 subset 추출하기
7. data.table에서 조건에 적합한 케이스 추출하기
8. 인종 level중에 백인과 흑인만 추출해서 시각화하기
9. 두 데이터를 비교해서 존재하지 않는 항목 탐색하기
10. 짝수 위치 탐색하기


VI. 파생 변수                   
1. 파생변수 이해 및 관련 함수
2. transform()과 mutate() 비교 따라하기
3. 온도와 오존값 변환하기
4. 비행 주행 빠르기 정도를 나타내는 파생 변수 생성하기
5. 비율척도에서 서열척도 파생변수 생성
6. 차량 연비를 낮음, 중간, 높음로 파생변수 생성하기
7. 야구 선수 아이디별로 데뷔 차수를 나타내는 파생 변수 생성하기
8. 조건의 일치하는지 여부를 나타내는 파생변수 생성 
9. Sepal.Length 가 5.0이상이고 Species가 Setosa인지 여부로 파생변
수 생성하기 


VII. 병합                        
1. 열단위로 병합
2. 행단위로 병합
3. Outer 조인 : 두 테이블
4. full Outer 조인 : 두 데이터셋에 있는 값 모두 나타나게 두 데이터
셋 합하기
5. Left outer 조인
6. Right outer 조인
7. 크로스 조인
8. Inner 조인
9. Anti 조인
10. 날짜를 기준으로 날씨 데이터 조인하기
11. 영화의 순위와 영화명 두 개 속성기준으로 조인하기

VIII. 리모델링   
1. 변수명이 level이 되게 리모델링
2. 3점 척도로 구성된 질문 응답 데이터, 리모델링하기
3. 속성 내 값을 다른 속성 값과 일대일 대응관계로 확장하기
4. 닭을 살찌우면서 안전한 닭의 모이는 무엇인가
5. 영화 장르 리모델링 따라하기
6. 연습문제

IX. 요약변수                                 
1. 요약 변수의 이해와 생성하기
2. 월별로 평균을 산출
3. 요약 값 계산시 NA 연산 불가 메세지 해결
4. 연습문제

X. 위도경도 좌표데이터 
1. 좌표에서 읍면동 변수생성하기
2. 주소를 좌표데이터로 변환

