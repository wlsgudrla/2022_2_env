dat <- iris
head(dat)
str(dat)
# 2 statisics
min(dat$Sepal.Length)
max(dat$Sepal.Length)

#use fuction
range2 <- function(x) {
  range <- max(x)-min(x)
  return(range)
}
range2(dat$Sepal.Length)

install.packages("pastecs")
library(pastecs)
stat.desc(dat, norm = TRUE)

#PLOT
hist(dat$Sepal.Length)
boxplot(dat$Sepal.Length)
boxplot(dat$Sepal.Length ~ dat$Species)

install.packages("car")
library(car)
qqPlot(dat$Sepal.Length)

install.packages("ggplot2")

library(ggplot2)
ggplot(dat) +
  aes(x = Sepal.Length) +
  geom_histogram(bins = 20)

ggplot(dat) +
  aes(x = Sepal.Length, y = Petal.Length, colour = Species) +
  geom_point() +
  scale_color_hue()


dat <- read.csv(file = "C:/Users/user/Desktop/R studio/Rawdata/jooam.csv")
head(dat)
str(dat)

min(dat$DO)
max(dat$DO)
range(dat$DO)

range2 <- function(x) {
  range <- max(x) - min(x)
  return(range)
}
range2(dat$DO)

mean(dat$DO, na.rm = TRUE) 
median(dat$DO)
quantile(dat$DO, 0.5)
IQR(dat$DO)
temp <- quantile(dat$DO, 0.75)-quantile(dat$DO, 0.25)
temp

sd(dat$DO)
var(dat$DO)

sapply(dat[, 1:4], sd)
lapply(dat[, 1:4], sd)

summary(dat)
dc_dat <- read.csv("C:/Users/user/Desktop/R studio/Rawdata/DC_2020.csv", na = "-", fileEncoding = "CP949", encoding = "UTF-8")
summary(dc_dat)

by(dc_dat[, 6:10], dc_dat$Depth, summary)

by(dc_dat[, 6:10], dc_dat$Site, summary)


library(pastecs)
stat.desc(dat, norm = TRUE)

tab <- table(dat$DO)
sort(tab, decreasing = TRUE)


hist(dat$DO)
boxplot(dat$DO)
boxplot(dc_dat$Chl.a ~ dc_dat$Site)
boxplot(dc_dat$Chl.a ~ dc_dat$Depth)
plot(dat$DO,type = "l") # "l" for line
plot(density(dat$DO))

qqnorm(dat$DO)

qqline(dat$DO)

library(car)
qqPlot(dat$DO)

library(ggplot2)
library(ggpubr)

install.packages("ggpubr")
ggqqplot(dat$DO)


library(ggplot2)

head(dat)

ggplot(dat) +
  aes(x = COD) +
  geom_histogram(bins = 30)

ggplot(dat) +
  aes(x = COD, y = BOD) +
  geom_boxplot()

ggplot(dat) +
  aes(x = COD, y = TOC) +
  geom_point()

ggplot(dat) +
  aes(x = COD, y = DO, colour = TN) +
  geom_point() +
  scale_color_hue()

ggplot(dat) +
  aes(x = COD) +
  geom_density()


#3주차
# ctrl+l = console clean
# 변수설정 = 미리 선언하여 할당한다.
# 변수값 할당 <- 

mean(x<-c(1,2,3))
x
mean(x=c(4,5,6))
x
mean(z=c(4,5,6))

#NA = NOT available
#null = 변수가 초기화 되지 않았을때
#Character = char
#factor = 범주
#대중소 = ordinary

#벡터
#c() 생성, name() 이름부여
#seq 연속된 숫자

#list 생성

#행렬 matrix

#Data 형식 확인
#함수 - 검색

#as.factor - classification


#실습
pellet <- c('red','green','blue')
print(pellet)
print(class(pellet))


#byrow = true 행을 먼저 채움

install.packages("matlab")


ls()


fibo <- function(n){
  if(n==1 || n==2) {
    return(1)
  }
  return(fibo(n-1)+fibo(n-2))
}
fibo(10)
fibo(50)

#재폭기(re airation) 상수-유속과 수심

#실습
# Lecture 4. R programming

#1) 조건문
## if-else
x <- 15
if(x > 10) {
  y <- x*5
} else {
  y <- x/5
}
print(y)  

## ifelse 
X <- c(1,2,4,5,6)
ifelse(X%%2==0, "even", "odd")

## if-else if
x <-50 
if (x>=90){
  grade = 'A'
} else if (x>=80){
  grade='B'
} else if (x>=70){
  grade='c'
} else {
  grade='D'
  print(grade)
}

for (x1 in c(50, 70, 100, 95, 85)) {
  if (x1>=90) {
    grade = 'A'
  } else if (x1>=80){
    grade='B'
  } else if (x1>=70){
    grade='c'
  } else {
    grade='D'
  }
  print(grade)
}

client<-"public"
net.price<-100
if(client=='private'){
  tot.price <- net.price * 1.12
} else if(client=='public'){
  tot.price <- net.price * 1.06
} else {
  tot.price <- net.price
}
print(tot.price)

#2) 반복문

## for
for (i in 1:7) {
  print(i^2)
}

x <-1:7
Looplees <- x^2        # R은 loop 사용없이 반복적 계산 용이
Looplees

Storage <- numeric(5)
for (i in 1:5){
  Storage[i] <- i^2
}
Storage
mean(Storage)

x <- c(-3,6,2,5,9)
storage1 <- numeric(5)
storage1
for (i in 1:5) {
  storage1[i]<-(x[i])^2
}
storage1

# vector에 대한 if 문 사용시 주의사항
Temp <- c(-4, 5, 10, -6, -40, 30)
if(Temp > 0) {
  print("warm")
} else {
  print("not so warm")
}

for(Temp in c(-4, 5, 10, -6, -40, 30)){
  if(Temp > 0) {
    print("warm")
  } else {
    print("not so warm")
  }  
}

# nested
for (i in 1:3){
  for (j in 1:2){
    print (i+j)
  }  #2
} #1

## while
i<-0
while(i<=9) {
  i<-i+1
  if(i%%2!=0) {
    next       #odd이면 프린터하지 않고 다음 숫자로 감
  }
  print(i)  
}

## repeat
i<-1
repeat {
  print(i)
  if(i>=10) {
    break       #i가 10보다 크거가 같으면 출력 중단
  }
  i<-i+1
}

Age<-c(56,34,67,33,25,28)
Weight<-c(78,67,56,44,56,89)
Height<-c(165, 171,167,167,166,181)
BMI_df<-data.frame(Age,Weight,Height)
BMI_df

# row wise sum up of dataframe using apply function in R
apply(BMI_df,1,sum)

# column wise sum up of dataframe using apply function in R
apply(BMI_df,2,sum)

# column wise mean of dataframe using apply function in R
apply(BMI_df,2,mean)

apply(BMI_df,2,min)


lapply(BMI_df, function(BMI_df) BMI_df/2)
lapply(BMI_df, mean)


result<-sapply(BMI_df, function(BMI_df) BMI_df/2)
result
sapply(BMI_df, mean)
random <- c("This", "is", "random",  "vector")
sapply(random,nchar)

attach(iris)
# mean sepal length by species 종별로 평균
tapply(iris$Sepal.Length, Species, mean)


OConner <- function (vel, dep) 
{
  ka<-(3.93 * (vel^0.5))/(dep^1.5)
  return(data.frame(vel, dep, ka))
}
U <- seq(0.2, 1.0, 0.2)
OConner(vel=U,dep=1)

#전역변수, 지역변수
#지역변수가 우선이다.

#함수외 변수, 함수내 변수 기준으로 함수내 변수가 우선임

n <- 1
f <- function() {
  print (n)
}
f()

n <- 1
f <- function() {
  print (n)
}
f()


f <- function() {
  a <- 1
  g <- function() {
    a <- 2
    print(a)
  }
  g()
  print(a)     #내부 블록 g함수에서 a를 2로 지정하려 했으나, 외부블록 f함수의 a=1로 유지
}
f()

f <- function() {
  a <- 1
  g <- function() {
    a <<- 2
    b <<- 2
    print(a)
    print(b)
  }
  g()
  print(a)     #내부 블록 g함수에서 <<-로 지정한 변수는 전역에 적용됨
  print(b)
}
f()


#grammer of grapics plot 2nd gen.

dat <- read.csv(file = "C:/Users/user/Desktop/R studio/Rawdata/jooam.csv")
head(dat)
str(dat)

#BASE 
x <- dat$COD
h<-hist(x, breaks = 10, xlab="COD", main = "Jooam Reservoir")

hist(x, breaks = 10, col="grey", border = "white",
    probability = TRUE,  xlab="COD", ylim = c(0,1.2))
lines =(density(x))


x <- dat$Chro
h<-hist(x, breaks = 30, xlab="Chro", main = "Jooam Reservoir")

hist(x, breaks =30, col="grey", border = "white",
     probability = TRUE,  xlab="Chro", ylim = c(0,1.2))
lines(density(dat$Chro))



dat <- read.csv(file = "C:/Users/user/Desktop/R studio/Rawdata/jooam.csv")
head(dat)
str(dat)
attach(dat)
plot(BOD, COD, main = "scatterplot", xlab="BOD",
      ylab = "COD", pch=20)

#install,packages("scatterplot3d")
install.packages("scatterplot3d")
library(scatterplot3d)
x <- dat$BOD
y <- dat$COD
z <- dat$Chro
with(dat, {
  scatterplot3d(x=BOD,
                y=COD,
                Z=Chro,
                main="3D Scatterplot")
})


