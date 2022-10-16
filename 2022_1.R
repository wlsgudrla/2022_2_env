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



#7week practice

# Lecture 7. Hypothesis tests and ANNOVA
#1-1. 일표본 평균 검정: t.test()
x<-rnorm(30, mean=0, sd=1)   # 평균 0, 표준편차 1인 정규분포 무작위 30개 추출
par(mar=c(2,2,2,2))
hist(x, main="Random draws from Std Normal", cex.axis=.8, xlim=c(-4,4))
t.test(x, mu=0)    # 귀무가설 모집단 평균 u=0

set.seed(3000)
xseq<-seq(-4,4,.01)
densities<-dnorm(xseq, 0,1)
plot(xseq, densities, col="darkgreen", xlab="", ylab="Density", 
     type="l", cex=2, cex.axis=1.5)
t.test(xseq)    # 귀무가설 모집단 평균 u=0

x<-rnorm(30, mean=10, sd=1)
t.test(x)    # 귀무가설 모집단 평균 u=0
t.test(x, mu=10)    # 귀무가설 모집단 평균 u=10

dat <- read.csv(file = "C:/Users/user/Desktop/R studio/Rawdata/jooam.csv")
t.test(dat$COD, mu=mean(dat$COD))
t.test(dat$COD, mu=7.5)
result <- t.test(dat$COD, mu=7.5)
# printing the p-value
result$p.value
# printing the mean
result$estimate
# printing the confidence interval
result$conf.int

#1-2. 비모수 일표본 평균 검정: One-Sample Wilcoxon Signed Rank Test in R 
dat <- read.csv(file = "C:/Users/user/Desktop/R studio/Rawdata/jooam.csv")
summary(dat$TP)
shapiro.test(dat$TP)   # 정규성 확인 p > 0.05이면 정규분포, p < 0.05 비정규분포
wilcox.test(dat$TP, mu = 16.0, alternative = "two.sided")  # "equal" or "not equal"
wilcox.test(dat$TP, mu = 16.0, alternative = "less")

#2. 독립(unpaird) 이표본 평균 검정: t.test()
?sleep   #t.test 예제, 약물 그룹별 환자(ID)의 수면 증가량
sleep2<-sleep[,-3]  # ID 생략하고 약물 그룹별 효과 차이 검정
library("ggpubr")
ggboxplot(sleep2, x = "group", y = "extra",
          color = "group", palette = c("#00AFBB", "#E7B800"),
          ylab = "extra", xlab = "group")  
tapply(sleep2$extra, sleep2$group, mean)   # 그룹으로 나누어 수면증가시간 평균값 구함
var.test(extra~group, sleep2)   # 그룹간 수면증가시간의 분산 차이 검정
t.test(extra~group, data=sleep2, paired=F, var.equal=T)  #독립 이표본, 등분산 평균 검정

#3. 짝지은(paired) 이표본 평균 검정: t.test()
with(sleep, t.test(extra[group==1], extra[group==2], paired = T))
with(sleep, t.test(extra~group, paired = T, var.equal=T))

#4. 이표본 분산
with(iris, var.test(Sepal.Width, Sepal.Length)) 

#5. 이표본 t-test (Parametric statistics)
# using under the assumption that both samples are random, independent, and come from 
# normally distributed population with unknow but equal variances
a = c(175, 168, 168, 190, 156, 181, 182, 175, 174, 179)
b = c(185, 169, 173, 173, 188, 186, 175, 174, 179, 180)
mean(a)
mean(b)
shapiro.test(a)
shapiro.test(b)
var.test(a,b)
t.test(a,b, var.equal=TRUE, paired=FALSE)

#6. 이표본 Mann-Whitney-Wilcoxon Test (Nonparametric statistics, 비모수 통계)
# data: mtcars
# the gas mileage data (mpg) for manual and automatic transmissions (am) are independent?
head(mtcars)    # mpg is numeric, am is factor
# Problem: decide at a=0.05 significance level if the gas mileage data of manual and 
# automatic transmissions in mtcars have identical data distribution. 
wilcox.test(mpg ~ am, data=mtcars) 
# answer: At a=0.05 significance level, we conclude that the gas mileage data of manual 
# and automatic transmissions in mtcar are nonidentical populations. 

#7. 다중표본 Kruskal-Wallis Test
# data: airquality
head(airquality)
# problem: Without assuming the data to have normal distribution, test at a=0.05 
# if the monthly ozone density has identical distributions from May to September. 
kruskal.test(Ozone ~ Month, data = airquality) 
# Answer: At a=0.05 significance level, we conclude that the monthly ozone density 
# from May to September 1973 are nonidentical populations. 
airquality$Month<-as.factor(airquality$Month)
str(airquality)
res.aov <- aov(Ozone ~ Month, data = airquality)
summary(res.aov)
TukeyHSD(res.aov)
plot(TukeyHSD(res.aov))

#8. One-way ANOVA (ANalysis Of VAriance)
## Factor data로 그룹화 된 three-samples에 대한 t-test
## Null hypothesis: the means of the different groups are the same
## Alternative hypothesis: At least one sample mean is not equal to the others.
my_data <- PlantGrowth
# Show a random sample
set.seed(1234)
dplyr::sample_n(my_data, 10)
# Show the levels
levels(my_data$group)
my_data$group <- ordered(my_data$group,
                         levels = c("ctrl", "trt1", "trt2"))
library(dplyr)
group_by(my_data, group) %>%
  summarise(
    count = n(),
    mean = mean(weight, na.rm = TRUE),
    sd = sd(weight, na.rm = TRUE)
  )
library("ggpubr")
ggboxplot(my_data, x = "group", y = "weight",
          color = "group", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("ctrl", "trt1", "trt2"),
          ylab = "Weight", xlab = "Treatment")

# Mean plots
# ++++++++++++++++++++
# Plot weight by group
# Add error bars: mean_se
# (other values include: mean_sd, mean_ci, median_iqr, ....)
ggline(my_data, x = "group", y = "weight",
       add = c("mean_se", "jitter"),
       order = c("ctrl", "trt1", "trt2"),
       ylab = "Weight", xlab = "Treatment")

# Compute the analysis of variance
res.aov <- aov(weight ~ group, data = my_data)
# Summary of the analysis
summary(res.aov)
## As the p-value is less than the significance level 0.05, we can conclude that there are
## significant differences between the groups highlighted with “*" in the model summary.  

# Tukey multiple pairwise-comparisons
## Tukey HSD (Tukey Honest Significant Differences, R function: TukeyHSD()) for performing 
##  multiple pairwise-comparison between the means of groups.
TukeyHSD(res.aov)  
# only the difference between trt2 and trt1 is significant  

# Multiple comparisons using multcomp package
#install.packages("multcomp")  
library(multcomp)
summary(glht(res.aov, linfct = mcp(group = "Tukey")))

# Pairewise t-test
pairwise.t.test(my_data$weight, 
                my_data$group,p.adjust.method = "BH") # Benjamini-Hochberg method

# Check ANOVA assumptions: test validity?
## The ANOVA test assumes that, the data are normally distributed and
## the variance across groups are homogeneous.
# Homogeneity of variances
## 1) residuals versus fits plot
plot(res.aov, 1)   # Points 17, 15, 4 are detected as outliers
## 2) use Bartlett’s test or Levene’s test to check the homogeneity of variances.
library(car)
leveneTest(weight ~ group, data = my_data)
## p > 0.05, we can assume the homogeneity of variances in the different groups

#9. Welch one-way test: ANOVA test with no assumption of equal variances
oneway.test(weight ~ group, data = my_data)
## Pairwise t-tests with no assumption of equal variances
pairwise.t.test(my_data$weight, my_data$group,
                p.adjust.method = "BH", pool.sd = FALSE)

## Check the normality assumption
## 1) Normality plot of residuals
plot(res.aov, 2)
# Extract the residuals
aov_residuals <- residuals(object = res.aov )
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals)

# 10. Kruskal-Wallis rank sum test
## use when ANOVA assumptions are not met
kruskal.test(weight ~ group, data = my_data)

# 11. Two-way ANOVA test
## evaluate simultaneously the effect of two grouping variables (A and B) on a response variable
## Two-way ANOVA test hypotheses
### 1) There is no difference in the means of factor A
### 2) There is no difference in means of factor B
### 3) There is no interaction between factors A and B
### The alternative hypothesis for cases 1) and 2) is: the means are not equal
### The alternative hypothesis for case 3) is: there is an interaction between A and B.

## (1) balanced designs - equal sample sizes within independent grouping levels
my_data <- ToothGrowth
# Show a random sample
set.seed(1234)
dplyr::sample_n(my_data, 10)
str(my_data)
# convert "dose" from numeric to factor
my_data$dose <- factor(my_data$dose,
                       levels = c(0.5, 1, 2),
                       labels = c("D0.5", "D1.0", "D2.0"))
str(my_data)
table(my_data$supp, my_data$dose)

library(dplyr)
group_by(my_data, supp, dose) %>%
  summarise(
    count = n(),
    mean = mean(len, na.rm = TRUE),
    sd = sd(len, na.rm = TRUE)
  )

## plot graphs
ggpubr::ggboxplot(my_data, x = "dose", y = "len", color = "supp",
                  palette = c("#00AFBB", "#E7B800"))

ggpubr::ggline(my_data, x = "dose", y = "len", color = "supp",
               add = c("mean_se", "dotplot"),
               palette = c("#00AFBB", "#E7B800"))

# Compute two-way ANOVA test
## We want to know if tooth length depends on supp and dose.
res.aov2 <- aov(len ~ supp + dose, data = my_data)
summary(res.aov2)

# Two-way ANOVA with interaction effect (교호효과)
# These two calls are equivalent
res.aov3 <- aov(len ~ supp * dose, data = my_data)
summary(res.aov3)
res.aov4 <- aov(len ~ supp + dose + supp:dose, data = my_data)
summary(res.aov4)

TukeyHSD(res.aov3, which = "dose")
TukeyHSD(res.aov3, which = "supp")
install.packages("multcomp")
library(multcomp)
summary(glht(res.aov2, linfct = mcp(dose = "Tukey")))

# Check the Homogeneity of variances
plot(res.aov3, 1)
leveneTest(len ~ supp*dose, data = my_data)

# Check the normality assumpttion
plot(res.aov3, 2)
# Extract the residuals
aov_residuals <- residuals(object = res.aov3)
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals )

## (2) Unbalanced designs - unequal sample sizes within independent grouping levels
### Anova() [in car package] is used to compute two-way ANOVA test for unbalanced designs
library(car)
my_anova <- aov(len ~ supp * dose, data = my_data)
Anova(my_anova, type = "III")

# 12. MANOVA Test: Multivariate Analysis of Variance - multiple response variables
my_data <- iris
## want to know if there is any significant difference, in sepal and petal length, between the different species
res.man <- manova(cbind(Sepal.Length, Petal.Length) ~ Species, data = iris)
summary(res.man)
# Look to see which differ
summary.aov(res.man)


res.man <- manova(cbind(Sepal.Length, Petal.Length, Sepal.Width, Petal.Width) ~ Species, data = iris)
summary(res.man)
# Look to see which differ
summary.aov(res.man)

