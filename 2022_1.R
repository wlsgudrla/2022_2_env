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
