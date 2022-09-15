dat <- iris
head(dat)
str(dat)
# 2 statisics
min(dat$Sepal.Length)
max(dat$Sepal.Length)

#use fuction
range2 <- fuction(x) {
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
qqplot(dat$Sepal.Length)
