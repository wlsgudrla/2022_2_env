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

