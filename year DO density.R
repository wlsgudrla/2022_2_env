
dat <- read.csv(file = "C:/Users/user/Desktop/R studio/Rawdata/jooam.csv")
head(dat)
str(dat)


density2 <- ggplot(data=dat, aes(x=DO, fill=year))
density2 + geom_density(stat="density", alpha=I(0.2)) +
  xlab("DO") +  ylab("Density") + ggtitle("Histogram & Density Curve of DO")

