


# Step 1 ??? collecting data
## download the jooam.csv file from the Packt Publishing website
## https://github.com/PacktPublishing/Machine-Learning-with-R-Second-Edition/blob/master/Chapter%2007/jooam.csv

# Step 2 exploring and preparing the data
jooam <- read.csv(file = "C:/Users/user/Desktop/R studio/Rawdata/jooam.csv")
str(jooam)

## use normalize() for normalize data
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

jooam_norm <- as.data.frame(lapply(jooam, normalize))
summary(jooam_norm$chloro)
summary(jooam$chloro)

## partition the data into a training set(75%) and a testing set(25%)
jooam_train <- jooam_norm[1:562, ]
jooam_test <- jooam_norm[563:749, ]


# Step 3 ??? training a model on the data
# install.packages("neuralnet")
install.packages("neuralnet")
library(neuralnet)
jooam_model <- neuralnet(chloro ~ COD + BOD 
                            + DO + TN + TOD + TOC, 
                            data = jooam_train)
plot(jooam_model)

# Step 4 ??? evaluating model performance
model_results <- compute(jooam_model, jooam_test[1:6])
predicted_chloro <- model_results$net.result
cor(predicted_chloro, jooam_test$chloro)
plot(predicted_chloro, jooam_test$chloro)


# Step 5 improving model performance by adding a hindden layer with 5 units
jooam_model2 <- neuralnet(chloro ~ COD + BOD 
                          + DO + TN + TOD + TOC, 
                          data = jooam_train, hidden = 5)
plot(jooam_model2)

model_results2 <- compute(jooam_model2, jooam_test[1:6])
predicted_chloro2 <- model_results2$net.result
cor(predicted_chloro2, jooam_test$chloro)
plot(predicted_chloro2, jooam_test$chloro)

# Extra work: Conventional Multiple Linear Regression  
MLR <-lm(chloro ~ COD + BOD 
         + DO + TN + TOD + TOC, 
         data = jooam_train)
summary(MLR)
MLR_prediected=predict(MLR, newdata = jooam_test[1:6])
cor(MLR_prediected, jooam_test$chloro)
plot(MLR_prediected, jooam_test$chloro)


# Paldang Chl-a
data<-read.csv("Paldang.csv", header=TRUE, na.strings = "NA")
data_norm <- as.data.frame(lapply(data, normalize))

## partition the data into a training set(75%) and a testing set(25%)
Chla_ann <- neuralnet(Chla ~ .,
                      data = data_norm, hidden = 5)
plot(Chla_ann)

model_results3 <- compute(Chla_ann, data_norm[1:14])
predicted_Chla <- model_results3$net.result
cor(predicted_Chla, data_norm$Chla)
plot(predicted_Chla, data_norm$Chla)
Footer
© 2022 GitHub, Inc.
Footer navigation
Terms
Privacy
Security
Status
Docs
Contact GitHub
Pricing
API
Training
Blog
About
lecture_2022_Fall/Lec_10_Linear Regression using ANN.R at master · lydia126/lecture_2022_Fall · GitHub