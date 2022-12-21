library(tidyverse)
library(CLVTools)

train <- read.csv("train.csv")
test <- read.csv("test.csv")
names(train)
train %>%
  ggplot(aes(total_night_minutes,fill=churn))+
  geom_histogram()

train$churn <- as.factor(train$churn)

caret::findCorrelation(train)

model <- glm(churn~international_plan+total_day_minutes+total_day_charge, train,family="binomial")

train$predictions <- predict.glm(model, type="response")
pred <- prediction( train$predictions, train$churn)
perf <- performance(pred,"tpr")
plot(perf)
train$churn_predicted <- if_else(train$predictions>0.75,"yes","no")
measures::ACC(train$churn, train$churn_predicted)


train %>% select(number_vmail_messages, total_day_minutes,total_intl_calls) %>% cor %>%
  round(2)