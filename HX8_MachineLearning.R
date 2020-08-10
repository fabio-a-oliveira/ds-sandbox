# Housekeeping -----------------------------------------------------------------

rm(list = ls())
gc()

library('tidyverse')
library('dslabs')
library('caret')
library('lubridate')

# Assessment -------------------------------------------------------------------

data(heights)
str(heights)
heights[777,]

heights %>%
  summarize(max_height = max(height),
            shortest = which.min(height),
            mean = mean(height),
            median = median(height),
            proportion_males = mean(sex == "Male"),
            taller = sum(height > 78 & sex == "Female"))

# Section 2.1 ------------------------------------------------------------------

# evaluation of heights database

data(heights)

list <- createDataPartition(heights$height, times = 2, p = .01, list = FALSE)

heights[list[,1],]
heights[list[,2],]

heights %>%
  mutate(height = round(height,0)) %>%
  group_by(height,sex) %>%
  count(height) %>%
  spread(key = sex, value = n, fill = 0) %>%
  transmute(height = height,
            males = Male/(Male+Female),
            females = Female/(Male+Female)) %>%
  gather(key='sex', value = 'proportion', males:females) %>%
  ggplot(aes(x = height, y = proportion, color = sex)) +
  geom_line()

list <- createDataPartition(heights$height,times=1,p=.05,list=FALSE)
test_set <- heights[list,]
train_set <- heights[-list,]

F1 <- sapply(min(heights$height):max(heights$height), function(x){
  y_hat <-  ifelse(heights$height > x, 'Male', 'Female') %>% factor()
  y_actual <-  heights$sex
  F_meas(data = y_hat, reference = y_actual)
})


scores <- 
  data.frame(strata = min(heights$height):max(heights$height),
             F1_score = F1)

scores %>%
  summarise(best_cutoff = strata[which.max(F1_score)],
            F1_score = F1_score[which.max(F1_score)])

y_hat <- ifelse(heights$height > 66, "Male", "Female") %>% factor()
y_actual <- heights$sex

se <- sensitivity(data = y_hat, reference = y_actual)
sp <- specificity(data = y_hat, reference = y_actual)
pr <- precision(data = y_hat, reference = y_actual)
F1 <- F_meas(data = y_hat, reference = y_actual)

2/(1/se + 1/pr) # coincides with F1 = geom mean sensitivity and precision
2/(1/se + 1/sp)

# Assessment - heights

data(reported_heights)

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type

model <- 
  data.frame(actual_sex = y, type = x)

model %>%
  mutate(predicted_sex = ifelse(type == "inclass", "Female", "Male")) %>%
  summarise(accuracy = mean(actual_sex == predicted_sex))
  
y <- model$actual_sex
y_hat <- ifelse(model$type == "inclass", "Female", "Male") %>% factor()

table(y_hat, y)
sensitivity(data = y_hat, reference = y)
specificity(data = y_hat, reference = y)

prevalence <- 
  dat %>%
  summarise(female_prevalence = mean(sex=="Female"))

# Assessment - iris dataset

data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species

set.seed(2, sample.kind="Rounding")
list <- createDataPartition(y,times=1,p=.5,list=FALSE)
test <- iris[list,]
train <- iris[-list,]

range <- 
  iris %>%
  select(-Species) %>%
  summary()

cutoff <- seq(1,7.9,.1)

tmp <- sapply(cutoff,function(x){
  tmp <- data.frame(y_hat_sl = ifelse(train$Sepal.Length > x,"virginica","versicolor"),
                    y_hat_sw = ifelse(train$Sepal.Width > x,"virginica","versicolor"),
                    y_hat_pl = ifelse(train$Petal.Length > x,"virginica","versicolor"),
                    y_hat_pw = ifelse(train$Petal.Width > x,"virginica","versicolor"),
                    y_actual = train$Species) %>%
    summarize(cutoff = x,
              accuracy_sl = mean(y_hat_sl == y_actual),
              accuracy_sw = mean(y_hat_sw == y_actual),
              accuracy_pl = mean(y_hat_pl == y_actual),
              accuracy_pw = mean(y_hat_pw == y_actual))
})

accuracy <-
  data.frame(cutoff = as.numeric(tmp[1,]),
             accuracy_sl = as.numeric(tmp[2,]),
             accuracy_sw = as.numeric(tmp[3,]),
             accuracy_pl = as.numeric(tmp[4,]),
             accuracy_pw = as.numeric(tmp[5,]))

accuracy %>%
  summary()

accuracy %>%
  gather(key = 'feature', value = 'measurement', -cutoff) %>%
  ggplot(aes(x = cutoff, y = measurement, color = feature)) +
  geom_line()

y_hat = ifelse(test$Petal.Length > 4.7, "virginica", "versicolor") %>% as.character()
y_actual = test$Species %>% as.character()

mean(y_hat == y_actual)


# tmp <- sapply(cutoff,function(x){
#   tmp <- data.frame(y_hat_sl = ifelse(test$Sepal.Length > x,"virginica","versicolor"),
#                     y_hat_sw = ifelse(test$Sepal.Width > x,"virginica","versicolor"),
#                     y_hat_pl = ifelse(test$Petal.Length > x,"virginica","versicolor"),
#                     y_hat_pw = ifelse(test$Petal.Width > x,"virginica","versicolor"),
#                     y_actual = test$Species) %>%
#     summarize(cutoff = x,
#               accuracy_sl = mean(y_hat_sl == y_actual),
#               accuracy_sw = mean(y_hat_sw == y_actual),
#               accuracy_pl = mean(y_hat_pl == y_actual),
#               accuracy_pw = mean(y_hat_pw == y_actual))
# })
# 
# accuracy <-
#   data.frame(cutoff = as.numeric(tmp[1,]),
#              accuracy_sl = as.numeric(tmp[2,]),
#              accuracy_sw = as.numeric(tmp[3,]),
#              accuracy_pl = as.numeric(tmp[4,]),
#              accuracy_pw = as.numeric(tmp[5,]))
# 
# accuracy %>%
#   summary()

plot(iris,pch=21,bg=iris$Species)



cutoff.pl <- accuracy$cutoff[which.max(accuracy$accuracy_pl)]
cutoff.pw <- accuracy$cutoff[which.max(accuracy$accuracy_pw)]

y_actual <- test$Species %>% droplevels()
y_hat <- ifelse(test$Petal.Length > cutoff.pl | test$Petal.Width > cutoff.pw,
                "virginica","versicolor")

mean(y_actual == y_hat)















