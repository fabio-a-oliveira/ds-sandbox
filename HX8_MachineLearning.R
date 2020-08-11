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

# Section 2.2 ------------------------------------------------------------------

# A = have disease
# B = test is positive

P_B_A <- .85 # sensitivity
P_A <- .02 # prevalence
P_notB_notA <- .90 # specificity
P_B <- .98*.10 + .02*.85 # positive test rate

P_A_B <- P_B_A * P_A / P_B
P_A_B

set.seed(1)
disease <- sample(c(0,1), size=1e6, replace=TRUE, prob=c(0.98,0.02))
test <- rep(NA, 1e6)
test[disease==0] <- sample(c(0,1), size=sum(disease==0), replace=TRUE, prob=c(0.90,0.10))
test[disease==1] <- sample(c(0,1), size=sum(disease==1), replace=TRUE, prob=c(0.15, 0.85))

mean(test)
mean(disease[test == 0])
mean(disease[test == 1])
mean(disease[test == 1]) / mean(disease)

# heights

data(heights)

heights %>%
  mutate(height = round(height)) %>%
  group_by(height) %>%
  summarize(p = mean(sex == "Male")) %>%
  qplot(height,p,data=.)

ps <- seq(0,1,.1)
heights %>%
  mutate(g = cut(height,quantile(height,ps),include.lowest=TRUE)) %>%
  group_by(g) %>%
  summarise(p = mean(sex == "Male"),
            height = mean(height)) %>%
  qplot(height,p,data=.)
  
Sigma <- 9*matrix(c(1,0.5,0.5,1), 2, 2)
dat <- MASS::mvrnorm(n = 10000, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

plot(dat)

ps <- seq(0, 1, 0.1)
dat %>% 
  mutate(g = cut(x,quantile(x,ps),include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarise(x = mean(x),
            y = mean(y)) %>%
  qplot(x, y, data =.)





# Section 3.1 ------------------------------------------------------------------

# Q1

set.seed(1, sample.kind="Rounding")
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

set.seed(1, sample.kind="Rounding")
RMSE <- replicate(100,{
  p <- createDataPartition(dat$y,times=1,p=.5,list=FALSE)
  train <- slice(dat,-p)
  test <- slice(dat,p)
  fit <- train %>%
    lm(y ~ x, data = .)
  test %>%
    mutate(y_hat = predict(fit,newdata=.)) %>%
    mutate(E = y-y_hat,
           SE = E^2) %>%
    summarize(MSE = mean(SE)) %>%
    mutate(RMSE = sqrt(MSE)) %>%
    .$RMSE
})
mean(RMSE)
sd(RMSE)

# Q2

RMSE <- function(n){
  
  Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
  dat <- MASS::mvrnorm(n = n, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))
  
  rmse <- replicate(100,{
    p <- createDataPartition(dat$y, times=1, p=.5, list=FALSE)
    test <- slice(dat,p)
    train <- slice(dat,-p)
    fit <- lm(data = train, y ~ x)
    y_hat <- predict(fit,test)
    y <- test$y
    rmse <- sqrt(mean((y-y_hat)^2))
  })
  
  c(mean_rmse = mean(rmse),
             sd_rmse = sd(rmse))
}

set.seed(1, sample.kind="Rounding")
n <- c(100, 500, 1000, 5000, 10000)
results <- sapply(n,RMSE)

# Q4

set.seed(1, sample.kind="Rounding")
n <- 100
Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

set.seed(1, sample.kind="Rounding")
RMSE <- replicate(100,{
  p <- createDataPartition(dat$y,times=1,p=.5,list=FALSE)
  train <- slice(dat,-p)
  test <- slice(dat,p)
  fit <- train %>%
    lm(y ~ x, data = .)
  test %>%
    mutate(y_hat = predict(fit,newdata=.)) %>%
    mutate(E = y-y_hat,
           SE = E^2) %>%
    summarize(MSE = mean(SE)) %>%
    mutate(RMSE = sqrt(MSE)) %>%
    .$RMSE
})
mean(RMSE)
sd(RMSE)

# Q6

set.seed(1)
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

cor(dat)

set.seed(1, sample.kind="Rounding")
p <- createDataPartition(dat$y, times=1, p=.5, list=FALSE)
test <- slice(dat,p)
train <- slice(dat, -p)

lm_x1 <- lm(y ~ x_1, data = train)
lm_x2 <- lm(y ~ x_2, data = train)
lm_both <- lm(y ~ x_1 + x_2, data = train)

test %>%
  mutate(y_hat_x1 = predict(lm_x1, newdata=.),
         y_hat_x2 = predict(lm_x2, newdata=.),
         y_hat_both = predict(lm_both, newdata=.)) %>%
  summarise(rmse_x1 = sqrt(mean((y-y_hat_x1)^2)),
            rmse_x2 = sqrt(mean((y-y_hat_x2)^2)),
            rmse_xboth = sqrt(mean((y-y_hat_both)^2)))

# Q8

set.seed(1)
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.95, 0.75, 0.95, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

cor(dat)

set.seed(1, sample.kind="Rounding")
p <- createDataPartition(dat$y, times=1, p=.5, list=FALSE)
test <- slice(dat,p)
train <- slice(dat, -p)

lm_x1 <- lm(y ~ x_1, data = train)
lm_x2 <- lm(y ~ x_2, data = train)
lm_both <- lm(y ~ x_1 + x_2, data = train)

test %>%
  mutate(y_hat_x1 = predict(lm_x1, newdata=.),
         y_hat_x2 = predict(lm_x2, newdata=.),
         y_hat_both = predict(lm_both, newdata=.)) %>%
  summarise(rmse_x1 = sqrt(mean((y-y_hat_x1)^2)),
            rmse_x2 = sqrt(mean((y-y_hat_x2)^2)),
            rmse_xboth = sqrt(mean((y-y_hat_both)^2)))

# Q1

set.seed(2, sample.kind="Rounding")

make_data <- function(n = 1000, p = 0.5, 
                      mu_0 = 0, mu_1 = 2, 
                      sigma_0 = 1,  sigma_1 = 1){
  
  y <- rbinom(n, 1, p)
  f_0 <- rnorm(n, mu_0, sigma_0)
  f_1 <- rnorm(n, mu_1, sigma_1)
  x <- ifelse(y == 1, f_1, f_0)
  
  test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
  
  list(train = data.frame(x = x, y = as.factor(y)) %>% slice(-test_index),
       test = data.frame(x = x, y = as.factor(y)) %>% slice(test_index))
}

dat <- make_data()

dat$train %>% ggplot(aes(x, color = y)) + geom_density()




set.seed(1, sample.kind="Rounding")

avg_1 <- seq(0,3,length.out = 25)

accuracy <- sapply(avg_1,function(avg){
  dat <- make_data(n=1000,p=.5,mu_0 = 0, mu_1 = avg, sigma_0 = 1, sigma_1 = 1)
  fit = glm(data = dat$train, y ~ x, family="binomial")
  y_hat <- ifelse(predict(fit,newdata=dat$test,type="response")>.5,1,0)
  y <- dat$test$y
  accuracy <- mean(y_hat == y)
})

plot(avg_1,accuracy)



















