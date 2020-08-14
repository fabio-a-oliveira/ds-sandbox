# Housekeeping -----------------------------------------------------------------

rm(list = ls())
gc()

library('tidyverse')
library('dslabs')
library('caret')
library('e1071')
library('lubridate')
library('purrr')
library('pdftools')
library('broom')
library('matrixStats')
library('rpart')
library('randomForest')

# install.packages("BiocManager")
library("BiocManager")
# BiocManager::install("genefilter")
library(genefilter)

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




















# Section 3.2 ------------------------------------------------------------------

# Q1

fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")
dat <- map_df(str_split(pdf_text(fn), "\n"), function(s){
  s <- str_trim(s)
  header_index <- str_which(s, "2015")[1]
  tmp <- str_split(s[header_index], "\\s+", simplify = TRUE)
  month <- tmp[1]
  header <- tmp[-1]
  tail_index  <- str_which(s, "Total")
  n <- str_count(s, "\\d+")
  out <- c(1:header_index, which(n==1), which(n>=28), tail_index:length(s))
  s[-out] %>%
    str_remove_all("[^\\d\\s]") %>%
    str_trim() %>%
    str_split_fixed("\\s+", n = 6) %>%
    .[,1:5] %>%
    as_data_frame() %>% 
    setNames(c("day", header)) %>%
    mutate(month = month,
           day = as.numeric(day)) %>%
    gather(year, deaths, -c(day, month)) %>%
    mutate(deaths = as.numeric(deaths))
}) %>%
  mutate(month = recode(month, "JAN" = 1, "FEB" = 2, "MAR" = 3, "APR" = 4, "MAY" = 5, "JUN" = 6, 
                        "JUL" = 7, "AGO" = 8, "SEP" = 9, "OCT" = 10, "NOV" = 11, "DEC" = 12)) %>%
  mutate(date = make_date(year, month, day)) %>%
  dplyr::filter(date <= "2018-05-01")

dat %>%
  ggplot(aes(date,deaths)) +
  geom_point(color="black") +
  geom_smooth(method = 'loess', color = 'red', span = .05, method.args = list(degree = 1))

fit <- loess(deaths ~ as.numeric(date), dat, span = .05, degree = 1)

dat %>%
  mutate(smooth = predict(fit,newdata=.)) %>%
  ggplot(aes(x = date, y = deaths)) +
  geom_point(color='black') +
  geom_line(aes(x = date, y = smooth), color = 'red', size = 2)

# Q2

dat %>%
  mutate(day_of_year = yday(date),
         smooth = predict(fit,newdata = .)) %>%
  ggplot(aes(x=day_of_year, y = smooth, color = year)) +
  geom_line()

# Q3

mnist_27$train %>% glm(y ~ x_2, family = "binomial", data = .) %>% tidy()

dat <- mnist_27$train

max(dat$x_2)
min(dat$x_2)

dat %>%
  ggplot(aes(x=x_2,color=y)) +
  geom_density()

qplot(x_2, y, data = mnist_27$train)

fit <- loess(as.numeric(y) ~ x_2, dat, degree = 1, span = .1)

dat %>%
  mutate(smooth = predict(fit,.)) %>%
  ggplot(aes(x=x_2,y=as.numeric(y)-1)) +
  geom_point(color = 'black') +
  geom_line(aes(x=x_2,y=smooth-1), color = 'red') +
  geom_hline(yintercept = .5, color = 'green')



# Section 3.3 ------------------------------------------------------------------

x <- matrix(rnorm(10*5),10,5)
x
dim(x)
dim(x)[2]

seq(nrow(x))
x+seq(nrow(x))
      
# Q6

dat <- read_mnist()
train <- dat$train

train
str(train)

mean(train$images > 50 & train$images < 205)

rm(train,dat)

# Section 4.1 ------------------------------------------------------------------

dist(1,2)
dist(c(1,2))
dist(c(1,2,3))
dist(1:5)
dist(matrix(1:25,5,5))

x
x[order(x)]

# Q1

data("tissue_gene_expression")

x <- tissue_gene_expression$x
y <- tissue_gene_expression$y

d <- dist(x)

image(as.matrix(d))

dist(x[1:2,])
dist(x[39:40,])
dist(x[73:74,])

dist(x[c(1,2,39,40,73,74),]) 

dist(x) %>% as.matrix %>% image

# Q1

set.seed(1, sample.kind = "Rounding")
data('heights')

index <- createDataPartition(heights$sex, p=.5, times=1, list=FALSE)
heights_test <- heights[index,]
heights_train <- heights[-index,]

k <- seq(1,101,3)

F_1 <- sapply(k,function(k){
  fit <- knn3(data = heights_train, sex ~ height, k=k)
  y_hat <- predict(fit, newdata=heights_test, type = "class")
  F_meas(data = y_hat, reference = heights_test$sex) 
  # accuracy <- mean(y_hat == heights_test$sex)
})

plot(k,F_1)
max(F_1)
k[which.max(F_1)]

fit <- knn3(data = heights_train, sex ~ height, k = which.max(F_1))
ref_heights <- data.frame(height = 45:85)
cond_prob <- predict(fit, newdata=ref_heights) %>%
  as_data_frame() %>%
  bind_cols(ref_heights) %>%
  gather('sex','probability',-height)

cond_prob %>%
  ggplot(aes(x=height, y=probability, color=sex)) +
  geom_line()


# Q2

data("tissue_gene_expression")
set.seed(1, sample.kind = "Rounding")

x <- tissue_gene_expression$x
y <- tissue_gene_expression$y

index <- 
  createDataPartition(y, p=.5, times=1, list=FALSE)

x_test <- x[index,]
y_test <- y[index]
x_train <- x[-index,]
y_train <- y[-index]

k <- seq(1,11,2)

accuracy <- sapply(k,function(k){
  fit <- knn3(x_train, y_train, k=k)
  y_hat <- predict(fit, newdata = x_test, type = "class")
  accuracy <- mean(y_test == y_hat)
})

plot(k,accuracy)

data.frame(k = k,
           accuracy = accuracy)

# Section 4.2 ------------------------------------------------------------------

# Cross-validation Q1 - Q7

set.seed(1996, sample.kind="Rounding")
n <- 1000
p <- 10000
x <- matrix(rnorm(n*p), n, p)
colnames(x) <- paste("x", 1:ncol(x), sep = "_")

y <- rbinom(n, 1, 0.5) %>% factor()
x_subset <- x[ ,sample(p, 100)]

fit <- train(x_subset, y, method = 'glm')
fit$results

tt <- colttests(x, y)
pvals <- tt$p.value
ind <- which(pvals < .01)

x_subset <- x[,ind]

fit <- train(x_subset, y, method = 'glm')
fit$results

fit <- train(x_subset, y, method = "knn", tuneGrid = data.frame(k = seq(101, 301, 25)))
ggplot(fit)

data("tissue_gene_expression")
x <- tissue_gene_expression$x
y <- tissue_gene_expression$y
fit <- train(x,y,method="knn", tuneGrid = data.frame(k= seq(1,7,2)))
fit$results

# Bootstraping Q1 - Q2

data('mnist_27')
set.seed(1995, sample.kind="Rounding")
indexes <- createResample(mnist_27$train$y, 10)
glimpse(indexes)
table(indexes$Resample01)

set.seed(1995, sample.kind="Rounding")
indexes <- createResample(mnist_27$train$y, 10, list=FALSE)
sum(indexes == 3)

# Bootstraping Q3

y <- rnorm(100, 0, 1)
qnorm(.75)
quantile(y,.75)

B <- 10000

set.seed(1,sample.kind = "Rounding")
stats <- replicate(B,{
  y <- rnorm(100,0,1)
  q <- quantile(y,.75)
})
data.frame(exp = mean(stats),
           std = sd(stats))

# Bootstraping Q4

set.seed(1,sample.kind = "Rounding")
y <- rnorm(100,0,1)

set.seed(1,sample.kind = "Rounding")
samples <- createResample(y,times=10, list=FALSE)
colMeans(samples)
colSds(samples)

set.seed(1,sample.kind = "Rounding")
ind <- createResample(y,times=10,list=FALSE)
samples <- matrix(y[ind],
                  nrow=dim(ind)[1],
                  dimnames=attr(ind,'dimnames')) %>% 
  as.data.frame()
samples %>% summarize_all(quantile, probs=.75) %>%
  gather('resample','quantile') %>%
  summarize(avg_hat = mean(quantile),
            std_hat = sd(quantile))

# Q5
set.seed(1,sample.kind = "Rounding")
ind <- createResample(y,times=10000,list=FALSE)

samples <- matrix(y[ind],
                  nrow=dim(ind)[1],
                  dimnames=attr(ind,'dimnames')) %>% 
  as.data.frame()

samples %>% summarize_all(quantile, probs=.75) %>%
  gather('resample','quantile') %>%
  summarize(avg_hat = mean(quantile),
            std_hat = sd(quantile))

# Section 4.3 ------------------------------------------------------------------

# Q1, Q2

data("tissue_gene_expression")

set.seed(1993, sample.kind="Rounding")
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]

fit <- train(x,y,method='lda')

means <- fit$finalModel$means %>%
  as.data.frame() %>%
  mutate(source = row.names(.)) %>%
  gather('gene','mean',-source)

means %>%
  ggplot(aes(x = gene, y = mean, color = source)) +
  geom_point()
  
# Q3, Q4

set.seed(1993)
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]

fit <- train(x,y,method='qda')

means <- fit$finalModel$means %>%
  as.data.frame() %>%
  mutate(source = row.names(.)) %>%
  gather('gene','mean',-source)

means %>%
  ggplot(aes(x = gene, y = mean, color = source)) +
  geom_point()

# Q5

fit <- train(x,y,method='lda', preProcess = "center")

means <- fit$finalModel$means %>%
  as.data.frame() %>%
  mutate(source = row.names(.)) %>%
  gather('gene','mean',-source)

means %>%
  ggplot(aes(x = gene, y = mean, color = source)) +
  geom_point()

# Q6

set.seed(1993, sample.kind="Rounding")
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
x <- x[, sample(ncol(x), 10)]

fit <- train(x,y,method='lda', preProcess = "center")

means <- fit$finalModel$means %>%
  as.data.frame() %>%
  mutate(tissue = row.names(.)) %>%
  gather('gene','mean',-tissue)

means %>%
  ggplot(aes(x = gene, y = mean, color = tissue)) +
  geom_point()

# Section 5.1 ------------------------------------------------------------------

# Q1 - Q3

n <- 1000
sigma <- 0.25
set.seed(1, sample.kind = "Rounding")
x <- rnorm(n, 0, 1)
y <- 0.75 * x + rnorm(n, 0, sigma)
dat <- data.frame(x = x, y = y)

fit <- rpart(y ~ ., data = dat)
plot(fit, margin = .05)
text(fit, cex = .75)

dat <- 
  dat %>%
  mutate(y_hat = predict(fit, newdata=.))

dat %>%
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  geom_step(aes(y = y_hat), color = 'red', size = 2)

# Q4 - Q5

fit <- randomForest(y ~ x, data = dat)
plot(fit)

dat <- 
  dat %>%
  mutate(y_hat = predict(fit, newdata=.))

dat %>%
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  geom_step(aes(y = y_hat), color = 'red', size = 2)

# Q6

fit <- randomForest(y ~ x, data = dat, maxnodes = 25, nodesize = 50, importance = TRUE)
plot(fit)

dat <- 
  dat %>%
  mutate(y_hat = predict(fit, newdata=.))

dat %>%
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  geom_step(aes(y = y_hat), color = 'red', size = 2)

fit$predicted - dat$y_hat

# Section 5.2 ------------------------------------------------------------------

# Q1

data("tissue_gene_expression")

set.seed(1991, sample.kind = "Rounding")
fit <- train(y ~ . , 
             data = as.data.frame(tissue_gene_expression),
             method = 'rpart',
             tuneGrid = data.frame(cp = seq(0, .1, .01)))
fit
confusionMatrix(fit)
plot(fit)

# Q2, Q3

set.seed(1991, sample.kind = "Rounding")
fit <- train(y ~ . , 
             data = as.data.frame(tissue_gene_expression),
             method = 'rpart',
             tuneGrid = data.frame(cp = seq(0, .1, .01)),
             control = rpart.control(minsplit = 0))
fit
confusionMatrix(fit)
plot(fit)
fit$finalModel
plot(fit$finalModel,margin=.1, main = "Decision Tree")
text(fit$finalModel,cex=.8)
fit_rpart <- fit

# Q4

getModelInfo('rf')$rf

y <- tissue_gene_expression$y
x <- tissue_gene_expression$x %>% as.matrix

set.seed(1991, sample.kind = "Rounding")

mtry.values <- c(1:10,seq(50,200,25))
# mtry.values <- 1
# control = rf.control(nodesize=1)
n <- 1

fitRF <- train(y ~ .,
               data = as.data.frame(tissue_gene_expression),
               method = 'rf',
               nodesize = 1,
               tuneGrid = data.frame(mtry = mtry.values))

fitRF2 <- train(x,y,
               method = 'rf',
               nodesize = 1,
               tuneGrid = data.frame(mtry = mtry.values))

fitRF3 <- train(x,y,
                method = 'rf',
                tuneGrid = data.frame(mtry = mtry.values))

fitRF4 <- train(x,y,
                method = 'rf',
                nodesize = 60,
                tuneGrid = data.frame(mtry = mtry.values))

fitRF5 <- train(x,y,
                method = 'rf',
                nodesize = 1,
                tuneGrid = data.frame(mtry = seq(50,200,25)))

imp <- varImp(fitRF5) %>% .$importance
tree_terms <- 
  unique(fit_rpart$finalModel$frame$var[!(fit_rpart$finalModel$frame$var == "<leaf>")]) %>%
  str_remove("^x.") %>%
  as.data.frame()
names(tree_terms) <- 'gene'

tree_terms  
imp

imp %>%
  mutate(gene = row.names(.)) %>%
  mutate(rank = rank(-Overall)) %>%
  right_join(tree_terms, by="gene") %>%
  arrange(rank)
