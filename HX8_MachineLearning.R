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

library('titanic')

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

# Section 5.3 ------------------------------------------------------------------

options(digits = 3)

# clean the data - `titanic_train` is loaded with the titanic package
titanic_clean <- titanic_train %>%
  mutate(Survived = factor(Survived),
         Embarked = factor(Embarked),
         Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age), # NA age to median age
         FamilySize = SibSp + Parch + 1) %>%    # count family members
  select(Survived,  Sex, Pclass, Age, Fare, SibSp, Parch, FamilySize, Embarked)

# Q1

set.seed(42, sample.kind = "Rounding")
ind <- createDataPartition(titanic_clean$Survived,
                           times = 1,
                           p = .2,
                           list = FALSE)
test <- titanic_clean[ind,]
train <- titanic_clean[-ind,]

nrow(train)
nrow(test)
train %>% summarize(pSurvived = mean(if_else(Survived == 1, 1, 0)))

# Q2

set.seed(3, sample.kind = "Rounding")

test <- 
  test %>%
  mutate(Survived = if_else(Survived == 1, 1,0),
         y_hat_guess1 = sample(c(0,1),nrow(.),replace = TRUE))

test %>%
  summarize(accuracy = mean(y_hat_guess1 == Survived))

# Q3

train %>%
  group_by(Sex) %>%
  mutate(Survived = if_else(Survived == 1, 1, 0)) %>%
  summarize(SurvivalRate = mean(Survived))

test <- 
  test %>%
  mutate(y_hat_guess2 = if_else(Sex == "female", 1, 0))

test %>%
  summarize(Accuracy = mean(y_hat_guess2 == as.numeric(as.character(Survived))))

# Q4

train %>%
  group_by(Pclass) %>%
  summarize(SurvivalRate = mean(as.numeric(as.character(Survived))))

test <- 
  test %>%
  mutate(y_hat_guess3 = if_else(Pclass == 1, 1, 0))

test %>%
  summarize(Accuracy = mean(y_hat_guess3 == as.numeric(as.character(Survived))))

train %>%
  group_by(Pclass, Sex) %>%
  summarize(SurvivalRate = mean(as.numeric(as.character(Survived)))) %>%
  arrange(desc(SurvivalRate)) %>%
  filter(SurvivalRate > .5)

test <- 
  test %>%
  mutate(y_hat_guess4 = if_else(Sex == 'female' & Pclass != 3, 1, 0))

test %>%
  summarize(Accuracy = mean(y_hat_guess4 == as.numeric(as.character(Survived))))

# Q5

test <- 
  test %>%
  mutate_at(vars(Survived,y_hat_guess1,y_hat_guess2,y_hat_guess3,y_hat_guess4),
            funs(as.factor(.)))
str(test)

# prediction by sex

confusionMatrix(data = test$y_hat_guess2,
                ref = test$Survived)

# predition by class

confusionMatrix(data = test$y_hat_guess3,
                ref = test$Survived)

# predition by class and sex

confusionMatrix(data = test$y_hat_guess4,
                ref = test$Survived)

# Q6

# prediction by sex

F_meas(data = test$y_hat_guess2,
       ref = test$Survived)

# predition by class

F_meas(data = test$y_hat_guess3,
       ref = test$Survived)

# predition by class and sex

F_meas(data = test$y_hat_guess4,
       ref = test$Survived)

# Q7

set.seed(1, sample.kind = "Rounding")
fitLDA <- train(Survived ~ Fare,
                method = 'lda', 
                data = train)

test <- 
  test %>%
  mutate(y_hat_lda = predict(fitLDA, newdata = .))

confusionMatrix(data = test$y_hat_lda,
                reference = test$Survived)

set.seed(1, sample.kind = "Rounding")
fitQDA <- train(Survived ~ Fare,
                method = 'qda', 
                data = train)
test <- 
  test %>%
  mutate(y_hat_qda = predict(fitQDA, newdata = .))

confusionMatrix(data = test$y_hat_qda,
                reference = test$Survived)

# Q8

set.seed(1, sample.kind = "Rounding")
fitGLM <- train(Survived ~ Age,
                data = train,
                method = 'glm')
test <- 
  test %>%
  mutate(y_hat_glm = predict(fitGLM, newdata = .))

confusionMatrix(data = test$y_hat_glm,
                ref = test$Survived)

set.seed(1, sample.kind = "Rounding")
fitGLM2 <- train(Survived ~ Sex + Pclass + Fare + Age,
                   data = train,
                   method = 'glm')
test <- 
  test %>%
  mutate(y_hat_glm2 = predict(fitGLM2, newdata = .))

confusionMatrix(data = test$y_hat_glm2,
                ref = test$Survived)

set.seed(1, sample.kind = "Rounding")
fitGLMall <- train(Survived ~ .,
                 data = train,
                 method = 'glm')
test <- 
  test %>%
  mutate(y_hat_glm_all = predict(fitGLMall, newdata = .))

confusionMatrix(data = test$y_hat_glm_all,
                ref = test$Survived)

# Q9

set.seed(6, sample.kind = "Rounding")
fitKNN <- train(Survived ~ .,
                data = train,
                method = 'knn',
                tuneGrid = data.frame(k = seq(3,51,2)))

plot(fitKNN)

test <-
  test %>%
  mutate(y_hat_knn = predict(fitKNN, newdata = .))

confusionMatrix(data = test$y_hat_knn,
                reference = test$Survived)

# Q10

suppressWarnings(set.seed(8, sample.kind = "Rounding"))
fitKNN2 <- train(Survived ~ .,
                 data = train,
                 method = 'knn',
                 tuneGrid = data.frame(k = seq(3,51,2)),
                 trControl = trainControl(method = "cv"))

test <- 
  test %>%
  mutate(y_hat_knn2 = predict(fitKNN2, newdata = .))

confusionMatrix(data = test$y_hat_knn2,
                reference = test$Survived)

# Q11

suppressWarnings(set.seed(10,sample.kind = "Rounding"))

fitTree <- train(Survived ~ .,
                 data = train,
                 method = 'rpart',
                 tuneGrid = data.frame(cp = seq(0,.05,.002)))

test <- 
  test %>%
  mutate(y_hat_tree = predict(fitTree, newdata = .))


with(test, confusionMatrix(data = y_hat_tree,
                           reference = Survived))

plot(fitTree)
plot(fitTree$finalModel,margin=.05)
text(fitTree$finalModel,cex = .7)

passengers <- 
  tribble(~Sex, ~Age, ~Pclass, ~Fare, ~SibSp, ~Parch, ~FamilySize, ~Embarked,
          'male', 28, 0, 0, 0, 0, 0, '', 
          'female', 0, 2, 0, 0, 0, 0, '', 
          'female',0,3,8,0, 0, 0, '',
          'male',5,0, 0,4, 0, 0,'',
          'female',0,3,25,0, 0, 0, '',
          'female',17,1,0,2, 0, 0,'',
          'male',17,1,0,2, 0, 0,'')

passengers <- 
  train %>%
  select(-Survived) %>%
  add_case(passengers) %>%
  tail(7) %>%
  mutate(y_hat = predict(fitTree, newdata = .))

passengers

# Q12

suppressWarnings(set.seed(14,sample.kind = "Rounding"))
fitRF <- train(Survived ~ .,
               data = train,
               method = 'rf',
               tuneGrid = data.frame(mtry = 1:7),
               ntree = 100)
test <-
  test %>%
  mutate(y_hat_rf = predict(fitRF, newdata = .))

confusionMatrix(data = test$y_hat_rf,
                reference = test$Survived)

varImp(fitRF)

# Section 6.1 ------------------------------------------------------------------

# Q1

models <- c("glm", 
            "lda", 
            "naive_bayes", 
            "svmLinear", 
            "knn", 
            "gamLoess", 
            "multinom", 
            "qda", 
            "rf", 
            "adaboost")

suppressWarnings(set.seed(1,sample.kind="Rounding"))

data("mnist_27")

fits <- lapply(models, function(model){ 
  print(model)
  train(y ~ ., method = model, data = mnist_27$train)
}) 

names(fits) <- models

# Q2

y_hat <- sapply(models,function(model){
  predict(fits[model],
          newdata = mnist_27$test)
}) %>%
  as.data.frame()

names(y_hat) <- models
glimpse(y_hat)

# Q3

accuracy <- 
  y_hat %>%
  summarize_all(.funs = function(y_hat){mean(y_hat == mnist_27$test$y)})

accuracy %>% as.numeric %>% mean()

# Q4

predictions <- 
  y_hat %>%
  mutate(case = 1:nrow(.)) %>%
  pivot_longer(cols = -case,
               names_to = 'method',
               values_to = 'prediction') %>%
  group_by(case) %>%
  summarize(most_voted = if_else(sum(prediction == 7) > 5,7,2)) %>%
  transmute(most_voted = as.factor(most_voted))

y_hat <- 
  predictions %>%
  select(most_voted) %>%
  bind_cols(y_hat) %>%
  mutate(most_voted = as.factor(most_voted))

accuracy <- 
  y_hat %>%
  summarize_all(.funs = function(y_hat){mean(y_hat == mnist_27$test$y)})

accuracy

# Q6

Accuracy <- sapply(models, function(model){
  accuracy <- fits[[model]]$results$Accuracy %>% max()
})

mean(Accuracy)

# Q7

y_hat_ensemble <- 
  y_hat %>%
  select(glm,naive_bayes,knn,gamLoess,qda,rf,adaboost) %>%
  mutate(case = 1:nrow(y_hat)) %>%
  pivot_longer(cols = -case,
               names_to = 'method',
               values_to = 'prediction') %>%
  group_by(case) %>%
  summarize(most_voted = ifelse(sum(prediction == 7)>=4,7,2)) %>%
  transmute(most_voted = as.factor(most_voted))


voted_accuracy <- mean(y_hat_ensemble == as.data.frame(mnist_27$test$y))

# Section 6.2 ------------------------------------------------------------------

data("movielens")

# Q1

ratings_by_year <- 
  movielens %>%
  group_by(movieId,title) %>% 
  summarize(counts = n(),
            year = median(year))

ratings_by_year %>% 
  group_by(year) %>% 
  summarize(median = median(counts)) %>%
  top_n(5,median)
  

ratings_by_year %>% 
  ggplot(aes(x=year, y=counts)) +
  geom_point(alpha = .1) +
  scale_y_continuous(trans = 'sqrt') +
  geom_smooth(method = 'lm')

# Q2

ratings_per_year <- 
  ratings_by_year %>% 
  mutate(avg_ratings_per_year = counts/(2018-year))

avg_ratings <- 
  movielens %>% 
  group_by(movieId) %>% 
  summarize(avg_rating = mean(rating))

options(tibble.print_max = 25)

movies <- left_join(ratings_per_year, avg_ratings, by='movieId') %>% 
  ungroup() 

movies %>% 
  slice_max(order_by=avg_ratings_per_year, n=25)

# Q3

movies %>% 
  filter(year >= 1993) %>%
  mutate(strata = round(avg_ratings_per_year)) %>%
  ggplot(aes(x = strata, y = avg_rating)) +
  geom_point() +
  geom_smooth(method = 'lm')

# Q5

movielens %>% 
  mutate(date = as_datetime(timestamp))

# Q6

movielens %>% 
  mutate(date = round_date(as_datetime(timestamp),'week')) %>%
  group_by(date) %>% 
  summarise(avg_rating = mean(rating)) %>% 
  ggplot(aes(x = date, y = avg_rating)) +
  geom_point(alpha = .1) +
  geom_smooth(method = 'loess')

# Q8

popular_genres <-
  movielens %>% 
  group_by(genres) %>% 
  summarize(count = n()) %>%
  filter(count > 1000)

movielens %>% 
  filter(genres %in% popular_genres$genres) %>%
  group_by(genres) %>% 
  summarize(avg_rating = mean(rating),
            min = avg_rating - sd(rating),
            max = avg_rating + sd(rating)) %>% 
  arrange(desc(avg_rating)) %>%
  ggplot(aes(y = reorder(genres,avg_rating), x = avg_rating)) +
  geom_errorbar(aes(xmin = min, xmax = max)) +
  geom_point()

# Section 6.3 - Regularization -------------------------------------------------

# Regularization - Q1

options(digits=7)

suppressWarnings(set.seed(1986, sample.kind = "Rounding"))
n <- round((2^rnorm(1000,8,1)))
suppressWarnings(set.seed(1, sample.kind = "Rounding"))
mu <- round(80 + 2*rt(1000,5))
schools <- data.frame(id = paste('PS',1:1000),
                      size = n,
                      quality = mu,
                      rank = rank(-mu))

schools %>%
  slice_max(n=10,quality)

suppressWarnings(set.seed(1, sample.kind = "Rounding"))
mu <- round(80 + 2*rt(1000, 5))

scores <- sapply(1:nrow(schools),function(i){
  rnorm(schools$size[i],schools$quality,30) %>% mean()
})

schools <- 
  schools %>% 
  mutate(score = scores)

slice_max(schools,n=10,order_by=score)


# correction

set.seed(1986, sample.kind="Rounding")
n <- round(2^rnorm(1000, 8, 1))

set.seed(1, sample.kind="Rounding")
mu <- round(80 + 2*rt(1000, 5))
range(mu)
schools <- data.frame(id = paste("PS",1:1000),
                      size = n,
                      quality = mu,
                      rank = rank(-mu))

schools %>% top_n(10, quality) %>% arrange(desc(quality))

set.seed(1, sample.kind="Rounding")
mu <- round(80 + 2*rt(1000, 5))

scores <- sapply(1:nrow(schools), function(i){
  scores <- rnorm(schools$size[i], schools$quality[i], 30)
  scores
})
schools <- schools %>% mutate(score = sapply(scores, mean))

slice_max(schools,n=10,order_by=score)

# Q2

schools %>%
  summarise(median_all = median(size),
            median_top = median(size[score >= 87.95731]))

# Q3

score_bottom <- 
  schools %>%
  slice_min(n = 10, order_by = score) %>%
  summarize(cutoff = max(score))

schools %>% 
  summarize(median_all = median(size),
            median_bottom = median(size[score <= score_bottom$cutoff]))

# Q4

top_schools <- schools %>% slice_max(n = 10, order_by = score)
bottom_schools <- schools %>% slice_min(n = 10, order_by = score)

schools %>% 
  ggplot(aes(x = size, y = score)) +
  geom_point(alpha = .2) +
  geom_point(data = slice_max(schools,n=10,order_by=score),
             mapping = aes(x = size, y = score),
             color = 'blue', size = 2) + 
  geom_point(data = slice_min(schools,n=10,order_by=score),
             mapping = aes(x = size, y = score),
             color = 'red', size = 2) +
  geom_smooth(method = 'lm')

# Q5

overall <- mean(schools$score)
alpha <- 25

schools <- 
  schools %>% 
  mutate(reg_score = overall + sapply(scores,function(score){
    sum(score-overall)/(length(score)+alpha)}))

schools %>%
  slice_max(n = 10, order_by = reg_score)

schools %>% 
  ggplot(aes(x = size, y = reg_score)) +
  geom_point(alpha = .2) +
  geom_point(data = slice_max(schools,n=10,order_by=reg_score),
             mapping = aes(x = size, y = reg_score),
             color = 'blue', size = 2) + 
  geom_point(data = slice_min(schools,n=10,order_by=reg_score),
             mapping = aes(x = size, y = reg_score),
             color = 'red', size = 2) +
  geom_smooth(method = 'lm')

# Q6

alpha <- 10:250
RMSE <- sapply(alpha, function(alpha){
  schools %>% 
    mutate(reg_score = overall + sapply(scores,function(score){
      sum(score-overall)/(length(score)+alpha)})) %>%
    summarize(RMSE = sqrt(sum((quality-reg_score)^2)/1000))
})

alpha <- data.frame(alpha = alpha,
                    RMSE = as.numeric(RMSE))

slice_min(alpha,n=1,order_by=RMSE)

# Q7

schools <- 
  schools %>% 
  select(-reg_score) %>% 
  mutate(bi = sapply(scores,function(score){
    sum(score-overall)/(length(score)+135)})) %>% 
  mutate(reg_score = overall + bi)
  
schools %>% 
  ggplot(aes(x = size, y = reg_score)) +
  geom_point(alpha = .2) +
  geom_smooth(method = 'lm') +
  geom_point(data = slice_max(schools, n = 10, order_by = reg_score),
             mapping = aes(x = size, y = reg_score),
             color = 'blue', size = 2) +
  geom_point(data = slice_min(schools, n = 10, order_by = reg_score),
             mapping = aes(x = size, y = reg_score),
             color = 'red', size = 2)

schools %>% 
  slice_max(n = 10, order_by = reg_score)

# Q8

alpha <- 10:250
RMSE <- sapply(alpha, function(alpha){
  schools %>% 
    mutate(reg_score = sapply(scores,function(score){
      sum(score)/(length(score)+alpha)})) %>%
    summarize(RMSE = sqrt(sum((quality-reg_score)^2)/1000))
})

alpha <- data.frame(alpha = alpha,
                    RMSE = as.numeric(RMSE))

slice_min(alpha,n=1,order_by=RMSE)

# Section 6.3 - Matrix Factorization -------------------------------------------

suppressWarnings(set.seed(1987, sample.kind = "Rounding"))

n <- 100
k <- 8
Sigma <- 64 * matrix(c(1, .75, .5, .75, 1, .5, .5, .5, 1), 3, 3)
m <- MASS::mvrnorm(n, rep(0, 3), Sigma)
m <- m[order(rowMeans(m), decreasing = TRUE),]
y <- m %x% matrix(rep(1, k), nrow = 1) + matrix(rnorm(matrix(n*k*3)), n, k*3)
colnames(y) <- c(paste(rep("Math",k), 1:k, sep="_"),
                 paste(rep("Science",k), 1:k, sep="_"),
                 paste(rep("Arts",k), 1:k, sep="_"))

# Q1

my_image <- function(x, zlim = range(x), ...){
  colors = rev(RColorBrewer::brewer.pal(9, "RdBu"))
  cols <- 1:ncol(x)
  rows <- 1:nrow(x)
  image(cols, rows, t(x[rev(rows),,drop=FALSE]), xaxt = "n", yaxt = "n",
        xlab="", ylab="",  col = colors, zlim = zlim, ...)
  abline(h=rows + 0.5, v = cols + 0.5)
  axis(side = 1, cols, colnames(x), las = 2)
}

my_image(y)

# Q2

my_image(cor(y), zlim = c(-1,1))
range(cor(y))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)

# Q3

s <- svd(y)
names(s)

y_svd <- s$u %*% diag(s$d) %*% t(s$v)
max(abs(y - y_svd))

ss_y <- apply(X = y,
              MARGIN = 2,
              FUN = function(col){sum(col^2)})

ss_yv <- apply(X = y %*% s$v,
               MARGIN = 2,
               FUN = function(col){sum(col^2)})

sum(ss_y)
sum(ss_yv)

# Q4

plot(ss_y)
plot(ss_yv)

# Q5

plot(sqrt(ss_yv),s$d)

# Q6

cumsum(s$d^2 / sum(s$d^2))

# Q7

U <- s$u
d <- s$d
D <- diag(d)
V <- s$v

UD <- t(apply(X = U,
            MARGIN = 1,
            FUN = function(l){l * d}))

UD <- sweep(U,2,d,FUN = "*")

identical(UD, U %*% diag(d))

# Q8

avg_std <- apply(X = y,
                 MARGIN = 1,
                 FUN = mean)

plot(avg_std, UD[,1])

# Q10

plot(U[,1])
plot(V[,1])

M <- UD[,1] %*% t(V[,1])
my_image(M)

M <- M + UD[,2] %*% t(V[,2])
my_image(M)

M <- M + UD[,3] %*% t(V[,3])
my_image(M)

M <- M + UD[,4] %*% t(V[,4])
my_image(M)

M <- M + UD[,5] %*% t(V[,5])
my_image(M)

M <- M + UD[,6] %*% t(V[,6])
my_image(M)

M <- M + UD[,7] %*% t(V[,7])
my_image(M)

M <- M + UD[,8] %*% t(V[,8])
my_image(M)

M <- M + UD[,9] %*% t(V[,9])
my_image(M)

M <- M + UD[,10] %*% t(V[,10])
my_image(M)

my_image(y)

# Q11

y_hat_1 <- UD[,1] %*% t(V[,1])

resid <- y - with(s,(u[, 1, drop=FALSE]*d[1]) %*% t(v[, 1, drop=FALSE]))
my_image(cor(resid), zlim = c(-1,1))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)

M <- UD[,2] %*% t(V[,2])
my_image(M)
my_image(resid)

plot(U[,2])
plot(V[,2])

# Q12

resid <- y - with(s,sweep(u[, 1:2], 2, d[1:2], FUN="*") %*% t(v[, 1:2]))
my_image(cor(resid), zlim = c(-1,1))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)

plot(U[,3])
plot(V[,3])

M <- UD[,3] %*% t(V[,3])
my_image(M)
my_image(resid)

# Q13

resid <- y - with(s,sweep(u[, 1:3], 2, d[1:3], FUN="*") %*% t(v[, 1:3]))
my_image(cor(resid), zlim = c(-1,1))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)

# Section 6.3 - Dimension Reduction --------------------------------------------

data("tissue_gene_expression")

genes <- 
  tissue_gene_expression$x %>% 
  as_tibble() %>% 
  add_column(tissue = tissue_gene_expression$y, .before = 1)

pc <- prcomp(tissue_gene_expression$x)

plot(pc$sdev)

# Q1

data.frame(tissue = tissue_gene_expression$y, pc$x[,1:2]) %>%
  ggplot(aes(x = PC1, y = PC2, color = tissue)) +
  geom_point()

# Q2

avg <- rowMeans(tissue_gene_expression$x)
PC1 = pc$x[,1]

data.frame(tissue = tissue_gene_expression$y, 
           PC1 = PC1,
           avg = avg) %>%
  ggplot(aes(x = avg, y = PC1, color = tissue)) +
  geom_point()

cor(PC1,avg)

# Q3

x <- tissue_gene_expression$x - rowMeans(tissue_gene_expression$x)
x <- with(tissue_gene_expression, sweep(x, 1, rowMeans(x)))

pc <- prcomp(x)

data.frame(PC1 = pc$x[,1], 
           PC2 = pc$x[,2], 
           tissue = tissue_gene_expression$y) %>%
  ggplot(aes(x = PC1, y = PC2, color = tissue)) +
  geom_point()

cor(rowMeans(x), pc$x[,1])
cor(pc$x[,1],pc$x[,2])

# Q4

data.frame(pc$x[,1:10],
           tissue = tissue_gene_expression$y) %>%
  pivot_longer(-tissue, 
               names_to = 'Principal Component',
               values_to = 'weight') %>% 
  ggplot(aes(x = tissue, y = weight)) +
  geom_boxplot() +
  facet_wrap(. ~ `Principal Component`) +
  theme(axis.text.x = element_text(angle = 90))

# Q5

summary(pc)

# playground

image(as.matrix(pc$x[,1:20]), col = rev(RColorBrewer::brewer.pal(9, "RdBu")))


# Section 6.3 - Clustering -----------------------------------------------------

# Section 7 --------------------------------------------------------------------







































































































