# housekeeping

library(tidyverse)
library(dslabs)

# poll data

polls <- polls_us_election_2016 %>%
  filter(state == "U.S." & enddate >= "2016-10-31" & 
         grade %in% c("A+","A","A-","B+") | !is.na(grade)) %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

one_poll_per_pollster <- polls %>% 
                         group_by(pollster) %>%
                         filter(enddate == max(enddate)) %>%
                         summarize(spread = sum(spread*samplesize)/sum(samplesize)) %>%
                         filter(!is.na(spread))

results <- one_poll_per_pollster %>%
           summarize(avg = mean(spread), se = sd(spread)/sqrt(length(spread))) %>%
           mutate(start = qnorm(0.025,avg,se), end = qnorm(0.975,avg,se))

# posterior distribution - no general bias

mu <- 0
tau <- 0.035
sigma <- results$se
Y <- results$avg
B <- sigma^2/(sigma^2+tau^2)
posterior_mean <- B*mu + (1-B)*Y
posterior_se <- 1/sqrt(1/sigma^2 + 1/tau^2)

posterior_results <- c(avg = posterior_mean,
                       start = qnorm(0.025,posterior_mean,posterior_se),
                       end = qnorm(0.975,posterior_mean,posterior_se),
                       prop_victory = 1 - pnorm(0,posterior_mean,posterior_se))

# posterior distribution - with general bias

sigma_b <- .025
sigma <- sqrt(results$se^2 + sigma_b^2)
Y <- results$avg
B <- sigma^2/(sigma^2+tau^2)
posterior_mean <- B*mu + (1-B)*Y
posterior_se <- 1/sqrt(1/sigma^2 + 1/tau^2)

posterior_results_gen_bias <- c(avg = posterior_mean,
                              start = qnorm(0.025,posterior_mean,posterior_se),
                              end = qnorm(0.975,posterior_mean,posterior_se),
                              prop_victory = 1 - pnorm(0,posterior_mean,posterior_se))

# trash

one_poll_per_pollster %>% filter(pollster == "TargetPoint")
