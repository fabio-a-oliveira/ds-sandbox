## Housekeeping

# suggested libraries and options
library(tidyverse)
options(digits = 3)

# load brexit_polls object
library(dslabs)
data(brexit_polls)

p <- 0.481    # official proportion voting "Remain"
d <- 2*p-1    # official spread

## Expected value and standard error

N <- 1500
exp <- 1500 * p
se <- sqrt(N*p*(1-p))
exp_X <- p
se_X <- sqrt(p*(1-p)/N)
exp_d <- 2*p-1
se_d <- 2* se_X
se_d

## Actual poll estimates

brexit_polls <-
  brexit_polls %>%
  mutate(x_hat = (spread+1)/2)

brexit_polls %>% .$spread %>% mean()
brexit_polls %>% .$spread %>% sd()
brexit_polls %>% .$x_hat %>% mean()
brexit_polls %>% .$x_hat %>% sd()

## Confidence interval

brexit_polls <- 
  brexit_polls %>%
  mutate(se_hat = sqrt(x_hat*(1-x_hat)/samplesize),
         ci_lower = qnorm(0.025,x_hat,se_hat),
         ci_upper = qnorm(0.975,x_hat,se_hat))

brexit_polls[1,]

## Confidence intervals for polls in June

brexit_polls <-
  brexit_polls %>%
  mutate(se_spread = 2*se_hat,
         ci_spread_lower = qnorm(0.025,spread,se_spread),
         ci_spread_upper = qnorm(0.975,spread,se_spread),
         hit = d > ci_spread_lower & d < ci_spread_upper,
         cover_zero = 0 > ci_spread_lower & 0 < ci_spread_upper)

brexit_polls %>%
  filter(enddate >= "2016-06-01") %>%
  .$cover_zero %>%
  mean()

brexit_polls %>%
  filter(enddate >= "2016-06-01") %>%
  transmute(winner_remain = ci_spread_lower > 0) %>%
  .$winner_remain %>%
  mean()
  
brexit_polls %>%
  filter(enddate >= "2016-06-01") %>%
  transmute(winner_leave = ci_spread_upper < 0) %>%
  .$winner_leave %>%
  mean()

brexit_polls %>%
  filter(enddate >= "2016-06-01") %>%
  .$hit %>%
  mean()  

## Hit rate by pollster

brexit_polls %>%
  filter(enddate >= "2016-06-01") %>%
  group_by(pollster) %>%
  summarize(number_of_polls = n(),
            proportion_of_hits = mean(hit)) %>%
  arrange(proportion_of_hits)
  
## Boxplot

brexit_polls %>%
  filter(enddate >= "2016-06-01") %>%
  ggplot(aes(x=poll_type,y=spread)) +
  geom_boxplot()

## Combined spread

combined_by_type <- 
  brexit_polls %>%
  filter(enddate >= "2016-06-01") %>%
  group_by(poll_type) %>%
  summarize(N = sum(samplesize),
            spread = sum(spread*samplesize)/N,
            p_hat = (spread + 1)/2) %>%
  mutate(ci_lower = qnorm(0.025,spread,2*sqrt(p_hat*(1-p_hat)/N)),
         ci_upper = qnorm(0.975,spread,2*sqrt(p_hat*(1-p_hat)/N)))

## Chi-squared p-value

brexit_hit <-
  brexit_polls %>%
  select(poll_type,hit)

brexit_hit %>%
  group_by(poll_type) %>%
  summarize(hits = sum(hit==TRUE),
            misses = sum(hit==FALSE)) %>%
  select(-poll_type) %>%
  chisq.test()

two_by_two <-
  brexit_hit %>%
  group_by(poll_type) %>%
  summarize(hits = sum(hit==TRUE),
            misses = sum(hit==FALSE))

odds <-
  two_by_two %>%
  group_by(poll_type) %>%
  mutate(odds = hits/misses)
  
odds_ratio <- 
  odds$odds[1] / odds$odds[2]

## Spread over time

brexit_polls %>%
  ggplot(aes(x = enddate,
             y = spread,
             color = poll_type)) + 
  geom_smooth(method = "loess",
              span = 0.4) +
  geom_point() +
  geom_hline(yintercept = d)

## Percentages over time

brexit_long <- brexit_polls %>%
  gather(vote, proportion, "remain":"undecided") %>%
  mutate(vote = factor(vote))

brexit_long %>%
  ggplot(aes(x = enddate,
             y = proportion,
             color = vote)) +
  geom_smooth(method = "loess",
              span = .3) +
  geom_point()

## Trash

# referência a uma linha específica
brexit_polls[1,]
