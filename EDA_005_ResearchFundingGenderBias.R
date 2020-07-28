# housekeeping

library(tidyverse)
library(dslabs)
library(ggplot2)
data("research_funding_rates")

# analysis

totals <- research_funding_rates %>%
  select(-discipline) %>%
  summarize_all(funs(sum)) %>%
  summarize(yes_men = awards_men,
            no_men = applications_men - awards_men,
            yes_women = awards_women,
            no_women = applications_women - awards_women)

totals %>% summarize(percent_men = yes_men/(yes_men+no_men),
                     percent_women = yes_women/(yes_women+no_women))

# analysis of the spread

totals_per_gender <- data.frame(gender = c("Male","Female"),
                                success_rate = c(totals$yes_men/(totals$yes_men + totals$no_men),
                                                 totals$yes_women/(totals$yes_women + totals$no_women)),
                                sample_size = c(totals$yes_men + totals$no_men, totals$yes_women + totals$no_women)) %>%
                     mutate(se = sqrt(success_rate*(1-success_rate)/sample_size))
                                                 
combined_spread <- totals_per_gender$success_rate[totals_per_gender$gender=="Male"] - totals_per_gender$success_rate[totals_per_gender$gender=="Female"]
combined_se <- 1/sqrt(1/totals_per_gender$se[totals_per_gender$gender=="Male"]^2 + 1/totals_per_gender$se[totals_per_gender$gender=="Female"]^2)
p_null_hypothesis <- pnorm(0,combined_spread,combined_se)
p_null_hypothesis

# analysis of the spread per discipline

totals_per_discipline <-
  research_funding_rates %>%
  mutate(success_rates_men = success_rates_men/100,
         success_rates_women = success_rates_women/100) %>%
  mutate(se_men = sqrt(success_rates_men*(1-success_rates_men)/applications_men),
         se_women = sqrt(success_rates_women*(1-success_rates_women)/applications_women),
         spread = success_rates_men - success_rates_women,
         se_spread = 1/sqrt(1/se_men^2 + 1/se_women^2)) %>%
  select(discipline,success_rates_men,success_rates_women,se_men,se_women,spread,se_spread) %>%
  add_case(discipline = "Combined",
           success_rates_men = 0,
           success_rates_women = 0,
           se_men = 0,
           se_women = 0,
           spread = combined_spread,
           se_spread = combined_se) %>%
  mutate(p_null = pnorm(0,spread,se_spread),
         confidence_interval_95pc = paste(qnorm(0.025,spread,se_spread),qnorm(0.975,spread,se_spread),sep=" : "),
         include_zero = 0>qnorm(0.025,spread,se_spread) & 0<qnorm(0.975,spread,se_spread),
         favorite = ifelse(spread>0,"Men","Women"))
 
# plot - com coordflip() - descontinuar...

# totals_per_discipline %>%
#   ggplot(aes(x=discipline,y=spread)) +
#   geom_point(size=5) +
#   geom_errorbar(aes(ymin = spread-1.96*se_spread, ymax = spread+1.96*se_spread),
#                 size=2,width=.5) +
#   geom_hline(yintercept=0,color='red') +
#   coord_flip() +
#   labs(title = "Gender bias in research funding",
#        subtitle = "based on data from the research_funding_rates database contained in the dslabs library") +
#   ylab("Gender bias \n (difference in probability and 95% confidence interval of receiving a grant according to gender)") +
#   xlab("Discipline") +
#   scale_x_discrete(limits = c("Technical sciences",
#                               "Social sciences",
#                               "Physics",
#                               "Physical sciences",
#                               "Medical sciences",
#                               "Interdisciplinary",
#                               "Humanities",
#                               "Earth/life sciences",
#                               "Chemical sciences",
#                               "Combined")) +
#   scale_y_continuous(breaks = seq(-.3,.3,.05), 
#                      minor_breaks=seq(-.3,.3,.01),
#                      labels = scales::percent,
#                      limits = c(-.16,.16)) +
#   geom_errorbar(aes(x = "Combined",
#                     y = combined_spread,
#                     ymin = qnorm(0.025,combined_spread,combined_se),
#                     ymax = qnorm(0.975,combined_spread,combined_se)),
#                 size=2,width=.5,color='blue') +
#   geom_point(aes(x = "Combined",
#                  y = combined_spread),
#              size=5, color='blue') +
#   geom_hline(yintercept = combined_spread, color = 'blue', size = 21, alpha = 0.1)

# plot 2 - sem coordflip()
  
totals_per_discipline %>%
  ggplot(aes(x=spread,y=discipline)) +
  geom_point(size=5) +
  labs(title = "Gender bias in research funding",
       subtitle = "based on data from the research_funding_rates database contained in the dslabs library") +
  xlab("Gender bias \n (95% confidence interval of the difference in probability of receiving a grant according to gender)") +
  ylab("Discipline") +
  scale_y_discrete(limits = c("Technical sciences",
                              "Social sciences",
                              "Physics",
                              "Physical sciences",
                              "Medical sciences",
                              "Interdisciplinary",
                              "Humanities",
                              "Earth/life sciences",
                              "Chemical sciences",
                              "Combined")) +
  scale_x_continuous(breaks = seq(-.3,.3,.05), 
                     minor_breaks=seq(-.3,.3,.01),
                     labels = scales::percent) +
  coord_cartesian(xlim=c(-.16,.16),
                  ylim=c(1.1,9.7)) +
  geom_vline(xintercept = combined_spread, color = 'green', size = 21, alpha = 0.2) +
  geom_vline(xintercept = .1, color = 'blue', size = 142, alpha = 0.2) +
  geom_vline(xintercept = -.1, color = 'red', size = 142, alpha = 0.2) +
  geom_vline(xintercept=0,color='red', size = 2) +
  geom_errorbar(aes(xmin = spread-1.96*se_spread, xmax = spread+1.96*se_spread),
              size=2,width=.5) +
  geom_errorbar(aes(y = "Combined",
                    x = combined_spread,
                    xmin = qnorm(0.025,combined_spread,combined_se),
                    xmax = qnorm(0.975,combined_spread,combined_se)),
                size=2,width=.5,color='blue') +
  geom_point(aes(y = "Combined",
                 x = combined_spread),
             size=5, color='blue')



  geom_col(inherit.aes = FALSE,
           aes(x=.2,y=2),
           alpha = .1,
           size = .1)
  
  
  
  geom_histogram(inherit.aes = FALSE,
                 aes(x=rep(c(-.5,.5),5),
                     color=rep(c('red','blue'),5)),
                 binwidth=1,
                 alpha=0.1)
  
totals_per_discipline %>%
  ggplot(aes(x=discipline,y=spread)) +
  geom_ribbon(aes(ymin=0,ymax=spread),color='red') +
  geom_point() +
  geom_histogram(binwidth=1,alpha=0.1,inherit.aes = FALSE,aes(x=c(0,0,0,0,1,1,1,1,-1,3)))
  
  
  
  
  

  geom_area(aes(x=discipline,y=.20),color='red')
  
  geom_ribbon(aes(ymin=rep(-.1,length(discipline)),
                  ymax=rep( .1,length(discipline))),color='green',fill='green')


# trash

research_funding_rates %>%
  mutate(winner = ifelse(success_rates_men>success_rates_women,"Men","Women")) %>%
  group_by(winner)%>%
  summarize(mean(success_rates_total))


a <- paste(research_funding_rates$discipline, sep = " / ")
a
paste(a)
a

paste(levels(research_funding_rates$discipline))
levels(as.factor(research_funding_rates$discipline)) %>% paste()



mutate(discipline = factor(discipline,levels=c("Combined",
                                               "Chemical sciences",
                                               "Earth/life sciences",
                                               "Humanities",
                                               "Interdisciplinary",
                                               "Medical sciences",
                                               "Physical sciences",
                                               "Physics",
                                               "Social sciences",
                                               "Technical sciences")))

