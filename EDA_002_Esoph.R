library('tidyverse')
data('esoph')

head(esoph)
str(esoph)

ggplot(esoph) +
  geom_point(aes(x=agegp,y=ncontrols),color='blue') +
  geom_point(aes(x=agegp,y=ncases),color='red')

nrow(esoph)

all_cases <- select(esoph,ncases) %>% sum()
all_controls <- esoph %>% select(ncontrols) %>% sum()

alc_cases <-  esoph %>% filter(alcgp == '120+') %>% select(ncases) %>% sum()
alc_controls <- esoph %>% filter(alcgp == '120+') %>% select(ncontrols) %>% sum()
alc_cases/(alc_cases+alc_controls)

cases <- esoph %>% 
  filter(ncases > 0) %>%
  select(alcgp,tobgp,ncases)

tab <- rep(cases$tobgp,cases$ncases) %>%
  table() %>%
  prop.table()

tab['30+']

cases_risk <- esoph %>%
  filter(alcgp=='120+' | tobgp=='30+') %>%
  select(ncases) %>%
  sum()

cases_total <- esoph %>%
  select(ncases) %>%
  sum()

cases_risk/cases_total

controls_alc <- esoph %>%
  filter(alcgp=='120+') %>%
  select(ncontrols) %>%
  sum()

controls_tob <- esoph %>%
  filter(tobgp=='30+') %>%
  select(ncontrols) %>%
  sum()

controls_tob_alc <- esoph %>%
  filter(tobgp=='30+' | alcgp=='120+') %>%
  select(ncontrols) %>%
  sum()


controls <- esoph %>%
  select(ncontrols) %>%
  sum()

controls_alc/controls
controls_tob/controls
controls_tob_alc/controls

cases_alc <- esoph %>%
  filter(alcgp == '120+') %>%
  select(ncases) %>%
  sum()

cases_total <- esoph %>%
  select(ncases) %>%
  sum()

(cases_alc/cases_total)/(controls_alc/controls)

# ACT test scores
set.seed(16,sample.kind="Rounding")
act_scores <- rnorm(10000,20.9,5.7)
mean(act_scores)
sd(act_scores)
sum(act_scores>=36)
mean(act_scores>30)
mean(act_scores<=10)
x <- 1:36
f_x <- dnorm(x,20.9,5.7)
plot(x,f_x)
z_act_scores <- (act_scores-mean(act_scores))/sd(act_scores)
mean(z_act_scores > 2)
mean(act_scores)+2*sd(act_scores)
qnorm(0.975,mean(act_scores),sd(act_scores))
F_x <- pnorm(x,20.9,5.7)
which(F_x >= 0.95)
qnorm(0.95,20.9,5.7)
sample_quantiles <- quantile(act_scores,seq(0.01,0.99,0.01))
sample_quantiles['60%']
theoretical_quantiles <- qnorm(seq(0.01,0.99,0.01),20.9,5.7)
plot(theoretical_quantiles,sample_quantiles)

# SAT test scores
set.seed(x, sample.kind = "Rounding")
scores <- replicate(10000,{
  sum(sample(c(1,-0.25),44,replace=TRUE,prob=c(0.2,0.8)))
})
mean(scores>=8)

p <- seq(0.25,0.95,0.05)
exp_p <- 1*p*44
sd_p <- sqrt(p*(1-p))*sqrt(44)
r <- data.frame(skill=p,exp=exp_p,sd=sd_p,p35=1-pnorm(35,exp_p,sd_p))

# Roulette
exp_single <- (5/38)*6 + (33/38)*(-1)
exp_sum500 <- exp_single * 500
se_single <- abs(6--1)*sqrt(5*33/38^2)
se_avg500 <- se_single/sqrt(500)
se_sum500 <- se_single*sqrt(500)
p_losing <- pnorm(0,exp_sum500,se_sum500)

# reference by level number
esoph$agegp == levels(esoph$agegp)[2]
filter(esoph,agegp==levels(agegp)[1])
filter(esoph,agegp==levels(agegp)[nlevels(agegp)])


