# Housekeeping -----------------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(Lahman)
library(HistData)
library(broom)
library(dslabs)

# Course 7 - Section 1.1 -------------------------------------------------------

Teams %>%
  filter(yearID >= 1961 & yearID <= 2001) %>%
  mutate(Runs_per_Game = R / G,
         At_Bats_per_Game = AB / G) %>%
  ggplot(aes(At_Bats_per_Game,Runs_per_Game)) +
  geom_point(alpha = .5)

Teams %>%
  filter(yearID >= 1961 & yearID <= 2001) %>%
  mutate(Wins_per_Game = W / G,
         Fielding_Errors_per_Game = E / G) %>%
  ggplot(aes(Fielding_Errors_per_Game,Wins_per_Game)) +
  geom_point(alpha = .5)

Teams %>%
  filter(yearID >= 1961 & yearID <= 2001) %>%
  mutate(Triples_per_Game = X3B / G,
         Doubles_per_Game = X2B / G) %>%
  ggplot(aes(Doubles_per_Game,Triples_per_Game)) +
  geom_point(alpha = .5)

Teams %>%
  filter(yearID %in% 1961:2001) %>%
  mutate(Runs_per_Game = R / G,
         At_Bats_per_Game = AB / G,
         Wins_per_Game = W / G,
         Fielding_Errors_per_Game = E / G,
         Triples_per_Game = X3B / G,
         Doubles_per_Game = X2B / G) %>%
  summarize(correlation_Runs_AB = cor(Runs_per_Game,At_Bats_per_Game),
            correlation_Errors_Wins = cor(Wins_per_Game,Fielding_Errors_per_Game),
            correlation_Triples_Doubles = cor(Triples_per_Game,Doubles_per_Game))

# Course 7 - Section 1.3 -------------------------------------------------------

set.seed(1989, sample.kind="Rounding")
data("GaltonFamilies")

female_heights <- GaltonFamilies%>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)

female_heights %>%
  summarize(mean_mother = mean(mother),
            sd_mother = sd(mother),
            mean_daughter = mean(daughter),
            sd_daughter = sd(daughter),
            correlation = cor(mother,daughter)) %>%
  mutate(slope_m2d = correlation * sd_daughter / sd_mother,
         intercept_m2d = mean_daughter - correlation * sd_daughter/sd_mother * mean_mother,
         delta_daughter = (1 / sd_mother)*sd_daughter*correlation,
         percentage = round((correlation^2)*100,0),
         exp_conditional_60 = mean_daughter + correlation*(60-mean_mother)*sd_daughter/sd_mother)

# Course 7 - Section 2.2 -------------------------------------------------------


set.seed(1983, sample.kind="Rounding")
galton_heights <- GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

rss <- function(beta0, beta1, data){
  resid <- galton_heights$son - (beta0+beta1*galton_heights$father)
  return(sum(resid^2))
}

beta1 = seq(0, 1, len=nrow(galton_heights))
results <- data.frame(beta1 = beta1,
                      rss = sapply(beta1, rss, beta0 = 36))
results %>% ggplot(aes(beta1, rss)) + geom_line() + 
  geom_line(aes(beta1, rss), col=2)


Teams %>%
  filter(yearID %in% 1961:2001) %>%
  mutate(R_G = R / G,
         BB_G = BB / G,
         HR_G = HR / G) %>%
  lm(R_G ~ BB_G + HR_G, data = .) %>%
  predict()


model <- lm(son ~ father, data = galton_heights)
predictions <- predict(model)
data <- as.tibble(predictions) %>% bind_cols(father = galton_heights$father)

ggplot(data, aes(x = father, y = fit)) +
  geom_line(color = "blue", size = 1) + 
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2) + 
  geom_point(data = galton_heights, aes(x = father, y = son))


set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
options(digits = 3)    # report 3 significant digits

female_heights <- GaltonFamilies %>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)

model <- lm(mother ~ daughter, data = female_heights)
predict(model)

# Lahman
bat_02 <- Batting %>% filter(yearID == 2002) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb)

bat_01 <- Batting %>% filter(yearID %in% 1999:2001) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb, yearID)

bat_01 %>% 
  group_by(playerID) %>% 
  summarize(mean_singles = mean(singles)) %>% 
  filter(mean_singles > .2) %>%
  nrow()

bat_all <-
  bat_01 %>% 
  group_by(playerID) %>% 
  summarize(bb = mean(bb),
            singles = mean(singles)) %>% 
  inner_join(bat_02,by="playerID",suffix = c(".ref",".actual"))

bat_all %>%
  summarize(correlation_singles = cor(singles.ref,singles.actual),
            correlation_bb = cor(bb.ref, bb.actual))

bat_all %>%
  gather(key = "stat.name",
         value = "stat.ref",
         c("bb.ref","singles.ref")) %>%
  mutate(stat.actual = ifelse(stat.name=="bb.ref",bb.actual,singles.actual)) %>%
  select(-singles.actual,-bb.actual) %>%
  ggplot(aes(x=stat.ref,y=stat.actual)) +
  geom_point() +
  geom_smooth(method='lm') +
  facet_grid(stat.name ~.)
         
model <- 
  lm(singles.actual ~ singles.ref, bat_all)

model <- 
  lm(bb.actual ~ bb.ref , bat_all)

bat_all %>%
  gather(key = "stat.name",
         value = "stat.value",
         -playerID) %>%
  sample_n(10)

# Course 7 - Section 2.3 -------------------------------------------------------

set.seed(1, sample.kind = "Rounding") # if you are using R 3.6 or later
galton <- GaltonFamilies %>%
  group_by(family, gender) %>%
  sample_n(1) %>%
  ungroup() %>% 
  gather(parent, parentHeight, father:mother) %>%
  mutate(child = ifelse(gender == "female", "daughter", "son")) %>%
  unite(pair, c("parent", "child"))

galton %>%
  group_by(pair) %>%
  summarize(n())

galton %>%
  filter(pair == 'father_daughter') %>%
  lm(childHeight ~ parentHeight, data = .) %>%
  summary

galton %>%
  filter(pair == 'mother_son') %>%
  lm(childHeight ~ parentHeight, data = .) %>%
  summary

galton %>%
  group_by(pair) %>%
  do(tidy(lm(childHeight ~ parentHeight, data = .))) %>%
  filter(term == 'parentHeight')

galton %>%
  group_by(pair) %>%
  do(tidy(lm(childHeight ~ parentHeight, data = .))) %>%
  mutate(ci_lower = estimate-2*std.error,
         ci_upper = estimate+2*std.error) %>%
  filter(term == "parentHeight") %>%
  ggplot(aes(x=pair,y=estimate)) +
  geom_point() + 
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper))

galton %>%
  filter(pair == "mother_son") %>%
  summarize(mean_mother = mean(parentHeight),
            sd_mother = sd(parentHeight),
            mean_son = mean(childHeight),
            sd_son = sd(childHeight))
  
galton %>%
  group_by(pair) %>%
  summarize(correlation = cor(childHeight,parentHeight))

m <-
galton %>%
  group_by(pair) %>%
  do(tidy(lm(childHeight ~ parentHeight, data = .)))

model <- lm(childHeight ~ parentHeight, data = galton)
summary(model)

# Course 7 - Section 2.4 -------------------------------------------------------

data(Teams)
data(Batting)

Teams <- 
  Teams %>%
  as_tibble() %>%
  filter(yearID %in% 2002)

Teams %>%
  mutate(pa_game = (AB+BB)/G) %>%
  summarize(pa_per_game = mean(pa_game))

pa_per_game <- Batting %>% 
  filter(yearID == 2002) %>% 
  group_by(teamID) %>%
  summarize(pa_per_game = sum(AB+BB)/max(G)) %>% 
  .$pa_per_game %>% 
  mean

TeamA <-
  data.frame(BB = 2,
             H = 4,
             X2B = 1,
             X3B = 0,
             HR = 1)

TeamB <-
  data.frame(BB = 1,
             H = 6,
             X2B = 2,
             X3B = 1,
             HR = 0)

bothTeams <- bind_rows(TeamA,TeamB)


model <- 
Batting %>%
  filter(yearID %in% 1961:2020) %>%
  mutate(PA = AB+BB) %>%
  select(playerID,yearID,PA,BB,H,X2B,X3B,HR,R,G) %>%
  group_by(playerID) %>%
  summarize(PA = sum(PA),
            BB = sum(BB)/sum(PA) * pa_per_game,
            H = sum(H-X2B-X3B-HR)/sum(PA) * pa_per_game,
            X2B = sum(X2B)/sum(PA) * pa_per_game,
            X3B = sum(X3B)/sum(PA) * pa_per_game,
            HR = sum(HR)/sum(PA) * pa_per_game,
            R = sum(R)/sum(PA) * pa_per_game) %>%
  filter(PA >= 100) %>%
  select(-PA) %>%
  ungroup() %>%
  lm(R ~ BB + H + X2B + X3B + HR, data = .)

coefs <- tidy(model, conf.int = TRUE)

bothTeams %>%
  mutate(R_hat = predict(model, newdata = .))

bothTeams %>%
  mutate(singles = H,
         doubles = X2B,
         triples = X3B) %>%
  mutate(R_hat = predict(fit, newdata = .))

model2 <- 
  Teams %>%
  filter(yearID %in% 1961:2020) %>%
  mutate(PA = AB+BB) %>%
  select(teamID,yearID,PA,BB,H,X2B,X3B,HR,R,G) %>%
  group_by(teamID) %>%
  summarize(PA = sum(PA),
            BB = sum(BB)/sum(PA) * pa_per_game,
            H = sum(H-X2B-X3B-HR)/sum(PA) * pa_per_game,
            X2B = sum(X2B)/sum(PA) * pa_per_game,
            X3B = sum(X3B)/sum(PA) * pa_per_game,
            HR = sum(HR)/sum(PA) * pa_per_game,
            R = sum(R)/sum(PA) * pa_per_game) %>%
  filter(PA >= 100) %>%
  select(-PA) %>%
  ungroup() %>%
  lm(R ~ BB + H + X2B + X3B + HR, data = .)

bothTeams %>%
  mutate(R_hat = predict(model2, newdata = .))

bothTeams %>%
  mutate(R_hat = predict(model, newdata = .))

bothTeams %>%
  mutate(singles = H,
         doubles = X2B,
         triples = X3B) %>%
  mutate(R_hat = predict(fit, newdata = .))


# BB / HR on R

model <- 
Teams %>%
  filter(yearID == 1971) %>%
  select(teamID,BB,HR,R) %>%
  lm(R ~ BB + HR, data = .)
  
tidy(model, conf.int = TRUE)

Teams %>%
  filter(yearID %in% 1961:2018) %>%
  select(yearID,teamID,BB,HR,R) %>%
  group_by(yearID) %>%
  do(tidy(lm(R ~ BB + HR, data = .), conf.int = TRUE)) %>%
  filter(term == 'BB') %>%
  ggplot(aes(x = yearID, y = estimate)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high))

Teams %>%
  filter(yearID %in% 1961:2018) %>%
  select(yearID,teamID,BB,HR,R) %>%
  group_by(yearID) %>%
  do(tidy(lm(R ~ BB + HR, data = .), conf.int = TRUE)) %>%
  filter(term == 'BB') %>%
  lm(estimate ~ yearID, data = .) %>%
  tidy(conf.int = TRUE)


# Course 7 - Section 2 - Assessment --------------------------------------------

data(Teams)

Teams_small <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(avg_attendance = attendance/G)

Teams_small %>%
  mutate(avg_Runs = R/G) %>%
  select(yearID,teamID,avg_Runs,avg_attendance) %>%
  lm(avg_attendance ~ avg_Runs, data = .) %>%
  tidy(conf.int = TRUE)

Teams_small %>%
  mutate(avg_HR = HR/G) %>%
  select(yearID,teamID,avg_HR,avg_attendance) %>%
  lm(avg_attendance ~ avg_HR, data = .) %>%
  tidy(conf.int = TRUE)

Teams_small %>%
  select(yearID,teamID,W,avg_attendance) %>%
  lm(avg_attendance ~ W, data = .) %>%
  tidy(conf.int = TRUE)

Teams_small %>%
  select(yearID,teamID,avg_attendance) %>%
  lm(avg_attendance ~ yearID, data = .) %>%
  tidy(conf.int = TRUE)

Teams_small %>%
  mutate(avg_Runs = R/G) %>%
  summarize(Runs = mean(avg_Runs),
            Attendance = mean(avg_attendance))

Teams_small %>%
  summarize(cor_W_R = cor(W,R/G),
            cor_W_HR = cor(W,HR/G))

Teams_small %>%
  mutate(W_strata = round(W/10,0)*10,
         avg_R = R / G) %>%
  group_by(W_strata) %>%
  do(tidy(lm(avg_attendance ~ avg_R,data = .),conf.int = FALSE)) %>%
  filter(term == 'avg_R')

Teams_small %>%
  mutate(W_strata = round(W/10,0)*10,
         avg_HR = HR / G) %>%
  group_by(W_strata) %>%
  do(tidy(lm(avg_attendance ~ avg_HR,data = .),conf.int = FALSE)) %>%
  filter(term == 'avg_HR')

model <- 
Teams_small %>%
  mutate(avg_attendance = attendance/G,
         avg_R = R/G,
         avg_HR = HR/G) %>%
  lm(avg_attendance ~ avg_R + avg_HR + W + yearID, data = .)

predict(model, data.frame(avg_R = 5,
                          avg_HR = 1.2,
                          W = 80,
                          yearID = 1960))

data(Teams)
Teams %>%
  filter(yearID %in% 2002) %>%
  mutate(avg_attendance = attendance/G,
         avg_HR = HR/G,
         avg_R = R/G) %>%
  select(teamID,avg_attendance,avg_R,avg_HR,W) %>%
  mutate(yearID = 2002) %>%
  mutate(predicted_attendance = predict(model,.)) %>%
  summarize(correlation = cor(avg_attendance,predicted_attendance))

predict(model,Teams)

#   slice_max(n=1,order_by=estimate)

# Course 7 - Section 3 ---------------------------------------------------------

data("research_funding_rates")
research_funding_rates

two_by_two <- 
research_funding_rates %>%
  summarise(yay_men = sum(awards_men),
            nay_men = sum(applications_men) - sum(awards_men),
            yay_women = sum(awards_women),
            nay_women = sum(applications_women) - sum(awards_women)) %>%
  transmute(men = str_c(yay_men,nay_men, sep = ","),
            women = str_c(yay_women,nay_women, sep = ",")) %>%
  gather(key = 'gender',
         value = 'yaynay',
         men:women) %>%
  transmute(gender = gender,
            yay = str_split(yaynay,',',n=2,simplify=TRUE)[,1],
            nay = str_split(yaynay,',',n=2,simplify=TRUE)[,2]) %>%
  mutate(yay = as.numeric(yay),
         nay = as.numeric(nay))

two_by_two %>%
  group_by(gender) %>%
  summarise(percentage_awarded = 100*yay/(yay+nay))

two_by_two %>%
  select(-gender) %>%
  chisq.test() %>%
  tidy()

dat <- research_funding_rates %>% 
  mutate(discipline = reorder(discipline, success_rates_total)) %>%
  rename(success_total = success_rates_total,
         success_men = success_rates_men,
         success_women = success_rates_women) %>%
  gather(key, value, -discipline) %>%
  separate(key, c("type", "gender")) %>%
  spread(type, value) %>%
  filter(gender != "total")

dat %>%
  ggplot(aes(y = discipline, x = success, color=gender)) +
  geom_point()

dat %>%
  ggplot(aes(y = discipline, x = applications, color=gender)) +
  geom_point()

dat %>%
  group_by(discipline) %>%
  summarize(application = sum(applications),
            awards = sum(awards),
            success = sum(awards)/sum(applications)) %>%
  arrange(success)


# Test -------------------------------------------------------------------------


data(Teams)
data(Batting) 



# regression with BB, singles, doubles, triples, HR
fit <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(BB = BB / G, 
         singles = (H - X2B - X3B - HR) / G, 
         doubles = X2B / G, 
         triples = X3B / G, 
         HR = HR / G,
         R = R / G) %>%  
  lm(R ~ BB + singles + doubles + triples + HR, data = .)
coefs <- tidy(fit, conf.int = TRUE)
coefs

# predict number of runs for each team in 2002 and plot
Teams %>% 
  filter(yearID %in% 2002) %>% 
  mutate(BB = BB/G, 
         singles = (H-X2B-X3B-HR)/G, 
         doubles = X2B/G, 
         triples =X3B/G, 
         HR=HR/G,
         R=R/G)  %>%
  select(BB, singles, doubles, triples, HR) %>%
  mutate(R_hat = predict(fit, newdata = .)) %>%
  ggplot(aes(R_hat, R, label = teamID)) + 
  geom_point() +
  geom_text(nudge_x=0.1, cex = 2) + 
  geom_abline()


a <- Teams %>% 
  filter(yearID %in% 2002) %>% 
  mutate(BB = BB/G, 
         singles = (H-X2B-X3B-HR)/G, 
         doubles = X2B/G, 
         triples =X3B/G, 
         HR=HR/G,
         R=R/G)  %>%
  select(BB, singles, doubles, triples, HR)



research_funding_rates %>%
  select(-applications_total,-awards_total,-success_rates_total) %>%
  rename(success_men = success_rates_men,
         success_women = success_rates_women) %>%
  gather(key,value,-discipline) %>%
  separate(key,into=c('what','who')) %>%
  spread(what,value)
