# start-up
library('dbplyr')
library('datasets')
library('ggplot2')
library('tidyverse')
data('msleep')

msleep = mutate(msleep,name=factor(name),
                       genus=factor(genus),
                       vore=factor(vore),
                       order=factor(order),
                       conservation=factor(conservation),
                       brain_ratio = brainwt/bodywt)

# begin analysis
msleep <- arrange(msleep,-sleep_total)

msleep %>% .$order %>% table() %>% prop.table()

msleep %>% group_by(order) %>%
           summarise(avg_sleep=mean(sleep_total)) %>%
           arrange(avg_sleep)

msleep %>%
  ggplot(aes(x=bodywt,y=sleep_total,color=order)) +
  geom_boxplot() + 
  geom_point() +
  scale_x_continuous(trans='log10')

# reorder - ordena todas as colunas de acordo com o crit?rio usado para 1 delas
msleep %>%
  mutate(order = reorder(order,sleep_total,median)) %>%
  ggplot(aes(x=order,y=sleep_total,color=order)) +
  geom_boxplot() + 
  geom_point()


#
msleep %>%
  ggplot(aes(x=sleep_total)) +
  geom_density(adjust=1/8,color='red') + 
  geom_density()

msleep %>%
  ggplot(aes(x=name,y=sleep_total/(brainwt/bodywt),color=order)) +
  geom_point() +
  scale_y_continuous(trans='identity')

msleep %>%
  ggplot(aes(x=vore,y=sleep_total/(brainwt/bodywt))) +
  geom_boxplot()

# brain to body weight analysis

msleep_filtered <- msleep %>%
  filter(!is.na(bodywt) & !is.na(brainwt)) %>%
  mutate(brain_ratio = brainwt/bodywt)

bodywt_avg_log10 <- msleep_filtered$bodywt %>%
                     log10() %>%
                     mean()

bodywt_dev_log10 <- msleep_filtered$bodywt %>%
                    log10()-bodywt_avg_log10

brainwt_avg_log10 <- msleep_filtered$brainwt %>%
                     log10() %>%
                     mean()

brainwt_dev_log10 <- msleep_filtered$brainwt %>%
                     log10()-brainwt_avg_log10

m <- mean(brainwt_dev_log10/bodywt_dev_log10)

exp_bodywt = 10^seq(-3,4,length.out = 100)
exp_brainwt = 10^(brainwt_avg_log10 + m*(log10(exp_bodywt) - bodywt_avg_log10))

exp_wt <- data.frame(bodywt = exp_bodywt, brainwt = exp_brainwt)

msleep_filtered <- mutate(msleep_filtered, exp_brainwt = 10^(brainwt_avg_log10 + m*(log10(bodywt) - bodywt_avg_log10)))
msleep_filtered <- mutate(msleep_filtered, dev_brainwt = 1 + (brainwt-exp_brainwt)/exp_brainwt)
    
msleep_filtered %>%
  ggplot(aes(bodywt,brainwt)) +
  geom_point(color='black') +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  geom_line(data=exp_wt,aes(x=exp_bodywt,y=exp_brainwt),color='red')

msleep_human <- filter(msleep_filtered, name=="Human")

msleep_filtered %>%
  ggplot(aes(bodywt,brainwt)) +
  geom_point(color='black') +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  geom_line(aes(x=bodywt,y=exp_brainwt),color='red') +
  geom_point(data=msleep_human,aes(bodywt,brainwt),color='blue') +
  geom_text(data=msleep_human,aes(bodywt,brainwt),label='Human',color='blue',nudge_y=0.5)

msleep_filtered %>%
  ggplot(aes(dev_brainwt,sleep_total))+
  geom_point() +
  scale_x_continuous(trans='log10')

msleep_primates = filter(msleep_filtered,order=='Primates')

msleep_filtered %>%
  ggplot(aes(brain_ratio,sleep_total)) +
  geom_point() + 
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  geom_point(data=msleep_primates,aes(brain_ratio,sleep_total),color='red') +
  geom_point(data=msleep_human,aes(brain_ratio,sleep_total),color='blue',size=3) +
  geom_label(data=msleep_human,aes(brain_ratio,sleep_total,label=name),color='blue',nudge_x=0.1,nudge_y=-0.03)
  


# support

length(levels(msleep$order))
table(msleep$order)

match('Cow',msleep$name)

prim <- msleep$order=='Primates'
