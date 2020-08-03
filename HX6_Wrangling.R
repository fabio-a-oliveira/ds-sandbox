# Housekeeping -----------------------------------------------------------------

library('readr')
library('tidyverse')
library('dslabs')

# Course 6 - Section 1 ---------------------------------------------------------

dir <- getwd()
dir
setwd(dir)
getwd()
setwd(~asdff)

URL <- "https://raw.githubusercontent.com/rasbt/python-machine-learning-book/master/code/datasets/wdbc/wdbc.data"
data <- read_csv(URL,col_names = FALSE)


# Course 6 - Section 2.1 -------------------------------------------------------

co2_wide <- data.frame(matrix(co2, ncol = 12, byrow = TRUE)) %>% 
  setNames(1:12) %>%
  mutate(year = as.character(1959:1997))

co2_tidy <- co2_wide %>%
  gather(key = "month",
         value = "co2",
         -year)

co2_tidy %>%
  mutate(month = as.numeric(month)) %>%
  group_by(year) %>%
  ggplot(aes(x=month,y=co2,color=year)) +
  geom_line()

co2_tidy %>%
  group_by(year) %>%
  summarize(mean = mean(co2)) %>% plot()

data('admissions')
data <- admissions %>% select(-applicants) %>%
  spread(key = gender, value = admitted)

data('admissions')
tmp <- 
  admissions %>% 
  gather(key,value,admitted:applicants)
tmp

tmp2 <- unite(tmp,column_name,c(key,gender))
tmp2
