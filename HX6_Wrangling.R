# Housekeeping -----------------------------------------------------------------

library('readr')
library('tidyverse')
library('dslabs')
library('Lahman')
library('rvest')

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

# Course 6 - Section 2.2 -------------------------------------------------------

top <- 
  Batting %>%
  filter(yearID == "2016") %>%
  arrange(desc(HR)) %>%
  slice(1:10)
  
top %>%
  as_tibble()
  
Master %>%
  as_tibble()

top_names <-
  top %>%
  left_join(Master, by = "playerID") %>%
  select(playerID,nameFirst,nameLast,HR)
  
top_salary <-
  Salaries %>%
  filter(yearID == "2016") %>%
  right_join(top_names, by = "playerID") %>%
  select(nameFirst, nameLast, teamID, HR, salary)
  
awards <-
AwardsPlayers %>%
  as_tibble() %>%
  filter(yearID == "2016")

intersect(top_names$playerID,awards$playerID)
  
setdiff(awards$playerID,top_names$playerID)

# Course 6 - Section 2.3 -------------------------------------------------------

# baseball payroll stats

url <- 'https://web.archive.org/web/20181024132313/http://www.stevetheump.com/Payrolls.htm'

h <- read_html(url)

nodes <- html_nodes(h,"table")

html_text(nodes[[3]])
html_table(nodes[[8]])


tab1 <- html_table(nodes[10])[[1]][2:31,2:4] %>%
  mutate(Team = X2, Payroll = X3, Average = X4) %>%
  select(Team, Payroll, Average)

tab2 <- html_table(nodes[19])[[1]][2:31,]
names(tab2) <- c("Team","Payroll","Average")

full_join(tab1,tab2,by="Team") %>% nrow()


# brexit referendum

url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
h <- read_html(url)

tab <- html_nodes(h,"table")

html_table(tab,fill=TRUE)[[5]][1,] %>% dim()











