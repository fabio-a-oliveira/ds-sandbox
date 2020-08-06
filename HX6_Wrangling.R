# Housekeeping -----------------------------------------------------------------

library('readr')
library('tidyverse')
library('dslabs')
library('Lahman')
library('rvest')
library('stringr')
library('tidytext')
library('scales')
library('rtweet')
library('lubridate')
library('textdata')
library('gutenbergr')
library('pdftools')

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


# Course 6 - Section 3.2 -------------------------------------------------------

list <- c('abc','def','ghi')
pattern <- '(a)[a-z]q(c)'

str_match(list,pattern)
str_replace(list,pattern,"\\1'\\2")

# Course 6 - Section 3.3 -------------------------------------------------------

schedule <- data.frame(day = c("Monday", "Tuesday"),
                       staff = c("Mandy, Cris and Laura","Steve, Ruth and Frank"))


str_split(schedule$staff, ", | and ")
str_split(schedule$staff, ",\\s|\\sand\\s")
str_split(schedule$staff, "\\s?(,|and)\\s?")


tidy_schedule <- schedule %>%
  mutate(staff = str_split(schedule$staff, ",\\s|\\sand\\s", simplify=TRUE)) %>%
  unnest()


schedule %>% as.data.frame() %>% mutate(day = as.factor(day)) %>% 
  mutate(day = recode(day,Monday = "M", Tuesday = "T"))

# Course 6 - Section 3.3 - Tests -----------------------------------------------

url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
tab <- read_html(url) %>% html_nodes("table")
polls <- tab[[5]] %>% html_table(fill = TRUE)

# polls %>% select(-"Conducted by", -"Polling type", -"Notes")

names(polls) <- c("dates", "remain", "leave", "undecided", "lead", "samplesize", "pollster", "poll_type", "notes")

polls_1 <- 
  polls %>%
  filter(str_detect(remain,'%'))

# Course 6 - Section 4 ---------------------------------------------------------

data("trump_tweets")

trump_tweets %>% 
  extract(source, "source", "Twitter for (.*)") %>%
  count(source) 

campaign_tweets <- trump_tweets %>% 
  extract(source, "source", "Twitter for (.*)") %>%
  filter(source %in% c("Android", "iPhone") &
           created_at >= ymd("2015-06-17") & 
           created_at < ymd("2016-11-08")) %>%
  filter(!is_retweet) %>%
  arrange(created_at)

ds_theme_set()
campaign_tweets %>%
  mutate(hour = hour(with_tz(created_at, "EST"))) %>%
  count(source, hour) %>%
  group_by(source) %>%
  mutate(percent = n / sum(n)) %>%
  ungroup %>%
  ggplot(aes(hour, percent, color = source)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = percent_format()) +
  labs(x = "Hour of day (EST)",
       y = "% of tweets",
       color = "")

i <- 3008
campaign_tweets$text[i]
campaign_tweets[i,] %>% 
  unnest_tokens(word, text) %>%
  select(word)

pattern <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"

campaign_tweets[i,] %>% 
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  select(word)

campaign_tweets[i,] %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  select(word)

tweet_words <- campaign_tweets %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) 

tweet_words %>% 
  count(word) %>%
  arrange(desc(n))
  
tweet_words <- campaign_tweets %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  filter(!word %in% stop_words$word ) 

tweet_words %>% 
  count(word) %>%
  top_n(10, n) %>%
  mutate(word = reorder(word, n)) %>%
  arrange(desc(n))

tweet_words <- campaign_tweets %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  filter(!word %in% stop_words$word &
           !str_detect(word, "^\\d+$")) %>%
  mutate(word = str_replace(word, "^'", ""))

android_iphone_or <- tweet_words %>%
  count(word, source) %>%
  spread(source, n, fill = 0) %>%
  mutate(or = (Android + 0.5) / (sum(Android) - Android + 0.5) / 
           ( (iPhone + 0.5) / (sum(iPhone) - iPhone + 0.5)))
android_iphone_or %>% arrange(desc(or))
android_iphone_or %>% arrange(or)

android_iphone_or %>% filter(Android+iPhone > 100) %>%
  arrange(desc(or))

android_iphone_or %>% filter(Android+iPhone > 100) %>%
  arrange(or)

get_sentiments("bing")
get_sentiments("afinn")
get_sentiments("loughran")
get_sentiments("nrc")

nrc <- get_sentiments("nrc") %>%
  select(word, sentiment)

tweet_words %>% inner_join(nrc, by = "word") %>% 
  select(source, word, sentiment)

sentiment_counts <- tweet_words %>%
  left_join(nrc, by = "word") %>%
  count(source, sentiment) %>%
  spread(source, n) %>%
  mutate(sentiment = replace_na(sentiment, replace = "none"))
sentiment_counts

tweet_words %>% group_by(source) %>% summarize(n = n())

sentiment_counts %>%
  mutate(Android = Android / (sum(Android) - Android) , 
         iPhone = iPhone / (sum(iPhone) - iPhone), 
         or = Android/iPhone) %>%
  arrange(desc(or))

library(broom)
log_or <- sentiment_counts %>%
  mutate( log_or = log( (Android / (sum(Android) - Android)) / (iPhone / (sum(iPhone) - iPhone))),
          se = sqrt( 1/Android + 1/(sum(Android) - Android) + 1/iPhone + 1/(sum(iPhone) - iPhone)),
          conf.low = log_or - qnorm(0.975)*se,
          conf.high = log_or + qnorm(0.975)*se) %>%
  arrange(desc(log_or))

log_or

log_or %>%
  mutate(sentiment = reorder(sentiment, log_or),) %>%
  ggplot(aes(x = sentiment, ymin = conf.low, ymax = conf.high)) +
  geom_errorbar() +
  geom_point(aes(sentiment, log_or)) +
  ylab("Log odds ratio for association between Android and sentiment") +
  coord_flip() 

android_iphone_or %>% inner_join(nrc) %>%
  filter(sentiment == "disgust" & Android + iPhone > 10) %>%
  arrange(desc(or))

android_iphone_or %>% inner_join(nrc, by = "word") %>%
  mutate(sentiment = factor(sentiment, levels = log_or$sentiment)) %>%
  mutate(log_or = log(or)) %>%
  filter(Android + iPhone > 10 & abs(log_or)>1) %>%
  mutate(word = reorder(word, log_or)) %>%
  ggplot(aes(word, log_or, fill = log_or < 0)) +
  facet_wrap(~sentiment, scales = "free_x", nrow = 2) + 
  geom_bar(stat="identity", show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

# Course 6 - Section 4 - Assessment --------------------------------------------

data(brexit_polls)

brexit_polls %>%
  mutate(month = month(startdate)) %>%
  group_by(month) %>%
  summarize(n())

brexit_polls %>%
  mutate(round_week = round_date(enddate, unit = "week")) %>%
  group_by(round_week) %>%
  summarize(n()) %>%
  filter(round_week == "2016-06-12")

brexit_polls %>%
  mutate(end_weekday = weekdays(enddate)) %>%
  group_by(end_weekday) %>%
  summarize(n())

dslabs('movielens')

names(movielens)
str(movielens)

movielens <- 
  movielens %>%
  mutate(timestamp = as_datetime(timestamp))

movielens %>%
  mutate(year = year(timestamp)) %>%
  group_by(year) %>%
  summarize(n()) %>%
  arrange(desc(`n()`))

movielens %>%
  mutate(hour = hour(timestamp)) %>%
  group_by(hour) %>%
  summarize(n()) %>%
  arrange(desc(`n()`))


# Course 6 - Section 4 - Assessment 2 ------------------------------------------

options(digits = 3)

gutenberg_metadata

gutenberg_metadata %>%
  filter(str_detect(title,"Pride and Prejudice"))

gutenberg_works(title == "Pride and Prejudice")

download <- 
  gutenberg_download(1342)

words <- 
  download %>%
  unnest_tokens(word,text) %>%
  filter(!(word %in% stop_words$word)) %>%
  filter(!str_detect(word,'\\d')) %>%
  select(-gutenberg_id)

words %>%
  group_by(word) %>%
  summarize(appearances = n()) %>%
  arrange(desc(appearances)) %>%
  filter(appearances >= 100)

afinn <- get_sentiments('afinn')  

afinn_sentiments <-
  inner_join(words,afinn,by='word')

afinn_sentiments %>%
  mutate(positive = value > 0) %>%
  summarize(proportion = mean(positive))

afinn_sentiments %>%
  filter(value == 4)

# Assessment: Puerto Rico Hurricane Mortality ----------------------------------

options(digits = 3)

fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")
# system("cmd.exe", input = paste("start", fn))

txt <- pdf_text(fn)

x <-
  txt[9] %>%
  str_split(pattern = '\\n')

s <- x[[1]]
s <- 
  s %>%
  str_trim()

header_index <- str_which(s,'2015')[1]

header <- 
  s[header_index] %>%
  str_replace_all('[A-Z]{3}\\s*','') %>%
  str_split('\\s+', simplify = TRUE)

month <- 
  s[header_index] %>%
  str_extract('[A-Z]{3}')
  
tail_index <- 
  s %>%
  str_which('Total')

s %>% str_count('\\d+') %>% table()

s <- 
  s[-c(1:header_index,tail_index:length(s))]

s <-
  s[str_count(s,'\\d+') != 1]
 
s <- str_remove_all(s,'[^\\d\\s]')

s <- str_split_fixed(s, "\\s+", n = 6)[,1:5]

tab <- s %>%
  as_data_frame()

names(tab) <- c("day",header)

tab <- 
  tab %>%
  mutate_all(as.numeric) %>%
  mutate(month = month)

tab %>%
  summarize_at(header,mean)

tab %>%
  filter(day <= 19) %>%
  summarize_at(header,mean)

tab %>%
  filter(day > 19) %>%
  summarize_at(header,mean)

tab <- tab %>% gather(year, deaths, -day, -month) %>%
  mutate(deaths = as.numeric(deaths))


tab %>%
  ggplot(aes(x=day,y=deaths,color=year)) +
  geom_line()

tab %>%
  filter(deaths >= 100)




