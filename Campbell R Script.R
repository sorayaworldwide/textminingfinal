library(rtweet)
library(writexl)
library(readxl)
library(tidyverse)
library(tidytext)
library(gganimate)
library(ggwordcloud)
library(stringr)
library(tidyr)
library(scales)
library(wordcloud2)
library(lubridate)
library(purrr)
library(broom)
library(vader)
library(tweetrmd)
##store api keys
app_name<-"TMdukeglobaled"
api_key<-"oJQwW4AKhHhde5jTm6p8vygfw"
api_secret_key<-"DAqTdDxnWxNRctBKnt0ujoXn38jtrxxwz31zwQI3VpXcHS69wc"
access_token<-"293694037-Ex0EnQydER3ujJ5xxogNJcAlwaGItlBJGYgdBzlj"
access_token_secret<-"gz5fLkIRkZDbM8NS2pIu24lfEwp40854yg0ONyL609VJS"
##authenticate via web browser
token <- create_token(
  app = app_name,
  consumer_key = api_key,
  consumer_secret = api_secret_key,
  access_token = access_token,
  access_secret = access_token_secret)
##get timelines
tmls <- get_timelines(c("dukeu", "ncstate", "unc"), n = 3200)
glimpse(tmls)
tmls
## Count tweets - Duke 3199, UNC 3199, NC State - 3197
tmls %>%
  count(screen_name, sort=TRUE)
##min and max date range
library(lubridate)
tmls %>%
  summarise(min=min(created_at),
            max=max(created_at))
##filter to look at tweets only from past year
tmlsyr<-tmls%>%
  filter(created_at>="2020-04-25")
## min and max date range
tmlsyr%>%
  summarise(min=min(created_at),
            max=max(created_at))
## Count tweets by year time frame - NC State 3057, UNC 2435, Duke 2030
tmlsyrcount <- tmlsyr %>%
  count(screen_name, sort=TRUE)
##ts_plot by screen name
  ts_plot(group_by(tmlsyr, screen_name), "months")
    
  
  
##save tweets
  write_xlsx(tmlsyr, "UniversityTweets.xlsx")
  write_xlsx(tmls, "UniversityTweetsorg.xlsx")

##histogram plot of time facet   
  histogramplot <-ggplot(tmlsyr, aes(x = created_at, fill = screen_name)) +
    geom_histogram(position = "identity", bins = 20, show.legend = FALSE) +
    facet_wrap(~screen_name, ncol = 1)  

##average per group
countweetsbymonth<-tmlsyr %>%
  mutate(month = format(created_at, "%m"), year = format(created_at, "%Y")) %>%
  group_by(screen_name, month, year)%>%
  count(screen_name)

countweetsbymonth2<-tmlsyr %>%
  mutate(month = format(created_at, "%m"), year = format(created_at, "%Y")) %>%
  group_by(month, year)%>%
  count(screen_name)
  
averagetwt<-countweetsbymonth2%>%
    group_by(screen_name)%>%
    summarise(avg= round(mean(n),0))%>%
    arrange(desc(avg))

countweetsbymonth2%>%
  group_by(screen_name)%>%
  summarise(avg= round(mean(n),0))%>%
  arrange(desc(avg))%>%
  kbl()%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = FALSE, position = "left")
##NC State tweets September and March 
NCStateMarch<-tmlsyr%>%
  filter(screen_name=="NCState", created_at>="2021-03-01" & created_at <="2021-03-31")%>%
  select(text, created_at)

NCStateSeptember<-tmlsyr%>%
  filter(screen_name=="NCState", created_at>="2020-09-01" & created_at <="2020-09-30")%>%
  select(text, created_at)

NCStateMarch%>%
  filter(grepl("#GivingPack", text))%>%
  sample_n(10)

NCStateSeptember%>%
  filter(grepl("#GivingPack", text))%>%
  sample_n(10)

##tokenize list
  ##some clean up alla Silge and Robinson
remove_reg <- "&amp;|&lt;|&gt;"
tidy_tweets <- tmlsyr %>% 
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_remove_all(text, remove_reg)) %>%
  unnest_tokens(word, text, token = "tweets") %>%
  filter(!word %in% stop_words$word,
         !word %in% str_remove_all(stop_words$word, "'"),
         str_detect(word, "[a-z]"))

frequency <-tidy_tweets%>%
  group_by(screen_name)%>%
  count(word, sort=TRUE)%>%
  left_join(tidy_tweets %>%
              group_by(screen_name)%>%
              summarise(total=n()))%>%
  mutate(freq = n/total)

frequency <- frequency %>% 
  select(screen_name, word, freq) %>% 
  pivot_wider(names_from = screen_name, values_from = freq) %>%
  arrange(NCState, UNC, DukeU)

##words most and less frequently used by both universities 
NCStateUNCcompfreq<-ggplot(frequency, aes(NCState, UNC)) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red")

UNCDukecompfreq <-ggplot(frequency, aes(UNC, DukeU)) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red")    

UNCDukecompfreq

DukeNCStatecompfreq <-ggplot(frequency, aes(NCState, DukeU)) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red")  

DukeNCStatecompfreq

##tf-idf 
tmlsyr_words <- tmlsyr %>%
  unnest_tokens(word, text) %>%
  count(screen_name, word, sort = TRUE)

total_words <-tmlsyr_words%>%
  group_by(screen_name) %>%
  summarise(total = sum(n))

tmlsyr_totals <- left_join(tmlsyr_words, total_words)

tmlsyr_tf_idf <- tmlsyr_totals %>%
  bind_tf_idf(word, screen_name, n)

##counts between all universities 
tweets_count <-count(tidy_tweets, word, sort = TRUE)

wordcloud2(tweets_count)

tweets_count%>%
  top_n(20)%>%
  mutate(word = reorder(word, n))%>%
  ggplot(aes(n, word, fill=n))+
  geom_col()

##count Duke
Duketweets_count <-tidy_tweets%>%
  filter(screen_name=="DukeU")%>%
  count(word, sort=TRUE)

wordcloud2(Duketweets_count)
wordcloud2(Duketweets_count, size=1.6, color=rep_len(c("blue", "grey"), nrow(Duketweets_count)))

Duketweets_count

## count NC state
NCStatetweets_count <-tidy_tweets%>%
  filter(screen_name=="NCState")%>%
  count(word, sort=TRUE)

wordcloud2(NCStatetweets_count, size=1.6, color=rep_len(c("red", "black"), nrow(NCStatetweets_count)))

NCStatetweets_count

## count UNC
UNCtweets_count <-tidy_tweets%>%
  filter(screen_name=="UNC")%>%
  count(word, sort=TRUE)

wordcloud2(UNCtweets_count, size=1.6, color=rep_len(c("cyan", "grey"), nrow(UNCtweets_count)))

UNCtweets_count

## facet wrap plot of top terms idf
tf_idfplot <-tmlsyr_tf_idf %>%
  group_by(screen_name) %>%
  slice_max(tf_idf, n = 10) %>%
  ungroup() %>%
  mutate(screen_name=as.factor(screen_name),
         word=reorder_within(word, tf_idf, screen_name)) %>%
  ggplot(aes(word, tf_idf, fill = screen_name)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~screen_name, ncol = 3, scales = "free") +
  coord_flip() +
  scale_x_reordered() +
  labs(title = "Top 10 Words Unique to Each University's Tweets Apr 2020- Apr 2021", x = "tf-idf value", y = NULL)

tf_idfplot
##ggwordcloud

##words by time

words_by_time <- tidy_tweets %>%
  filter(!str_detect(word, "^@")) %>%
  mutate(time_floor = floor_date(created_at, unit = "1 month")) %>%
  count(time_floor, screen_name, word) %>%
  group_by(screen_name, time_floor) %>%
  mutate(time_total = sum(n)) %>%
  group_by(screen_name, word) %>%
  mutate(word_total = sum(n)) %>%
  ungroup() %>%
  rename(count = n) %>%
  filter(word_total > 30)

words_by_time

## Duke Time
Duketime <-words_by_time%>%
  filter(screen_name=="DukeU")

gg <- Duketime %>%
  ggplot(aes(label = word, size=count)) +
  geom_text_wordcloud(area_corr = TRUE) +
  scale_size_area(max_size = 20)+
  theme_minimal()
gg2 <-gg+transition_time(time_floor) +
  labs(title = "@DukeU's most tweeted words: {frame_time}")

animate(gg2, fps=1, end_pause = 30)

anim_save("gg_Duke.gif")

## UNC time

UNCtime <-words_by_time%>%
  filter(screen_name=="UNC")

ggunc <- UNCtime %>%
  ggplot(aes(label = word, size=count)) +
  geom_text_wordcloud(area_corr = TRUE) +
  scale_size_area(max_size = 24)+
  theme_minimal()
ggunc2 <-ggunc+transition_time(time_floor) +
  labs(title = "@UNC's most tweeted words: {frame_time}")

animate(ggunc2, fps=1, end_pause = 30)

anim_save("gg_UNC.gif")

## NC State Time

NCStatetime <-words_by_time%>%
  filter(screen_name=="NCState")

ggState <- NCStatetime %>%
  ggplot(aes(label = word, size=count)) +
  geom_text_wordcloud_area(area_corr_power = 1) +
  scale_size_area(max_size = 24)+
  theme_minimal()
ggState2 <-ggState+transition_time(time_floor) +
  labs(title = "@NCState's most tweeted words: {frame_time}")

animate(ggState2, fps=1, end_pause = 30)

anim_save("gg_NCState.gif")


## try facet wrap - no bueno, no se ve bien y muy apretadito 

ggall <- words_by_time %>%
  ggplot(aes(label = word, size=count, color=count)) +
  geom_text_wordcloud(area_corr = TRUE) +
  scale_size_area(max_size = 20)+
  theme_minimal()+
  facet_wrap(~screen_name)

gg2all <-ggall+transition_time(time_floor) +
  labs(title = 'Most tweeted words Month&Year: {frame_time}')

animate(gg2all, fps=1, end_pause = 30)

## line graph of words by time per
nested_data <- words_by_time %>%
  nest(-word, -screen_name) 

nested_models <- nested_data %>%
  mutate(models = map(data, ~ glm(cbind(count, time_total) ~ time_floor, ., 
                                  family = "binomial")))

slopes <- nested_models %>%
  mutate(models = map(models, tidy)) %>%
  unnest(cols = c(models)) %>%
  filter(term == "time_floor") %>%
  mutate(adjusted.p.value = p.adjust(p.value))

top_slopes <- slopes %>% 
  filter(adjusted.p.value < 0.05)

##Duke slope

Dukeslope <-words_by_time %>%
  inner_join(top_slopes, by = c("word", "screen_name")) %>%
  filter(screen_name == "DukeU") %>%
  filter(word != "duke") %>%
  ggplot(aes(time_floor, count/time_total, color = word)) +
  geom_line(size = 1.3) +
  labs(x = NULL, y = "Word frequency")

## NCState slope

NCStateslope<-words_by_time %>%
  inner_join(top_slopes, by = c("word", "screen_name")) %>%
  filter(screen_name == "NCState") %>%
  ggplot(aes(time_floor, count/time_total, color = word)) +
  geom_line(size = 1.3) +
  labs(x = NULL, y = "Word frequency")

##unc slope
UNCslope<-words_by_time %>%
  inner_join(top_slopes, by = c("word", "screen_name")) %>%
  filter(screen_name == "UNC") %>%
  ggplot(aes(time_floor, count/time_total, color = word)) +
  geom_line(size = 1.3) +
  labs(x = NULL, y = "Word frequency")
UNCslope

## retweets and favorites 
tidy_tweetsrefav <- tmlsyr %>% 
  filter(!str_detect(text, "^(RT|@)")) %>%
  mutate(text = str_remove_all(text, remove_reg)) %>%
  unnest_tokens(word, text, token = "tweets", strip_url = TRUE) %>%
  filter(!word %in% stop_words$word,
         !word %in% str_remove_all(stop_words$word, "'"))

totalsrefav <- tidy_tweetsrefav %>% 
  group_by(screen_name, status_id) %>% 
  summarise(rts = first(retweet_count)) %>% 
  group_by(screen_name) %>% 
  summarise(total_rts = sum(rts))

totalsrefav

word_by_rts <- tidy_tweetsrefav %>% 
  group_by(status_id, word, screen_name) %>% 
  summarise(rts = first(retweet_count)) %>% 
  group_by(screen_name, word) %>% 
  summarise(retweet_count = median(rts), uses = n()) %>%
  left_join(totalsrefav) %>%
  filter(retweet_count != 0) %>%
  ungroup()

word_by_rts%>%
  filter(uses >= 5) %>%
  arrange(desc(retweet_count))

## highest median retweet
rts_plot<-word_by_rts %>%
  filter(uses >= 5) %>%
  group_by(screen_name) %>%
  slice_max(retweet_count, n = 10) %>% 
  arrange(retweet_count) %>%
  ungroup() %>%
  mutate(word = factor(word, unique(word))) %>%
  ungroup() %>%
  ggplot(aes(word, retweet_count, fill = screen_name)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ screen_name, scales = "free", ncol = 3) +
  coord_flip() +
  labs(x = NULL, 
       y = "Median # of retweets for tweets containing each word")

## favorites
totalsrefav2 <- tidy_tweets %>% 
  group_by(screen_name, status_id) %>% 
  summarise(favs = first(favorite_count)) %>% 
  group_by(screen_name) %>% 
  summarise(total_favs = sum(favs))

word_by_favs <- tidy_tweets %>% 
  group_by(status_id, word, screen_name) %>% 
  summarise(favs = first(favorite_count)) %>% 
  group_by(screen_name, word) %>% 
  summarise(favorite_count = median(favs), uses = n()) %>%
  left_join(totalsrefav2) %>%
  filter(favorite_count != 0) %>%
  ungroup()

favplot <-word_by_favs %>%
  filter(uses >= 5) %>%
  group_by(screen_name) %>%
  slice_max(favorite_count, n = 10) %>% 
  arrange(favorite_count) %>%
  ungroup() %>%
  mutate(word = factor(word, unique(word))) %>%
  ungroup() %>%
  ggplot(aes(word, favorite_count, fill = screen_name)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ screen_name, scales = "free", ncol = 3) +
  coord_flip() +
  labs(x = NULL, 
       y = "Median # of favorites for tweets containing each word")

##remove stop words from totals

tmlsyr_nostop<-tmlsyr_totals%>%
  anti_join(stop_words)%>%
  filter(word !="t.co")%>%
  filter(word != "https")

##bar chart of frequent words

tmlsyr_nostop %>%
  filter(n > 100) %>%
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(n, word)) + 
  geom_col() 

tmlsyr_nostop
## top mentions
top_mentions<-tmlsyr %>% 
  group_by(screen_name)%>%
  unnest_tokens(mentions, text, "tweets", to_lower = FALSE) %>%
  filter(str_detect(mentions, "^@")) %>%  
  count(mentions, sort = TRUE)

Duke_topmentions<-top_mentions%>%
  filter(screen_name=="DukeU")%>%
  top_n(20)

Duke_topmentions

UNC_topmentions<-top_mentions%>%
  filter(screen_name=="UNC")%>%
  top_n(20)

UNC_topmentions


NCState_topmentions<-top_mentions%>%
  filter(screen_name=="NCState")%>%
  top_n(20)


DukeNCStatemention<-top_mentions%>% #4
  filter(screen_name=="DukeU" & mentions=="@NCState")

NCStateDukemention <-top_mentions%>% # 1 time
  filter(screen_name=="NCState" & mentions=="@DukeU")

DukeUNCmention <-top_mentions%>% #9 times 
  filter(screen_name=="DukeU" & mentions=="@UNC")

UNCDukemention <- top_mentions%>% # 5 times 
  filter(screen_name=="UNC" & mentions=="@DukeU")

NCStateuncmention <- top_mentions%>% # 3 time
  filter(screen_name=="NCState" & mentions=="@UNC")

UNCNCState <-top_mentions%>% # 2 time
  filter(screen_name=="UNC" & mentions=="@NCState")

Allmentions<-rbind.data.frame(DukeNCStatemention, NCStateDukemention, DukeUNCmention, UNCDukemention, 
                       NCStateuncmention, UNCNCState)

Allmentions%>%
  arrange(desc(n))

## Bigrams tokenlist

bigramtmlsyr <-tmlsyr %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

bigrams_separated <- bigramtmlsyr %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

bigram_counts

bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

bigram_tf_idf <- bigrams_united %>%
  count(screen_name, bigram) %>%
  bind_tf_idf(screen_name, book, n) %>%
  arrange(desc(tf_idf))

## most retweeted tweet - DukeU Blacklives matter! 

Mostretweetedtwt <-tmlsyr %>% 
  arrange(-retweet_count) %>%
  slice(1) %>% 
  select(created_at, screen_name, text, retweet_count, status_id)

tweet_screenshot(tweet_url ("DukeU", "1276633436384419"))  

#log ratios

word_ratiosDukeNCState <- tidy_tweets %>%
  filter(!str_detect(word, "^@")) %>%
  count(word, screen_name) %>%
  group_by(word) %>%
  filter(sum(n) >= 10) %>%
  ungroup() %>%
  pivot_wider(names_from = screen_name, values_from = n, values_fill = 0) %>%
  mutate_if(is.numeric, list(~(. + 1) / (sum(.) + 1))) %>%
  mutate(logratio = log(DukeU / NCState)) %>%
  arrange(desc(logratio))

tableDukeNCState_ratio<-word_ratiosDukeNCState %>% 
  select(word, DukeU, NCState, logratio) %>% 
  arrange(abs(logratio))

##vader sentiment analysis
vader_tmlsyr <-vader_df(tmlsyr$text)


joinvader_tmlsyr <-tmlsyr %>%
  inner_join(vader_tmlsyr, by="text")

vader_results<-joinvader_tmlsyr%>%
  select(screen_name, created_at, text, compound, pos, neu, neg, but_count)%>%
  mutate(sentiment = ifelse(compound > 0, "positive",
                            ifelse(compound <0, "negative", "neutral")))

vader_results
##covid sentiment
vader_covid <- vader_results %>%
  filter(grepl("(?i)COVID|corona|virus|pandemic", text))

#lubridate
vader_date <-vader_covid%>%
  mutate(year_month = floor_date(created_at, "months") %>% ymd())

vader_date_summarise<-vader_date%>%
  group_by(screen_name, year_month)%>%
  summarise(mean= mean(compound))

##covid count
covid_plot_count<-vader_date%>%
  group_by(screen_name, year_month)%>%
  count(screen_name)
#plot
covid_plot_count_plot<-ggplot(covid_plot_count, aes(x=year_month, y=n,))+
  geom_line(aes(color=screen_name))+
  labs(x=NULL, title="Tweets related to Covid-19 and the 
       Pandemic: Count of area universities activity by month")

##research sentiment 
vader_research <-vader_results%>%
  filter(grepl("(?i)research", text))

#lubridate research
vader_date_research <-vader_research%>%
  mutate(year_month = floor_date(created_at, "months") %>% ymd())

vader_research_summarise<-vader_date_research%>%
  group_by(screen_name, year_month)%>%
  summarise(mean= mean(compound))

vader_research_summarise<-vader_date_research%>%
  group_by(screen_name, year_month)%>%
  count(screen_name)

research_count_plot <-vader_date_research%>%
  group_by(screen_name, year_month)%>%
  count(screen_name) 

plot_research_count <-ggplot(research_count_plot, aes(x=year_month, y=n,))+
  geom_line(aes(color=screen_name))



##plot for covid sentiment 
sentiment_plot_covid <-vader_date_summarise %>%
ggplot(aes(x=year_month, y=mean)) +
  geom_point(aes(color=screen_name))+
  geom_smooth(method="auto")+
  labs(x=NULL, y="Mean Compound Score", title="Tweets related to Covid-19 and the 
       Pandemic: Vader Sentiment Score over time for Duke, UNC, and NC State", caption="Compound Score:
       -1 (most extreme negative) and +1 (most extreme positive)")

##plot for research sentiment
sentiment_plot_research<-vader_research_summarise%>%
  ggplot(aes(x=year_month, y=mean)) +
  geom_point(aes(color=screen_name))+
  geom_smooth(method="auto")+
  labs(x=NULL, y="Mean Compound Score", title="Tweets related to research:
  Vader Sentiment Score over time for Duke, UNC, and NC State", caption="Compound Score:
       -1 (most extreme negative) and +1 (most extreme positive)")

## text net
#df for textnets - keeps crashing. won't do
textnetsdf <-tmlsyr%>%
  select(screen_name, text)
  

tmlsyr_net <-PrepText(tmlsyr, textvar = "text", groupvar = "screen_name", node_type = "groups", 
                      remove_stop_words = TRUE, tokenizer = "tweets", pos = "nouns")

tmlsyr_net2 <-PrepText(tmlsyr, textvar = "text", groupvar = "screen_name", node_type = "words", 
                      remove_stop_words = TRUE, tokenizer = "tweets", pos = "nouns")

tmlsyr_net3 <-PrepText(textnetsdf, textvar = "text", groupvar = "screen_name", node_type = "words",
                       remove_stop_words = TRUE, tokenizer = "tweets", pos = "nouns")

  tmlsyrnetwork <-CreateTextnet(tmlsyr_net)
  tmlsyrnetwork2 <-CreateTextnet(tmlsyr_net2)
  
  tmlsyrnetwork3 <-CreateTextnet(tmlsyr_net3)
  
  VisTextNetD3(tmlsyrnetwork)  
  VisTextNetD3(tmlsyrnetwork2)
  VisTextNetD3(tmlsyrnetwork3)
  
  memory.limit(size = 24000)
  
  memory.limit()
  