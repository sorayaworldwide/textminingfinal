library(rtweet)
library(writexl)
library(readxl)
library(tidyverse)
library(tidytext)
library(gganimate)
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
##ts_plot by screen name
  ts_plot(group_by(tmlsyr, screen_name), "months")
##save tweets
  write_xlsx(tmlsyr, "UniversityTweets.xlsx")
  write_xlsx(tmls, "UniversityTweetsorg.xlsx")

##histogram plot of time facet   
  histogramplot <-ggplot(tmlsyr, aes(x = created_at, fill = screen_name)) +
    geom_histogram(position = "identity", bins = 20, show.legend = FALSE) +
    facet_wrap(~screen_name, ncol = 1)  
  
##tokenize list
  ##top terms by instit
    