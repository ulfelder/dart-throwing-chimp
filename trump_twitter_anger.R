library(tidyverse)
library(tidytext)
library(jsonlite)
library(lubridate)

# to get the full archive:
# 1. open http://www.trumptwitterarchive.com/archive
# 2. click on 'export' and select 'JSON'
# 3. if you like, unselect columns we won't use, like RTs, favorites, source
# 4. position the cursor in the text box, select all, and copy
# 5. open a new plain text document
# 6. paste the copied text into the document
# 7. save it to your desired directory
# 8. close the tab
#
# NOTE: I have repeatedly had problems with some ghost operation happening in R after I do this,
# and the only way I've found to fix it is to reboot the whole computer

mypath <- "~/documents/blog_posts/trump_tweets_20180804.txt"

X <- fromJSON(mypath)

# the date stamp in the json is a pain, so:
# 1) split it into its parts; and then
# 2) create a col of clean dates based on the key bits of those parts.
date_fields <- strsplit(X$created_at, " ")
X$date <- date(sapply(seq_along(date_fields), function(i) {
	
	sprintf("%s-%s-%s",
	        date_fields[[i]][6], # year
	        match(date_fields[[i]][2], month.abb), # month
	        date_fields[[i]][3]) # day
	
}))

# reduce the data to only the cols we want: tweet id, date, and content
X <- X[,c("id_str", "date", "text")]

# prep the tweets for analysis by...
X2 <- X %>%
  # removing RTs
  filter(!grepl("^RT", text)) %>%
  # removing urls and converting ampersands to ands
  mutate(text = gsub(" ?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", "", text),
         text = gsub("&amp;", "and", text)) %>%
  # creating tidy text version with one word per row (other cols preserved by default)
  unnest_tokens(word, text) %>%
  # removing stop words
  anti_join(stop_words) %>%
  # removing digits
  filter(!grepl("[0-9]{1,}", word))

# make master data frame of dates spanning full period
date_master <- data.frame(date = seq(from = min(X2$date),
                                     to = max(X2$date),
                                     by = "day"))

# daily word counts
trump_daily_word_count <- X2 %>%
  group_by(date) %>%
  summarize(total_words = n()) %>%
  left_join(date_master, .)
# replace NAs for days with no tweets with 0s
trump_daily_word_count[is.na(trump_daily_word_count)] <- 0

# daily anger word counts
trump_daily_word_count_anger <- X2 %>%
  inner_join(., get_sentiments("nrc")) %>%
  filter(sentiment == "anger") %>%
  group_by(date) %>%
  summarize(anger_words = n()) %>%
  left_join(trump_daily_word_count, .) %>%
  mutate(anger_words = ifelse(is.na(anger_words), 0, anger_words),
         anger_share = ifelse(total_words == 0, 0, anger_words/total_words))

png("~/documents/blog_posts/trump_tweets_words_raw_20180804.png",
    width = 7, height = 5, unit = "in", res = 300)
ggplot(filter(trump_daily_word_count, date >= "2012-01-01"), aes(date, total_words)) +
  geom_col() +
  geom_smooth(method = "loess", size = 1, span = 1/4, colour = "blue") +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  ggtitle("Daily counts of words tweeted by @realDonaldTrump")
dev.off()

png("~/documents/blog_posts/trump_tweets_anger_raw_20180804.png",
    width = 7, height = 5, unit = "in", res = 300)
ggplot(filter(trump_daily_word_count_anger, date >= "2012-01-01"), aes(date, anger_words)) +
  geom_col() +
  geom_smooth(method = "loess", size = 1, span = 1/4, colour = "red") +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  ggtitle("Daily count of anger words tweeted by @realDonaldTrump")
dev.off()

png("~/documents/blog_posts/trump_tweets_anger_share_20180804.png",
    width = 7, height = 5, unit = "in", res = 300)
ggplot(filter(trump_daily_word_count_anger, date >= "2012-01-01"), aes(date, anger_share)) +
  geom_col() +
  geom_smooth(method = "loess", size = 1, span = 1/3, colour = "red") +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  ggtitle("Anger words as a share of daily words tweeted by @realDonaldTrump")
dev.off()

# make a table of tallies of the anger words
trump_anger_vocabulary <- X2 %>%
  inner_join(., get_sentiments("nrc")) %>%
  filter(sentiment == "anger") %>%
  group_by(word) %>%
  tally() %>%
  arrange(desc(n))

# inspect the top 15 anger words for curiousity's sake and to make sure nothing looks wacky
trump_anger_vocabulary[1:15,]
