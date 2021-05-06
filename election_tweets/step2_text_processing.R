# Step 2: Text Processing----
# The goal of this step is to analyze and compare the sentiment of tweets that mentioned Trump and those mentioned Biden.
# To replicate output files, please first run the .Rporj file, and execute R scripts in the project environment.
library(tidyverse)
library(data.table)
library(tidytext)
library(SnowballC)
library(here)
# Relative paths
figs <- here("output", "figures")
biden <- here("data", "built", "hashtag_biden_contents.txt")
trump <- here("data", "built", "hashtag_trump_contents.txt")
stemmed_tweets_path <- here("data", "built", "stemmed_tweets.csv")
# Read texts
hashtag_biden_contents <- read_file(biden)
hashtag_trump_contents <- read_file(trump)

# Parameters----
x_title <- "Sentiments"
y_title <- "Percent (%)"
title <- "Sentiment Analysis of US 2020 Election Tweets"
bing_caption <- "Sentiment source: BING \n Data source: US Election 2020 Tweets \n Data courtesy of Kaggle user Manch Hui"
nrc_caption <- "Sentiment source: NRC \n Data source: US Election 2020 Tweets \n Data courtesy of Kaggle user Manch Hui"
fill <- "Candidate"

# Tokenize----
contents_list <- list(hashtag_biden_contents = hashtag_biden_contents, hashtag_trump_contents = hashtag_trump_contents)
tokenized_tweets_list <- mapply(function(char) {
  tweets_tibble <- tibble(tweets = char)
  tweets_tokenized <- unnest_tokens(tweets_tibble, word_tokens, tweets, token = "words")
},
contents_list,
SIMPLIFY = FALSE
)

# Remove stop words and extract stems of each word token----
stemmed_tweets_list <- mapply(function(df, name) {
  df %>%
    mutate(hashtag = if_else(name == "hashtag_trump_contents", "Trump", "Biden")) %>%
    anti_join(stop_words, by = c("word_tokens" = "word")) %>%
    mutate(word_stem = wordStem(word_tokens, language = "porter")) %>%
    filter(!word_stem %in% c("donaldtrump", "joebiden", "DJT", "Donald", "Trump", "donald", "trump", "Joe", "Biden", "joe", "biden")) %>%
    select(!word_tokens)
},
tokenized_tweets_list, names(tokenized_tweets_list),
SIMPLIFY = FALSE
)
stemmed_tweets <- rbindlist(stemmed_tweets_list)

# Join in sentiments----
# positive or negative sentiment
bing_sentiment <- as.data.table(get_sentiments("bing"))
# more complex sentiments
nrc_sentiment <- as.data.table(get_sentiments("nrc"))
join_sentiment <- function(df, sentiment) {
  df %>%
    left_join(sentiment, by = c("word_stem" = "word")) %>%
    mutate(sentiment = str_to_title(sentiment)) %>%
    # rename in function from https://stackoverflow.com/a/58404128/14216571
    filter(!is.na(sentiment))
}
tweets_bing_sentiment <- join_sentiment(stemmed_tweets, bing_sentiment)
tweets_nrc_sentiment <- join_sentiment(stemmed_tweets, nrc_sentiment)

# Plot----
# Prepare datasets for plotting bar graphs
tweets_bing_sentiment %>%
  count(hashtag, bing) %>%
  group_by(hashtag) %>%
  mutate(pct = n / sum(n) * 100)

calculate_group_pct <- function(df) {

}
tweets_sentiment_list <- list(tweets_bing_sentiment = tweets_bing_sentiment, tweets_nrc_sentiment = tweets_nrc_sentiment)
tweets_sentiment_pct_list <- mapply(function(df) {
  df %>%
    count(hashtag, sentiment) %>%
    group_by(hashtag) %>%
    mutate(pct = n / sum(n) * 100)
},
tweets_sentiment_list,
SIMPLIFY = FALSE
)

# generalize plot function
plot <- function(df) {
  df %>%
    ggplot() +
    geom_col(aes(x = sentiment, y = pct, fill = hashtag), position = "dodge") +
    theme(
      panel.background = element_rect(fill = "white"),
      axis.text = element_text(size = 12),
      axis.text.x = element_text(angle = 45),
      axis.title = element_text(size = 14),
      axis.ticks = element_blank(),
    ) +
    scale_fill_manual(values = artyfarty::pal("five38"))
}

# plot positive/negative sentiments
plot(tweets_sentiment_pct_list$tweets_bing_sentiment) +
  labs(
    x = x_title,
    y = y_title,
    caption = bing_caption,
    fill = fill
  )
ggsave(here(figs, "plot_bing.png"))

# plot nrc sentiments
plot(tweets_sentiment_pct_list$tweets_nrc_sentiment) +
  labs(
    x = x_title,
    y = y_title,
    caption = nrc_caption,
    fill = fill
  )
ggsave(here(figs, "nrc.png"))
