# Step 1: Data Wrangling----
# To replicate output files, please first run the .Rporj file, and execute R scripts in the project environment.
library(tidyverse)
library(data.table)
library(here)
# Relative paths----
tweets_shiny_path <- here("step_4_interactive_plot", "tweets_shiny.csv")
hashtag_trump_path <- here("data/raw", "tweets/hashtag_donaldtrump.csv")
hashtag_biden_path <- here("data/raw", "tweets/hashtag_joebiden.csv")
clean_hashtag_biden_trump_path <- here("data/built", "clean_hashtag_biden_trump.csv")
campaign_events_path <- here("data/raw", "2020 Presidential Candidate General Election Events Tracker (maintained by FairVote, Nov Version).xlsx")
campaign_events_clean_path <- here("data/built", "campaign_events_2020.csv")

# Read datasets----
# Read tweets
# hashtag_trump is not pushed to the github repo due to the size limit. The data can be downloaded from
# https://www.kaggle.com/manchunhui/us-election-2020-tweets?select=hashtag_donaldtrump.csv
hashtag_trump <- read_csv(hashtag_trump_path)
# hashtag_biden is not pushed to the github repo due to the size limit. The data can be downloaded from 
# https://www.kaggle.com/manchunhui/us-election-2020-tweets?select=hashtag_joebiden.csv
hashtag_biden <- read_csv(hashtag_biden_path)
# Read 2020 presidential campaign visit history
campaign_events <- readxl::read_xlsx(campaign_events_path, sheet = "2020 Candidate Events", skip = 4)

# Parameters----
# In the hashtag datasets, drop tweets from users joining later than 2020-01-01 and the users with no more than 5 followers.
# Missing join dates are coded as "1970-01-01"
# Also drop tweets created before "2020-01-01"
election_year_start <- as.Date("2020-01-01", "%Y-%m-%d")
missing_date <- as.Date("1970-01-01", "%Y-%m-%d")
# US geography bounding box found here https://boundingbox.klokantech.com/
westlimit <- -127.8
southlimit <- 25.5
eastlimit <- -63.5
northlimit <- 49.4

# Clean tweets data ----
hashtags <- list(hashtag_biden = hashtag_biden, hashtag_trump = hashtag_trump)
clean_hashtags <- mapply(function(df, name) {
  df %>%
    mutate(hashtag = if_else(name == "hashtag_trump", "Trump", "Biden")) %>%
    mutate(hashtag = if_else(str_detect(tweet, c("JoeBiden", "DonaldTrump")), "Both", hashtag)) %>%
    # remove special characters in tweets by swapping out non-alphanumeric characters
    # from https://stackoverflow.com/a/10294818/14216571
    mutate(tweet = str_replace_all(tweet, "[^[:alnum:]]", " ")) %>%
    # Change user follower count from chr to int
    mutate(
      user_followers_count = as.integer(user_followers_count),
      created_at = as.Date(created_at, "%Y-%m-%d")
    ) %>%
    # keep needed variables; `use_location` seems to be self-reported and inaccurate
    select(-source, -collected_at, -user_location, -user_description) %>%
    # Remove potential "bot" accounts
    filter(user_followers_count >= 5 & user_join_date <= election_year_start) %>%
    # Remove tweets missing user join date or tweet created date
    filter(created_at > missing_date & user_join_date > missing_date) %>%
    # Keep use tweets only
    filter(lat >= southlimit & lat <= northlimit & long >= westlimit & long <= eastlimit)
},
hashtags, names(hashtags),
SIMPLIFY = FALSE
)

## Append "hashtag_trump" and "hashtag_biden" together into one tidy dataset for event study----
clean_hashtag_biden_trump <- rbindlist(clean_hashtags)
# Remove duplicate tweets
clean_hashtag_biden_trump <- clean_hashtag_biden_trump %>%
  distinct(tweet_id, .keep_all = TRUE)
# Save the cleaned tweet dataframe into built
# The cleaned tweet data is not pushed to the github repo due to the size limit.
# The data can be downloaded from 
# https://drive.google.com/file/d/1wTgWslUAMXzndUDwuFDwPnIK2jadFjRh/view?usp=sharing
write_csv(clean_hashtag_biden_trump, clean_hashtag_biden_trump_path)

## Extract tweets into txt files for sentiment analysis----
# To simplify sentiment analysis, drop the tweets that mention both candidates.
put_to_txt <- function(df, name) {
  df <- df %>%
    filter(hashtag == {{ name }})
  as.vector(df$tweet)
}
hashtag_biden_contents <- put_to_txt(clean_hashtag_biden_trump, "Biden")
hashtag_trump_contents <- put_to_txt(clean_hashtag_biden_trump, "Trump")
# Write txt file found here https://stackoverflow.com/a/2470277/14216571
for (filename in c("hashtag_trump_contents", "hashtag_biden_contents")) {
  file.create(paste0(here("data", "built"), filename, ".txt"))
}
bidenConn <- file(here("data/built", "hashtag_biden_contents.txt"))
trumpConn <- file(here("data/built", "hashtag_trump_contents.txt"))
writeLines(hashtag_biden_contents, bidenConn)
writeLines(hashtag_trump_contents, trumpConn)

## Prepare tweets for shiny----
tweets_shiny <- clean_hashtag_biden_trump %>%
  rename(
    latitude = lat,
    longitude = long
  ) %>%
  select(created_at, likes, retweet_count, user_followers_count, latitude, longitude, tweet)
write_csv(tweets_shiny, tweets_shiny_path)

# Clean campaign events----
dem_events <- campaign_events[1:7]
# Remove NA rows
dem_events <- dem_events %>%
  filter(!is.na(Date...1))
gop_events <- campaign_events[9:15]
# clean variable names
event_list <- list(dem_events = dem_events, gop_events = gop_events)
clean_event_list <- lapply(event_list, function(df) {
  df %>%
    rename_all(~ sub("\\..*", "", names(df))) %>%
    janitor::clean_names() %>%
    mutate(date = lubridate::date(date))
})
# append dem and gop events into one tidy data
campaign_events_clean <- do.call(rbind, clean_event_list)
# store the clean data
write_csv(campaign_events_clean, campaign_events_clean_path)
