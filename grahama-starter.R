install.packages("rtweet")

library(tidyverse)
library(rtweet)
library(ggthemes)

theme_reach <- function() {
  theme_fivethirtyeight() +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 22, hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(size = 18, hjust = 0.5),
      axis.title.x = element_text(size=18),
      axis.title.y = element_text(size=18),
      axis.text = element_text(size = 14),
      strip.text = element_text(size = 16, face = "bold"),
      legend.text = element_text(size = 14)
    )
}

follower_ids <- rtweet::get_followers("GrahamInstitute")

ids <- follower_ids$user_id

follower_stats <- rtweet::lookup_users(ids)

follower_stats %>% 
  ggplot(aes(x = followers_count)) +
  geom_histogram(fill = "darkblue", alpha = 0.9) +
  theme_reach() +
  labs(x = "Follower Count (Log Scale)",
       title = "Each of Graham's Followers Twitter Follower Count",
       y = "") +
  scale_x_log10() 

follower_stats %>%
  group_by(source) %>%
  summarize(count = n()) %>%
  filter(!is.na(source), count >= 50) %>%
  ggplot(aes(x = count, y = fct_reorder(source, count))) +
  geom_bar(aes(fill = count), stat = "identity") +
  scale_fill_gradient(high = "darkorange", low = "darkblue") +
  theme_reach() +
  labs(x = "Count",
       y = "What Followers Used to Follow Graham",
       title = "What Type of Twitter Followers Are Using to Follow Graham",
       subtitle = "Types with at least 50 instances included")

graham_tweets <- rtweet::search_tweets("GrahamInstitute", include_rts = FALSE, type = "recent") %>%
  select(created_at, text, is_quote, favorite_count, retweet_count, quote_count, reply_count,
         hashtags, media_type, urls_expanded_url)

graham_tweets <- graham_tweets %>%
  mutate(quote_count = ifelse(is.na(quote_count), 0, quote_count),
         reply_count = ifelse(is.na(reply_count), 0, reply_count),
         media_type = ifelse(is.na(media_type), "None", media_type),
         urls_expanded_url = ifelse(is.na(media_type), "No URL", urls_expanded_url),
         use_hashtag = ifelse(is.na(hashtags), "No", "Yes"))





