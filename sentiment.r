##############################
##### SENTIMENT ##############
##############################

library(tidyverse)
library(tidytext)
word_250 <- df_250m_ex %>% unnest_tokens(word, description) %>%
  anti_join(stop_words)

word_250 %>% count(word, sort = T) %>% head()

word_popu <- df_popu_ex %>% unnest_tokens(word, description) %>%
  anti_join(stop_words)

word_popu %>% count(word, sort = T) %>% head()


# frequency

library(tidyr)

freq <- bind_rows(mutate(word_250, movie = "top 250"),
                  mutate(word_popu, movie = "popular")) %>%
  mutate(word = str_extract(word, "[a-z]+")) %>% # exclude words in abnormal format, e.g. any'day
  count(movie, word) %>%
  group_by(movie) %>% 
  mutate(proportion = n / sum(n)) %>% # compute frequency after grouping
  select(-n) # no need to look at n now

# create seperate columns for comparison
frequency <- freq %>% pivot_wider(names_from = "movie", values_from = "proportion")
frequency

# Plot proportions of words used in two movie lists:
ggplot(frequency, aes(x = `top 250`, y = `popular`)) +
  geom_abline(color = "red", lty = 2, lwd = 2) +
  geom_point(color = "grey") + 
  geom_text(aes(label = word), check_overlap = T) +
  scale_x_log10() + scale_y_log10()

frequency %>% filter(!(`top 250` == "NA" | `popular` == "NA")) %>% select(,2:3) %>% cor()
# barely no common features

# ten most common bigrams in two lists
bigram_250 <- df_250m_ex %>% unnest_tokens(bigram, description, token = "ngrams", n = 2) %>%
  filter(bigram != "NA")
# Split out the words in a pair and delete the pairs with a stop word in them
bigram_250 %>% separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%    # Delete pairs with at least one stop word
  filter(!word2 %in% stop_words$word) %>%
  unite(bigram, word1, word2, sep = " ") %>%
  count(bigram, sort = T)

bigram_popu <- df_popu_ex %>% unnest_tokens(bigram, description, token = "ngrams", n = 2) %>%
  filter(bigram != "NA")
# Split out the words in a pair and delete the pairs with a stop word in them
bigram_popu %>% separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%    # Delete pairs with at least one stop word
  filter(!word2 %in% stop_words$word) %>%
  unite(bigram, word1, word2, sep = " ") %>%
  count(bigram, sort = T)



################################################
###   Draw plot for sentiment analysis       ###
################################################
tidy_movie <- bind_rows(mutate(word_250, movie = "top 250"),
                       mutate(word_popu, movie = "popular")) %>% 
  mutate(word = str_extract(word, "[a-z]+")) %>%   
  group_by(movie) %>%
  select(word, movie, title, imDbRating) 
tidy_movie

movie_sentiment <- tidy_movie %>%
  inner_join(get_sentiments("bing")) %>%
  count(movie, index = imDbRating, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

ggplot(movie_sentiment, aes(index, sentiment, fill = movie)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~movie, ncol = 2, scales = "free_x")
## Back to each list and look at word
word_250 %>% count(word, sort = T) %>% head(n=10)

word_popu %>% count(word, sort = T) %>% head(n=10)

############ FAILED ##################
######### Topic model ################
######################################


## topic models 
library(topicmodels)
senti_250 <- word_250 %>% select(word, title, imDbRating) %>%
  inner_join(get_sentiments("bing"), by = c(word = "word"))
   
# Fit topic model
try <- word_250 %>% drop_na() %>% select(word) %>%
  count(word, sort = T)
m250_lda <- LDA(try,k=3,method = "Gibbs", control = list(seed = 2021))








