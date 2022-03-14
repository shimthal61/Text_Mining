# THe 'bing' database has sentiment ratings (pos vs. neg) for about 7000 words

get_sentiments("bing")

# We can join our all_text data to our "bing" data

all_text_sentiments <- all_text %>% 
  inner_join(get_sentiments("bing"))

head(all_text_sentiments)

all_text_sentiments %>% 
  filter(title == "The War of the Worlds") %>% 
  count(word, sentiment, sort = TRUE) %>% 
  top_n(25) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  #mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = reorder(word, n), y = n, fill = sentiment)) +
  geom_col() +
  coord_flip() +
  labs(title = "Sentiment Analysis of Top 25 Words in The War of the Worlds", 
       x = "Word",
       y = "Count") 

# Problem with binary - just pos vs neg. Also looks at words out of contexts

# We can look at the proportion of each words in a book

book_words <- all_text %>% 
  group_by(title) %>% 
  count(title, word, sort = TRUE)

total_words <- book_words %>% 
  group_by(title) %>% 
  summarise(total = sum(n))

book_words <- left_join(book_words, total_words)

book_words %>%
  mutate(proportion = n/total) %>%
  group_by(title) %>%
  arrange(desc(title, proportion)) %>%
  top_n(3) %>%
  select(-n, -total)

book_words %>%
  ggplot(aes(x = n/total, fill = title)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~title, ncol = 2, scales = "free")

#Zipf's Law - the most common word in a language tends to be 2x as common 
#as the 2nd most common and 3x as common as the 3rd common word

