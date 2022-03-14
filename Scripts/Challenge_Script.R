titles <- c("Dracula",
            "Pride and Prejudice",
            "A Tale of Two Cities",
            "Alice's Adventures in Wonderland")

books <- gutenberg_works(title %in% titles) %>% 
  gutenberg_download(meta_fields = "title")

books %>% 
  distinct(title)

all_text <- books %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words)

all_text

# Top 10 most common words
all_text %>% 
  filter(title == "Dracula") %>% 
  count(word, sort = TRUE) %>% 
  top_n(10)

all_text %>% 
  filter(title == "Pride and Prejudice") %>% 
  count(word, sort = TRUE) %>% 
  top_n(10)

all_text %>%
  filter(title == "Alice's Adventures in Wonderland") %>%
  count(word, sort = TRUE) %>%
  top_n(10) %>%
  ggplot(aes(x = reorder(word, n), y = n, fill = word)) +
  geom_col() +
  coord_flip() +
  guides(fill = FALSE) +
  labs(x = "Word", 
       y = "Count", 
       title = "Top 10 most commonly occurring words in\nAlice's Adventures in Wonderland") +
  theme_minimal()

all_text_sentiments <- all_text %>% 
  inner_join(get_sentiments("bing"))

all_text_sentiments %>% 
  filter(title == "A Tale of Two Cities") %>% 
  count(word, sentiment, sort = TRUE) %>% 
  top_n(25) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = reorder(word, n), y = n, fill = sentiment)) +
  geom_col() +
  coord_flip() +
  labs(title = "Sentiment Analysis of Top 25 Words in A Tale of Two Cities", 
       x = "Word",
       y = "Count") 

# The proportion of words in the books
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


# TFIDF
book_words_tf_idf <- book_words %>% 
  bind_tf_idf(word, title, n)

book_words_tf_idf %>%
  top_n(15, tf_idf) %>%
  ggplot(aes(x = reorder(word, tf_idf), y = tf_idf, fill = title)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "Term Frequency-Inverse Document Frequency") +
  coord_flip() +
  facet_wrap(~ title, ncol = 2, scales = "free") +
  theme(text = element_text(size = 8))

Dracula_bigrams <- books %>% 
  filter(title == "Dracula") %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 3) %>%
  separate(col = bigram, into = c("word1", "word2", "word3", sep = " ")) %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  count(word1, word2, word3, sort = TRUE)

bigram_graph <- Dracula_bigrams %>%
  filter(n > 5) %>%
  graph_from_data_frame()

set.seed(1234)
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(alpha = .25) +
  geom_node_point(alpha = .25) +
  geom_node_text(aes(label = name), vjust = -.1, hjust = 1.25, size = 3) +
  guides(colour = 'none') +
  xlim(10, 22) +
  theme_void() 
