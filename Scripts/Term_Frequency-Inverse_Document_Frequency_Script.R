# Which words are the most important and unique to each book?

# We can use the Term Frequency-Inverse Document Frequency

# bind_tf_idf() works out the important words for each book by adding
# weighting to each word - decreasing the weight for commonly used words and
# in increasning the weight for words not used much in the overall corpus

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

# Good at extracting the core words that define a text

# We can also unnest by n-grams to capture sequences of words. in this example,
# let's look at tokenizing by bigram

library(igraph)
library(ggraph)

wotw_bigrams <- books %>% 
  filter(title == "The War of the Worlds") %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  separate(col = bigram, into = c("word1", "word2", sep = " ")) %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  count(word1, word2, sort = TRUE)

bigram_graph <- wotw_bigrams %>%
  filter(n > 5) %>%
  graph_from_data_frame()

set.seed(1234)
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(alpha = .25) +
  geom_node_point(alpha = .25) +
  geom_node_text(aes(label = name), vjust = -.1, hjust = 1.25, size = 3) +
  guides(size = FALSE) +
  xlim(10, 22) +
  theme_void() 
