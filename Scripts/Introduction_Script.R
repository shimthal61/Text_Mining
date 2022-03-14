library(tidyverse)
library(tidytext) #Working with text in a tidy format
library(gutenbergr) #Free books from the Gutenberg library

titles <- c("The War of the Worlds",
            "The Time Machine",
            "Twenty Thousand Leagues under the Sea",
            "The Invisible Man: A Grotesque Romance")

# We've downloaded our texts such that each row corresponds to a line in the book
books <- gutenberg_works(title %in% titles) %>% 
  gutenberg_download(meta_fields = "title")

str(books)

head(books, n = 8)

books %>% 
  distinct(title)

# We can examine subsets of the tibble

books$text[31:40]

# The text is all in one column - not in tidy format.
# We can do this in a process called unnesting.
# We're also going to remove 'stop' words. These are common words which (e.g. 'the', 'of') that have little meaning

all_text <- books %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words)

all_text

all_text %>% 
  filter(title == "The Time Machine") %>% 
  count(word, sort = TRUE) %>% 
  top_n(10)

all_text %>% 
  filter(title == "The War of the Worlds") %>% 
  count(word, sort = TRUE) %>% 
  top_n(10)

all_text %>%
  filter(title == "The Time Machine") %>%
  count(word, sort = TRUE) %>%
  top_n(10) %>%
  ggplot(aes(x = reorder(word, n), y = n, fill = word)) +
  geom_col() +
  coord_flip() +
  guides(fill = FALSE) +
  labs(x = "Word", 
       y = "Count", 
       title = "Top 10 most commonly occurring words in The Time Machine") +
  theme_minimal()
