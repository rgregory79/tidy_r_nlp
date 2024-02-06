# Load libraries
library(gutenbergr)
library(tm)
library(tidytext)
library(dplyr)
library(ggplot2)

# Set up a list of book IDs by Friedrich Nietzsche
book_ids <- c(1998, 19322, 4363, 52263, 19322)

# Fetch books from Project Gutenberg
books <- lapply(book_ids, gutenberg_download)

# Convert books to data frames
book_dfs <- lapply(books, function(book) data.frame(text = book$text))

# Combine data frames into a single data frame
all_books <- bind_rows(book_dfs, .id = "book_id")

# Perform text cleaning
clean_text <- all_books$text %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords, stopwords("en")) %>%
  tm_map(stripWhitespace)

# Create a document-term matrix
dtm <- DocumentTermMatrix(Corpus(VectorSource(clean_text)))

# Convert the document-term matrix to a tidy data frame
tidy_dtm <- tidy(dtm)

# Find correlations between terms
correlations <- tidy_dtm %>%
  inner_join(tidy_dtm, by = "document") %>%
  filter(term.x != term.y) %>%
  group_by(term.x, term.y) %>%
  summarize(correlation = cor(count.x, count.y))

# Plot correlations
ggplot(correlations, aes(term.x, term.y, fill = correlation)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1), space = "Lab") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
