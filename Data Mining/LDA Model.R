library(tidytext)
library(readr)
library(dplyr)
library(tidyr)
library(topicmodels)
library(forcats)
library(ggplot2)
library(stringr)

#code to assign directory containing the book files to data_path
data_path <- "C:/Users/Ashish Khatavkar/Downloads/books"

#list of book filenames
book_filenames <- c("A_Tale_of_Two_Cities.txt",
                    "Adventures_of_Huckleberry_Finn.txt",
                    "Alices_Adventures_in_Wonderland.txt",
                    "Great_Expectations.txt",
                    "The_Adventures_of_Tom_Sawyer.txt",
                    "Through_the_Looking-Glass.txt")

#code to get full path of the book files
full_book_paths <- paste(data_path, book_filenames, sep="/")

#code to read the contents of each book into a list
books_content <- sapply(full_book_paths, readLines, simplify = FALSE)

#function to process text and extract chapter information
process_book_contents <- function(contents, file_path) {
  chapter_regex <- "(?i)\\b(CHAPTER|SONNET)\\s+[\\dIVXL]+"  #code to define a pattern to get chapter headings
  chapter_starts <- grep(chapter_regex, contents) #to get the start lines of each chapter
  if (length(chapter_starts) == 0) {
    warning(paste("No chapters found in", basename(file_path)))
    return(data.frame(line = contents, chapter = NA, book_name = basename(file_path)))
  }
  chapter_assignment <- rep(NA, length(contents))
  chapter_limits <- c(chapter_starts, length(contents) + 1)
  for (i in seq_along(chapter_starts)) {
    chapter_assignment[seq(chapter_limits[i], chapter_limits[i + 1] - 1)] <- i
  }
  #to construct a data frame with the line, chapter number, and book name
  data.frame(line_text = contents, chapter_number = chapter_assignment, book_name = basename(file_path))
}

#code to process each book and combine results to a single data frame
all_books_data <- do.call(rbind, Map(process_book_contents, books_content, full_book_paths))

#code remove rows where chapter = NA
final_books_data <- subset(all_books_data, !is.na(chapter_number))

#final data frame with bookname chapter number and line
print(head(final_books_data))

#code to merge the lines of a chapter in a single list
books_grouped_by_chapter <- final_books_data %>%
  group_by(book_name, chapter_number) %>%
  summarise(chapter_text = paste(line_text, collapse = " ")) %>%
  ungroup() 

bigrams_by_book <- books_grouped_by_chapter %>% 
  unnest_tokens(bigram, 
                chapter_text, 
                token = "ngrams",
                n = 2) %>%
  separate(bigram, c("word_1", "word_2"), sep = " ", remove = FALSE) %>%
  filter(!word_1 %in% stop_words$word,
         !word_2 %in% stop_words$word) %>%
  count(book_name, bigram, word_1, word_2) %>%
  bind_tf_idf(bigram, book_name, n) 

#code to give top 5 bigrams for individual books
top_5_bigrams_by_book <- bigrams_by_book %>%
  group_by(book_name) %>%
  top_n(5, tf_idf)
top_5_bigrams_by_book

#code to plot the top 5 bigram for each book
top_5_bigram_by_book_plot <- ggplot(top_5_bigrams_by_book, aes(x = bigram, y = tf_idf, fill = book_name)) + 
  geom_col() + 
  facet_wrap(~book_name, scales = "free") +
  labs(title = "Top 5 Bigrams for each book",
       y = "Term Freq - Inverse Doc Freq (tf_idf)",
       x = "Bigrams") +
  coord_flip() +
  theme_light() +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

top_5_bigram_by_book_plot

#unite the document and chapter no column
books_df <- books_grouped_by_chapter %>%
  unite(document, book_name, chapter_number)

#untokenised
words_by_chapter <- books_df %>%
  unnest_tokens(word, chapter_text)

#code to counts words per chapter after removing stop words.
count_of_words <- words_by_chapter %>%
  anti_join(stop_words, by = "word") %>%
  count(document, word, sort = TRUE)

word_counts <- count_of_words %>% 
  group_by(word) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  filter(word != "chapter")

#code to cast to a Document Term Matrix (DTM)
document_term_matrix <- count_of_words %>%
  cast_dtm(document, word, n)

first_6_documents <- rownames(document_term_matrix) %>% head()
first_6_terms <- colnames(document_term_matrix) %>% head()

first_6_documents
first_6_terms

#code to get first few rows and columns of dtm
document_term_matrix[1:6, 1:6] %>% as.matrix()

#code to call the LDA function with 3 topics
lda_model <- LDA(document_term_matrix, k = 3, control = list(seed = 11111))

#code to get the beta terms with prob 1
terms_beta <- tidy (lda_model, matrix = "beta")
terms_beta

terms_beta %>%
  group_by(topic) %>%
  summarise(n = n(), sum = sum(beta))

#code to get the top five words for each topic using beta matrix
top_7_beta <- terms_beta %>%
  group_by(topic) %>%
  top_n(7, beta) %>%
  mutate(term = fct_reorder(term, beta),
         topic = as.factor(topic))

print(top_7_beta, n = 10)

#betaPlot
ggplot(top_7_beta, aes(x = term, y = beta, fill = topic)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~topic, scales = "free") +
  labs(title = "Top Terms in Three Topics",
       x = "Term",
       y = "Beta Value") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5))


#code to get the gamma matrix to check how the topics behave with the books
documents_gamma <- tidy(lda_model, matrix="gamma") %>%
  separate(document, into = c("book", "chapter"), remove = FALSE, sep = ".txt_") %>%
  mutate(topic = as.factor(topic))
documents_gamma

documents_gamma %>%
  group_by(document) %>%
  summarise(n = n(), sum = sum(gamma))


#code to plot distribution of gamma values by chapter by topic in each book
ggplot(documents_gamma, aes(y = gamma, x = topic, fill = topic)) +
  geom_boxplot() +
  facet_wrap(~book) +
  labs(title = "Gamma Plot of Topics across different books",
       x = "Term",
       y = "Gamma Value") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5))

#Distribution of gamma values for the chapters in Great_Expectations
#ggplot(documents_gamma %>%  filter(book == "Great_Expectations"), aes(x = gamma, fill = topic)) + geom_histogram() + facet_wrap(~topic) + theme_classic()
