library(httr)
library(xml2)
library(dplyr)
library(readr)
library(rebus)
library(tidyr)
library(tidytext)
library(textdata)
library(ggplot2)
library(stringr)
#library(topicmodels)

#a <- GET("http://rss.cnn.com/rss/edition_world.rss")
a <- GET("http://rss.cnn.com/rss/cnn_latest.rss")
writeLines(content(a, as = "text"), "question1.xml")

my_doc <- read_xml("question1.xml")

xml_structure(my_doc)

all_titles <- xml_find_all(my_doc, ".//title")
all_description <- xml_find_all(my_doc, ".//description")

titles <- xml_text(all_titles)
descriptions <- xml_text(all_description)

#all_rows <- append(titles, descriptions)

all_rows <- c(titles, descriptions)

tokenised <- all_rows %>%
  tibble(text = .) %>%
  unnest_tokens(token, text)

head(tokenised)

word_counts <- tokenised %>% 
    count(token)

#introducing sentiments in tidytext package
nrc <- get_sentiments("nrc")

#nrc %>% write_csv("nrc.csv")

nrc <- read_csv("nrc.csv")

nrc_sentiment_details <- nrc %>%
  inner_join(word_counts, by = c("word" = "token")) %>% 
  group_by(sentiment)

nrc_sentiment_counts <- nrc %>%
  inner_join(word_counts, by = c("word" = "token")) %>% 
  group_by(sentiment) %>%
  summarise(n = sum(n))

nrc_graph <- nrc_sentiment_counts %>% 
  ggplot(aes(x = sentiment, y = n, fill = sentiment)) +
  geom_col(show.legend = FALSE) + 
  labs(title = "Frequency of NRC sentiments in CNN rss feed")

#ggsave("nrc_graph_0203.png", plot = nrc_graph_0203, width = 16, height = 10)

nrc_graph
nrc_graph_2502

word_counts %>%
  arrange(desc(n)) %>%
  top_n(10) %>%
  ggplot(aes(x = reorder(token, n), y = n)) +
  geom_col(fill = "blue") +
  coord_flip() +
  labs(title = "Top 10 Most Frequent Words",
       x = "Word",
       y = "Frequency")


# Create a filename with today's date and week number
file_name <- format(Sys.Date(), "%Y-Week-%V.csv")

####################################################

source("require_packages.R")
require_packages(c(
  "xml2",
  "textdata",
  "httr",
  "dplyr",
  "readr",
  "rebus",
  "tidyr",
  "tidytext",
  "stringr"
))


#HTTP GET Request
a <- GET("http://rss.cnn.com/rss/cnn_latest.rss")

html <- read_html(rawToChar(a$content))

#code to save the raw content to xml file
writeLines(content(a, as = "text"), "question1.xml")

#code to read the created xml file
my_doc <- read_xml("question1.xml")

#code to extract all the titles and description nodes from the xml document
all_titles <- xml_find_all(my_doc, ".//title")
all_description <- xml_find_all(my_doc, ".//description")

#code to just extract the text from the nodes
titles <- xml_text(all_titles)
descriptions <- xml_text(all_description)

#code to combine the titles and descriptions in a single list
all_rows <- c(titles, descriptions)

#code to tokenise the above created list into individual words or tokens
tokenised <- all_rows %>%
  tibble(text = .) %>%
  unnest_tokens(token, text)

#head(tokenised)

#code to check the count the occurance of each word
word_counts <- tokenised %>% 
  count(token)

#code to load generated sentiments in tidytext package stored into a csv file
nrc <- read_csv("nrc.csv")

#code to add sentiment to the tokens and further into sentiment counts
nrc_sentiment_counts <- nrc %>%
  inner_join(word_counts, by = c("word" = "token")) %>% 
  group_by(sentiment) %>%
  summarise(n = sum(n))


nrc_sentiment_counts |>  write_csv(format(Sys.Date(), "%Y-Week-%V.csv"))

writeLines(format(Sys.time()), "timestamp.txt") 


# List of CSV files representing 5 days
daily_file_paths <- c("C:/Users/Ashish Khatavkar/Downloads/2024-04-22.csv", 
                      "C:/Users/Ashish Khatavkar/Downloads/2024-04-23.csv", 
                      "C:/Users/Ashish Khatavkar/Downloads/2024-04-24.csv", 
                      "C:/Users/Ashish Khatavkar/Downloads/2024-04-25.csv", 
                      "C:/Users/Ashish Khatavkar/Downloads/2024-04-26.csv",
                      "C:/Users/Ashish Khatavkar/Downloads/2024-04-27.csv",
                      "C:/Users/Ashish Khatavkar/Downloads/2024-04-28.csv")

# Read data from each file and add a 'day' column to indicate which day the data belongs to
data_frames <- lapply(1:length(daily_file_paths), function(i) {
  df <- read_csv(daily_file_paths[i])
  df$day <- as.Date(paste("2024-04-", 21 + i, sep = ""))
  df
})

# Combine all dataframes into a single dataframe
df_combined <- do.call(rbind, data_frames)

# Group by day and sentiment, then sum the counts for each sentiment category over 5 days
sentiment_counts_by_day <- df_combined %>%
  group_by(day, sentiment) %>%
  summarise(total_count = sum(n)) %>%
  ungroup()

average_counts <- df_combined %>%
  group_by(sentiment) %>%
  summarise(average_count = mean(n)) %>%
  ungroup()

# Calculate percentages
total_count <- sum(average_counts$average_count)
average_counts$percentage <- average_counts$average_count / total_count * 100

# Create the pie chart with percentages
ggplot(average_counts, aes(x = "", y = average_count, fill = sentiment)) +
  geom_bar(width = 1, stat = "identity") +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), position = position_stack(vjust = 0.5)) +
  coord_polar("y", start = 0) +
  theme_void() +
  labs(title = "Average Count per Sentiment", fill = "Sentiment") +
  theme(legend.position = "right") +
  guides(fill = guide_legend(title = "Sentiment"))

ggplot(average_counts, aes(x = "", y = average_count, fill = sentiment)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  theme_void() +
  labs(title = "Average count per sentinemt",fill = "Sentiment") +
  theme(legend.position = "right") +
  guides(fill = guide_legend(title = "Sentiment"))

ggplot(data = sentiment_counts_by_day, aes(x = day, y = total_count, color = sentiment, group = sentiment)) +
  geom_line() +
  geom_point() +
  labs(title = "Sentiment Counts Over 7 Days From 22 April 2024 to 28 April 2024",
       x = "Day",
       y = "Sentiment Count") +
  theme_minimal() +
  theme(legend.position = "right", axis.text.x = element_text(angle = 60, hjust = 0.7)) +
  scale_x_date(date_breaks = "1 day", date_labels = "%b %d") +
  scale_color_manual(values = c("anger" = "red",
                                "anticipation" = "purple",
                                "disgust" = "magenta",
                                "fear" = "#3182BD",
                                "joy" = "cyan",
                                "negative" = "black",
                                "positive" = "orange",
                                "sadness" = "grey",
                                "surprise" = "yellow",
                                "trust" = "green"))



# Assuming 'sentiment_counts' is your combined dataframe and it has columns 'Sentiment', 'Count', and 'Day'
# Let's first convert 'Day' into a factor for consistent coloring and ordering in the plot
sentiment_counts_by_day$day <- as.factor(sentiment_counts_by_day$day)

#create the bar chart
ggplot(sentiment_counts_by_day, aes(x = sentiment, y = total_count, fill = day)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Sentiment Counts Over Days", x = "Sentiment", y = "Count") +
  theme_minimal() +
  theme(legend.position = "right", plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 60, hjust = 0.7))



