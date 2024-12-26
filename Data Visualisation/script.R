library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(plotly)
library(visdat)
library(htmlwidgets)
library(patchwork)

apps_data <- read_csv("C:/Users/Ashish Khatavkar/Downloads/Google apps dataset/googleplaystore.csv")

#code to check missing data in the columns
missing_data <- apps_data %>% vis_miss()

# write the dataframe to a csv file
write_csv(apps_data, "data.csv")

#Static Plot

#code to take average of the installs wrt to category and store it in a variable
avg_installs_data <- apps_data %>%
  group_by(Category) %>%
  summarize(Avg_Installs = mean(Installs, na.rm = TRUE)) %>% 
  arrange(desc(Avg_Installs))

#category wise installs bar plot
categorywise_installs_plot <- ggplot(avg_installs_data, aes(x = reorder(Category, -Avg_Installs), y = Avg_Installs, fill = Category)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Category-wise Average Installs of Apps",
    x = "Category",
    y = "Average Installs") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8), 
        legend.position = "none") +
  scale_y_continuous(labels = scales::comma_format(scale = 1e-6, suffix = "M")) #change the scale to millions


avg_reviews_data <- apps_data %>%
  group_by(Category) %>%
  summarize(Avg_Reviews = mean(Reviews, na.rm = TRUE)) %>% 
  arrange(desc(Avg_Reviews))

#code to plot the category vs avg reviews plot
categorywise_reviews_plot <- ggplot(avg_reviews_data, aes(x = reorder(Category, -Avg_Reviews), y = Avg_Reviews, fill = Category)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Category-wise Average Reviews of Apps",
    x = "Category",
    y = "Average Reviews") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8), 
        legend.position = "none") +
  scale_y_continuous(labels = scales::comma_format(scale = 1e-6, suffix = "M"))

#code to get average size of the apps category wise
avg_size_data <- apps_data %>%
  group_by(Category) %>%
  summarize(Avg_size = mean(Size_MB, na.rm = TRUE)) %>% 
  arrange(desc(Avg_size))

#code to plot category vs avg size bar plot
categorywise_size_plot <- ggplot(avg_size_data, aes(x = reorder(Category, -Avg_size), y = Avg_size, fill = Category)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Category-wise Average Size of Apps",
    x = "Category",
    y = "Average Reviews") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8), 
        legend.position = "none")

#code to plot bar plot category wise number of apps
categorywise_count_plot <- ggplot(apps_data, aes(x = Category, fill = Category)) +
  geom_bar() +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5, size = 3, color = "black") +
  labs(
    title = "Category-wise Count of Apps",
    x = "Category",
    y = "App Count"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8), legend.position = "none")


#code to combine above 4 plots
static_plot <- (categorywise_count_plot + categorywise_size_plot) /(categorywise_installs_plot + categorywise_reviews_plot)

#code to save the plot
ggsave("visualisation_1.png", plot = static_plot, width = 16, height = 10)

#Interactive Plot

#code to plot box plot of ratings vs Category  
Category_rating_boxplot <- ggplot(apps_data, aes(x = Category, y = Rating, fill = Category)) +
  geom_boxplot(color = "darkgray", alpha = 0.7) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, hjust = 0.5, color = "#333333"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
    axis.title = element_text(size = 10),
    legend.position = "none") +
  labs(
    title = "DISTRIBUTION OF RATINGS ACROSS APP CATEGORIES",
    x = "Category",
    y = "Ratings")

#using ggplotly to create an interactive plot
Category_rating_boxplot_interactive <- ggplotly(Category_rating_boxplot, dynamicTicks = TRUE)

#code to save the plot as html file
htmlwidgets::saveWidget(Category_rating_boxplot_interactive, "visualisation_2.html")

