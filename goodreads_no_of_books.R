# load libraries
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)

# read csv file exported from goodreads. I have been using Goodreads since 2015. So I filtered accordingly
goodreads_books <- read.csv("goodreads_library_export.csv") %>%
                   filter(Exclusive.Shelf == "read") %>%
                   select(Title, Author, Average.Rating, Number.of.Pages, Date.Read, My.Rating) %>%
                   mutate(Date.Read = ymd(Date.Read)) %>%
                   mutate(year = year(Date.Read),
                     year_month = format(Date.Read, "%Y-%m")) %>%
                   filter(year >= 2015 & year <= 2023)
                   

# remove January 2024 as it is not finished yet and find number of books per month
read_stats <- goodreads_books %>% 
              filter(!is.na(Date.Read) & year_month != "2024-01") %>%
              group_by(year_month) %>%
              summarise(read_count = n()) %>%
              ungroup()

# find top 3 months together and separately
top_months <- head(read_stats[order(read_stats$read_count, decreasing = TRUE), ], 3)

top_month <- top_months %>% slice(1)
second_highest <- top_months %>% slice(2)
third_highest <- top_months %>% slice(3)
                   
# create the chart with labels and texts
ggplot(read_stats, aes(x = year_month, read_count)) + 
  geom_bar( stat = "identity", fill = "#143c8a") +
  geom_text(data = top_months, aes(x = year_month, y = read_count, label = read_count, fontface = "bold"),
            color = "red", size = 3, vjust = -0.5) +
  geom_text(data = top_month, aes(x = year_month, y = read_count, label = "I had one month between jobs"),
            color = "black", size = 4 , vjust = -0.5, hjust = -0.1) + 
  geom_text(data = second_highest, aes(x = year_month, y = read_count, label = "Our son was born previous month, lots of watching him breathe = lots of audibooks"),
            color = "black", size = 4 , vjust = -1.5) + 
  geom_text(data = third_highest, aes(x = year_month, y = read_count, label = "First month of unemployment"),
            color = "black", size = 4 , vjust = 0.5, hjust = 1.1) +
  theme_classic() +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, max(read_stats$read_count) + 2)
    ) +
  labs(title = "Number of Books Read by Month") +
  theme(
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 45, vjust = 0.5, size = 6)
  ) 
