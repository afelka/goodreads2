# load libraries
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(writexl)
library(readxl)
library(maps)
library(rayshader)
library(magick)


# read csv file exported from goodreads. I have been using Goodreads since 2015. So I filtered accordingly
goodreads_books <- read.csv("goodreads_library_export.csv") %>%
                   filter(Exclusive.Shelf == "read") %>%
                   select(Title, Author, Average.Rating, Number.of.Pages, Date.Read, My.Rating) %>%
                   mutate(Date.Read = ymd(Date.Read)) %>%
                   mutate(year = year(Date.Read),
                     year_month = format(Date.Read, "%Y-%m")) %>%
                   filter(year >= 2015 & year <= 2024)
                   
#remove March 2024 as it is not finished yet
read_stats <- goodreads_books %>% 
              filter(!is.na(Date.Read) & year_month != "2024-03") %>%
              group_by(year_month) %>%
              summarise(read_count = n()) %>%
              ungroup()

# find top 5 months together and separately
top_months <- head(read_stats[order(read_stats$read_count, decreasing = TRUE), ], 5)

top_month <- top_months %>% slice(1)
second_highest <- top_months %>% slice(2)
third_highest <- top_months %>% slice(3)
fourth_highest <- top_months %>% slice(4)
fifth_highest <- top_months %>% slice(5)
                   
# create the chart with labels and texts
books_over_month <- ggplot(read_stats, aes(x = year_month, read_count)) + 
  geom_bar( stat = "identity", fill = "#143c8a") +
  geom_text(data = top_months, aes(x = year_month, y = read_count, label = read_count, fontface = "bold"),
            color = "red", size = 3, vjust = -0.5) +
  geom_text(data = top_month, aes(x = year_month, y = read_count, label = "I had one month between jobs"),
            color = "black", size = 4 , vjust = -0.5, hjust = -0.1) + 
  geom_text(data = second_highest, aes(x = year_month, y = read_count, label = "Our son was born previous month, lots of watching him breathe = lots of audibooks"),
            color = "black", size = 4 , vjust = -1.5) + 
  geom_text(data = third_highest, aes(x = year_month, y = read_count, label = "First 3 months of unemployment"),
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

ggsave("no_of_books_per_month.png", plot = books_over_month, width = 6, height = 4, dpi = 300)

#Create unique author 
authors_unique <- read.csv("goodreads_library_export.csv") %>%
  filter(Exclusive.Shelf == "read") %>%
  select(Title, Author, Average.Rating, Number.of.Pages, Date.Read, My.Rating) %>% group_by(Author) %>%
  summarise(read_count = n()) %>% arrange(desc(read_count)) 

#to add countries for authors manually
#write_xlsx(authors_unique, "authors_unique.xlsx")

#country information is added manually
authors_unique_country_added <- read_excel("authors_unique.xlsx")

authors_summary <- authors_unique_country_added %>% group_by(country) %>% 
                   summarise(book_count = sum(read_count),
                             authour_count = n())

#read world data from maps package
world <- map_data("world")

world_joined_all_countries <- left_join(world, authors_summary, by = c("region" = "country")) %>%
  arrange(book_count)

#create theme for ggplot
plain <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank(),
  panel.background = element_rect(fill = "white"),
  plot.title = element_text(hjust = 0.5,),
  plot.subtitle = element_text(hjust = 0.5,)
)


authors_per_country <- ggplot(data = world_joined_all_countries, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = authour_count), color = "white") +  
  scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Author Count") +  
  ggtitle("Unique Authors Read per Country") +
  theme(plot.title = element_text(size = 8, face = "bold")) +
  plain +
  theme(legend.position = "none") 


books_per_country <- ggplot(data = world_joined_all_countries, mapping = aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = book_count), color = "white") +
  scale_fill_gradient(low = "pink", high = "darkred", name = "Book Count") +
  plain +
  ggtitle("Books Read per Country") +
  theme(plot.title = element_text(size = 8, face = "bold")) +
  theme(legend.position = "none") 

#change theta of the 3d plot and write png output  
for (i in 45:135) {

# Convert ggplot to 3D plot
books_per_country3d <- plot_gg(books_per_country,
                               theta=i,
                               windowsize = c(1400, 866)
                               )
j <- i - 44

filename <- paste0("no_of_books_3d_", j , ".png")

render_snapshot(
  filename = filename
  , clear = TRUE
  
)

}


#change phi of the 3d plot and write png output
for (x in 45:30) {
  
  # Convert ggplot to 3D plot
  books_per_country3d <- plot_gg(books_per_country,
                                 theta= 135,
                                 phi = x,
                                 windowsize = c(1400, 866)
  )
  
  
  filename <- paste0("no_of_books_3d_", j , ".png")
  
  render_snapshot(
    filename = filename
    , clear = TRUE
    
  )
  j <<- j + 1
}

#change theta of the 3d plot on the opposite and write png output  
for (i in 135:45) {
  
  # Convert ggplot to 3D plot
  books_per_country3d <- plot_gg(books_per_country,
                                 theta=i,
                                 phi = 30,
                                 windowsize = c(1400, 866)
  )
  
  
  filename <- paste0("no_of_books_3d_", j , ".png")
  
  render_snapshot(
    filename = filename
    , clear = TRUE
    
  )
  j <<- j + 1
}

#create data_frame with images in correct reading order
image_data <- data.frame(
  image_id = 1:197,
  image_path = paste0("./no_of_books_3d_", 1:197, ".png")
)

# read images in a list
img_list <- lapply(image_data$image_path, image_read)

## join the images together
img_joined <- image_join(img_list)

## animate at 1 frames per second
img_animated <- image_animate(img_joined, fps = 4)

# write the gif
image_write(image = img_animated,
            path = "books_read_3d.gif")


