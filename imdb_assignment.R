# Turkish characters detection

Sys.setlocale("LC_ALL", "Turkish")

# Packages

library(tidyverse) 
library(rvest) 
library(stringr) 
library(ggplot2)

# combining 2 URL's into a single vector

url_1 <- "https://m.imdb.com/search/title/?title_type=feature&release_date=2010-01-01,2023-12-31&num_votes=2500,&country_of_origin=TR&count=250"

url_2 <- "https://m.imdb.com/search/title/?title_type=feature&release_date=,2009-12-31&num_votes=2500,&country_of_origin=TR&count=250"

combine_vector <- c(url_1, url_2)

# creating empty vectors

titles <- c()
years <- c()
durations <- c()
ratings <- c()
votes <- c()

# Scraping the data

for (url in combine_vector) {
  page <- read_html(url)
  
  # Extract movie names
  
  title_names <- page %>% html_nodes('.ipc-title__text') %>% html_text()
  title_names <- tail(head(title_names, -1), -1)
  title_names <- str_split(title_names, " ", n=2)
  title_names <- unlist(lapply(title_names, function(x) {x[2]}))
  
  
  # Extract year
  year <- page %>% html_nodes('.sc-300a8231-7:nth-child(1)') %>% html_text() %>% substr(1, 4) %>% as.numeric()
  
  
  # Extract rating
  rating <- page %>%
    html_nodes('.ipc-rating-star--rating') %>%
    html_text() %>%
    substr(1, 3) %>%
    as.numeric()
  
  
  # Extract number of votes
  vote <- page %>% html_nodes(".ipc-rating-star--voteCount") %>% html_text() %>% parse_number()
  vote <- gsub("\\(|\\)| ", "", vote)
  vote <- as.numeric(vote)
  
  
  # Extract duration
  duration <- page %>% html_nodes('.sc-300a8231-7:nth-child(2)') %>% html_text()
  
  
  # Extract hour part, if present
  hour <- str_extract(duration, "\\d+h") %>%
    str_replace("h", "") %>%
    as.numeric()
  
  
  # If hour is NA, set it to 0
  hour[is.na(hour)] <- 0
  
  
  # Extract minute part, if present
  minute <- str_extract(duration, "\\d+m") %>%
    str_replace("m", "") %>%
    as.numeric()
  
  
  # If minute is NA, set it to 0
  minute[is.na(minute)] <- 0
  
  
  # Calculate total duration
  total_duration <- (hour * 60) + minute
  
  
  # Append data to vectors
  titles <- append(titles, title_names)
  years <- append(years, year)
  ratings <- append(ratings, rating)
  votes <- append(votes, vote)
  durations <- append(durations, total_duration)
}


# Create a data frame from the scraped data
imdb_data <- data.frame(Title = titles, Year = years, Duration = durations, Rating = ratings, Votes = votes)


# Print the first few rows
print(head(imdb_data))





# Sort by rating in descending order and get the top 5

top_5 <- imdb_data %>% 
  arrange(desc(Rating)) %>% 
  head(5)

print(top_5)

# Sort by rating in ascending order and get the bottom 5

bottom_5 <- imdb_data %>% 
  arrange(Rating) %>% 
  head(5)

print(bottom_5)



# Filter the data for the movies "Aile Arasında" and "Av Mevsimi"
selected_movies <- imdb_data %>% 
  filter(Title %in% c("Aile Arasında", "Av Mevsimi"))

# Print the results
cat("Ratings for the selected movies:\n")
print(selected_movies[, c("Title", "Rating")])




# Calculate the average rating for each year
yearly_avg <- imdb_data %>%
  group_by(Year) %>%
  summarise(Average_Rating = mean(Rating, na.rm = TRUE))

# Plotting yearly average ratings
ggplot(yearly_avg, aes(x = Year, y = Average_Rating)) +
  geom_point(color = "red", size = 3) +
  geom_line(color = "blue", size = 1) +
  labs(
    title = "Yearly Average Movie Ratings",
    x = "Year",
    y = "Average Rating"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12)
  )



# Create a box plot for yearly movie ratings
ggplot(imdb_data, aes(x = factor(Year), y = Rating)) +
  geom_boxplot(fill = "skyblue", color = "darkblue", outlier.color = "red") +
  labs(
    title = "Box Plot of Movie Ratings by Year",
    x = "Year",
    y = "Rating"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )





# Calculate correlation between Votes and Rating
correlation <- cor(imdb_data$Votes, imdb_data$Rating, use = "complete.obs")

# Print the correlation result
print(paste("Correlation between Votes and Rating: ", correlation))







# Scrapping Turkish Movies in top 1000

url_new <- "https://m.imdb.com/search/title/?title_type=feature&groups=top_1000&country_of_origin=TR"

# creating empty vectors

titles <- c()
years <- c()
durations <- c()
ratings <- c()
votes <- c()

for(url in url_new){
  page = read_html(url)

# Extract movie names

title_names <- page %>% html_nodes('.ipc-title__text') %>% html_text()
title_names <- tail(head(title_names, -1), -1)
title_names <- str_split(title_names, " ", n=2)
title_names <- unlist(lapply(title_names, function(x) {x[2]}))


# Extract year
year <- page %>% html_nodes('.sc-300a8231-7:nth-child(1)') %>% html_text() %>% substr(1, 4) %>% as.numeric()


# Extract rating
rating <- page %>%
  html_nodes('.ipc-rating-star--rating') %>%
  html_text() %>%
  substr(1, 3) %>%
  as.numeric()


# Extract number of votes
vote <- page %>% html_nodes(".ipc-rating-star--voteCount") %>% html_text() %>% parse_number()
vote <- gsub("\\(|\\)| ", "", vote)
vote <- as.numeric(vote)


# Extract duration
duration <- page %>% html_nodes('.sc-300a8231-7:nth-child(2)') %>% html_text()


# Extract hour part, if present
hour <- str_extract(duration, "\\d+h") %>%
  str_replace("h", "") %>%
  as.numeric()


# If hour is NA, set it to 0
hour[is.na(hour)] <- 0


# Extract minute part, if present
minute <- str_extract(duration, "\\d+m") %>%
  str_replace("m", "") %>%
  as.numeric()


# If minute is NA, set it to 0
minute[is.na(minute)] <- 0


# Calculate total duration
total_duration <- (hour * 60) + minute


# Append data to vectors
titles <- append(titles, title_names)
years <- append(years, year)
ratings <- append(ratings, rating)
votes <- append(votes, vote)
durations <- append(durations, total_duration)

}

# Create a data frame from the scraped data
top1000_turkish <- data.frame(Title = titles, Year = years, Duration = durations, Rating = ratings, Votes = votes)


# Print the dataframe 
print(top1000_turkish[1:2])



# Sorting from largest to smallest according to rating value
top1000_turkish_sorted <- top1000_turkish[order(-top1000_turkish$Rating), ]

print(top1000_turkish_sorted)



