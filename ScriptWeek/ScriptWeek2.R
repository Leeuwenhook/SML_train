#' ---
#' title: 'Challenge 2: predicting users’ film ratings'
#' author: 
#' output: html_document
#' ---
#' 
## ----setup, include=FALSE---------------------------
knitr::opts_chunk$set(echo = TRUE)

#' 
#' 
#' # The data
#' Load the data 
## -------------------------------------------
ratings.train <- read.csv("ratings_train.csv", header=TRUE)
ratings.test <- read.csv("ratings_test.csv", header=TRUE)
movies<-read.csv('movies.csv',header = T)

#' 
## -------------------------------------------
head(ratings.train)

#' 
## -------------------------------------------
head(movies)

#' 
#' Rating distributions
#' The overall distribution of ratings to understand user preferences.
## --------------------------------------------
library(dplyr)
library(ggplot2)
library(ggplot2)

ggplot(ratings.train, aes(x = rating)) +
  geom_histogram(binwidth = 0.5, fill = "blue", color = "black") +
  ggtitle("Distribution of Ratings") +
  xlab("Ratings") +
  ylab("Frequency") +
  theme_minimal()

#' 
#' The total of unique movies and users in the train set 
## -------------------------------------------
ratings.train %>%
summarize(n_users = n_distinct(userId), 
          n_movies = n_distinct(movieId))

#' Number of ratings per movie
## ---------------------------------------------
ratings.train %>%
count(movieId) %>%
ggplot(aes(n)) +
geom_histogram(bins = 30, color = "blue",fill='blue') +
scale_x_log10() +
xlab("MovieId") +
  ylab("Number of ratings") +
ggtitle("Number of ratings per movie")+theme_bw()

#' 
#' 
#' 
#' Number of ratings per User
## --------------------------------------------
ratings.train %>%
count(userId) %>%
ggplot(aes(n)) +
geom_histogram(bins = 30, color = "blue",fill='blue') +
scale_x_log10() +
xlab("UserId") +
  ylab("Number of ratings") +
ggtitle("Number of ratings per User")+theme_bw()

#' 
#' 
#' 
#' 
#' 
#' 
#' Let's construct some features from the timestamp variable
#' 
## --------------------------------------------
ratings.train$timestamp <- as.POSIXct(ratings.train$timestamp)

# Extract day of the week
ratings.train$day_of_week <- weekdays(ratings.train$timestamp)

# You could also create a binary feature for weekend vs. weekday
ratings.train$is_weekend <- ifelse(weekdays(ratings.train$timestamp) %in% c("Saturday", "Sunday"), 1, 0)

# Extract part of the day
ratings.train$part_of_day <- cut(as.integer(format(ratings.train$timestamp, "%H")),
                           breaks=c(-Inf,6,12,18,Inf),
                           labels=c("Night", "Morning", "Afternoon", "Evening"),
                           right=FALSE)
# Extract Year
ratings.train$year <- format(ratings.train$timestamp, "%Y")

# Define a function to determine the season based on the month
getSeason <- function(month) {
  ifelse(month %in% c(3, 4, 5), 'Spring',
         ifelse(month %in% c(6, 7, 8), 'Summer',
                ifelse(month %in% c(9, 10, 11), 'Fall', 'Winter')))
}

# Extract Month as a numeric value
ratings.train$month <- as.numeric(format(ratings.train$timestamp, "%m"))

# Apply the function to determine the season
ratings.train$season <- sapply(ratings.train$month, getSeason)

#' 
#' Let's merge with the movie data for additional insights
#' 
## --------------------------------------------
library(dplyr)
ratings.train<-ratings.train %>% left_join(movies,'movieId')

#' 
#' Heatmap of Ratings by Part of Day and Day of Week
#' if there are specific times when higher or lower ratings are given.
## -------------------------------------------
library(ggplot2)

ratings.train %>%
  mutate(day_of_week = factor(day_of_week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")),
         part_of_day = factor(part_of_day, levels = c("Night", "Morning", "Afternoon", "Evening"))) %>%
  group_by(day_of_week, part_of_day) %>%
  summarize(avg_rating = mean(rating, na.rm = TRUE)) %>%
  ggplot(aes(x = day_of_week, y = part_of_day, fill = avg_rating)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "red") +
  ggtitle("Average Ratings by Part of Day and Day of Week") +
  xlab("Day of Week") +
  ylab("Part of Day") +
  theme_minimal()


#' 
#' Ratings Over Time
#' To observe how the average movie rating evolves over time
## --------------------------------------------------
library(ggplot2)
library(dplyr)

ratings.train %>%
  mutate(year = as.factor(year)) %>%
  group_by(year) %>%
  summarize(avg_rating = mean(rating, na.rm = TRUE)) %>%
  ungroup() %>% 
  ggplot(aes(x = year, y = avg_rating, fill = avg_rating)) +  # Added fill for a gradient effect, optional
  geom_bar(stat = "identity", color = "black") +  # Using geom_bar with stat="identity" for pre-summarized data
  scale_fill_gradient(low = "lightblue", high = "darkblue") +  # Gradient fill from light to dark blue, optional
  ggtitle("Average Movie Rating Over Time") +
  xlab("Year") +
  ylab("Average Rating") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#' 
#' User Rating Behavior by Season
#' Identify if there's a seasonal pattern in how users rate movies
#' 
## --------------------------------------------------
ratings.train %>%
  group_by(season) %>%
  summarize(avg_rating = mean(rating, na.rm = TRUE)) %>%
  ggplot(aes(x = season, y = avg_rating, fill = season)) +
  geom_bar(stat = "identity") +
  ggtitle("Average Rating by Season") +
  xlab("Season") +
  ylab("Average Rating") +
  theme_minimal()


#' 
#' 
## ------------------------------------------
# Split each genre string into individual genres
split_genres <- strsplit(as.character(ratings.train$genres), split = "\\|")

# Unlist and find unique genres across all movies
unique_genres <- unique(unlist(split_genres))

# Initialize a matrix to hold binary variables for genres, with the same number of rows as the 'ratings.train' dataset
genre_matrix <- matrix(0, nrow = nrow(ratings.train), ncol = length(unique_genres))

# Name the columns after the unique genres
colnames(genre_matrix) <- unique_genres

# Fill the matrix: set to 1 where the movie belongs to the genre
for (i in 1:nrow(ratings.train)) {
  genres <- strsplit(as.character(ratings.train$genres[i]), split = "\\|")[[1]]
  genre_matrix[i, genres] <- 1
}

# Convert the matrix to a data frame 
genre_df <- as.data.frame(genre_matrix)
# Bind the genre binary variables with the original dataset
ratings.train.expanded <- cbind(ratings.train, genre_df)


#' 
#' how genres influence ratings
## ---------------------------------------
library(dplyr)
library(tidyr)


ratings.train.expanded %>%
  gather(key = "genre", value = "present", Action:`(no genres listed)`) %>%
  filter(present == 1) %>%
  group_by(genre) %>%
  summarize(avg_rating = mean(rating, na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(genre, avg_rating), y = avg_rating)) +
  geom_bar(stat = "identity", fill = "blue") +
  coord_flip() +
  ggtitle("Average Rating per Genre") +
  xlab("Genre") +
  ylab("Average Rating") +
  theme_minimal()


#' 
#' how genre preferences have changed over time
#' 
## ----------------------------------------
ratings.train.expanded %>%
  gather(key = "genre", value = "present",  `Action`:`(no genres listed)`) %>%
  filter(present == 1) %>%
  group_by(year, genre) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = year, y = count, fill = genre)) +
  geom_bar(stat="identity") +  # Use bars and dodge position for clarity
  ggtitle("Genre Popularity Over Time") +
  xlab("Year") +
  ylab("Number of Ratings") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 4, byrow = TRUE))



#' 
#' Distribution of Ratings by Genre
#' how ratings are distributed across different genres
#' 
## ----------------------------------------
ratings.train.expanded %>%
  gather(key = "genre", value = "present",  `Action`:`(no genres listed)`) %>%
  filter(present == 1) %>%
  ggplot(aes(x = genre, y = rating)) +
  geom_boxplot() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Distribution of Ratings by Genre")


#' 
#' 
#' Number of Ratings per Genre
#' which genres are most popular among users based on the number of ratings.
## ----------------------------------------
ratings.train.expanded %>%
  gather(key = "genre", value = "present", `Action`:`(no genres listed)`) %>%
  filter(present == 1) %>%
  count(genre) %>%
  ggplot(aes(x = reorder(genre, n), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  ggtitle("Number of Ratings per Genre") +
  xlab("") +
  ylab("Number of Ratings") +
  theme_minimal()


#' 
#' 
## ---------------------------------------

library(caret)

library(janitor)
ratings.train.expanded<-clean_names(ratings.train.expanded)

# Define the predictor variables and the target variable
predictors <- c("part_of_day", "is_weekend", "year", "season", "action", "drama", "thriller",
                "comedy", "adventure", "sci_fi", "imax", "fantasy", "romance", "horror",
                "children", "war", "animation", "mystery", "crime", "documentary", "western",
                "musical", "film_noir", "no_genres_listed")

# Convert factor variables
ratings.train.expanded$part_of_day <- as.factor(ratings.train.expanded$part_of_day)

ratings.train.expanded$season <- as.factor(ratings.train.expanded$season)
ratings.train.expanded$year <- as.factor(ratings.train.expanded$year)   
# Split data into training (80%) and testing (20%) sets
set.seed(123) # For reproducibility
indices <- createDataPartition(ratings.train.expanded$rating, p = 0.8, list = FALSE)
trainingData <- ratings.train.expanded[indices, ]
testingData <- ratings.train.expanded[-indices, ]

#' 
#' 
#' 
#' 
#' 
#' Now we prepare the ratings.test data
#' 
## ------------------------------------
ratings.test$timestamp <- as.POSIXct(ratings.test$timestamp)

# Extract day of the week
ratings.test$day_of_week <- weekdays(ratings.test$timestamp)

# You could also create a binary feature for weekend vs. weekday
ratings.test$is_weekend <- ifelse(weekdays(ratings.test$timestamp) %in% c("Saturday", "Sunday"), 1, 0)

# Extract part of the day
ratings.test$part_of_day <- cut(as.integer(format(ratings.test$timestamp, "%H")),
                           breaks=c(-Inf,6,12,18,Inf),
                           labels=c("Night", "Morning", "Afternoon", "Evening"),
                           right=FALSE)
# Extract Year
ratings.test$year <- format(ratings.test$timestamp, "%Y")

# Define a function to determine the season based on the month
getSeason <- function(month) {
  ifelse(month %in% c(3, 4, 5), 'Spring',
         ifelse(month %in% c(6, 7, 8), 'Summer',
                ifelse(month %in% c(9, 10, 11), 'Fall', 'Winter')))
}

# Extract Month as a numeric value
ratings.test$month <- as.numeric(format(ratings.test$timestamp, "%m"))

# Apply the function to determine the season
ratings.test$season <- sapply(ratings.test$month, getSeason)
#merge with the movies data to get the genres
ratings.test <- ratings.test %>% left_join(movies,'movieId')

# Split each genre string into individual genres
split_genres <- strsplit(as.character(ratings.test$genres), split = "\\|")

# Unlist and find unique genres across all movies
unique_genres <- unique(unlist(split_genres))

# Initialize a matrix to hold binary variables for genres, with the same number of rows as the 'ratings.test' dataset
genre_matrix <- matrix(0, nrow = nrow(ratings.test), ncol = length(unique_genres))

# Name the columns after the unique genres
colnames(genre_matrix) <- unique_genres

# Fill the matrix: set to 1 where the movie belongs to the genre
for (i in 1:nrow(ratings.test)) {
  genres <- strsplit(as.character(ratings.test$genres[i]), split = "\\|")[[1]]
  genre_matrix[i, genres] <- 1
}

# Convert the matrix to a data frame 
genre_df <- as.data.frame(genre_matrix)
# Bind the genre binary variables with the original dataset
ratings.test.expanded <- cbind(ratings.test, genre_df)

#' 
#' 
## ----------------------------------------
# Convert factor variables
ratings.test.expanded$part_of_day <- factor(ratings.test.expanded$part_of_day,levels=levels(ratings.train.expanded$part_of_day))

ratings.test.expanded$season <- factor(ratings.test.expanded$season,levels=levels(ratings.train.expanded$season))
ratings.test.expanded$year <- factor(ratings.test.expanded$year,levels = levels(ratings.train.expanded$year))
ratings.test.expanded<-clean_names(ratings.test.expanded)

#' 
#' 
#' 
#' 
#' Let's train a random forest model to improve
#' 
#' 
## ---------------------------------------
library(caret)
library(ranger)
num_predictors <- length(predictors)
# Define the model using caret with the ranger method
model <- train(
  formula,
  data = trainingData,
  method = "ranger",
  trControl = trainControl(method = "cv", number = 10),
  tuneGrid = expand.grid(
    .mtry = c(2:num_predictors),
    .splitrule = c("variance", "extratrees", "maxstat"),
    .min.node.size = 5
  ),
  metric = "MSE",
  seed = 42
)

#' 
#' print the results
## --------------------------------------------
model


#' 
#' the final model
#' 
## --------------------------------------
model$finalModel


#' 
#' Rebuilding the model to calculate variable importance
#' 
## ----------------------------------------
library(ranger)

rf_model <- ranger(
formula, 
  data = trainingData,                 # The data frame containing the training data
  mtry =8, # max_features: 50% of variables
  min.node.size = 5,                   # min_samples_leaf: Minimum node size
  num.trees = 500,                     # n_estimators: Number of trees
  seed = 42,                           # random_state: Seed for reproducibility
  importance = 'permutation',          # To calculate variable importance
  write.forest = TRUE,                 #  to make predictions, save the forest
  verbose = TRUE
)



#' 
#' variable importance
## ---------------------------------------
importance(rf_model)

#' 
## ----------------------------------------
# Make predictions on the test set
predictions <- predict(model, newdata = testingData)
# Adjust the predictions to the nearest 0.5
adjusted_predictions <- round(predictions * 2) / 2
# Calculate MSE
test_rmse <- mean((adjusted_predictions - testingData$rating)^2)
print(paste("Test MSE:", test_rmse))

#' 
## ---------------------------------------
# Make predictions on the test set
predictions <- predict(model, newdata = ratings.test.expanded)
# Adjust the predictions to the nearest 0.5
adjusted_predictions <- round(predictions * 2) / 2

#' 
## ---------------------------------------
write.csv(adjusted_predictions, file = "film_rating_predictions_group_F_week_2.csv", row.names=FALSE)

