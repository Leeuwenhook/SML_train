#' ---
#' title: 'Challenge 2: predicting users’ film ratings'
#' author: 
#' output: html_document
#' ---
#' 
## ----setup, include=FALSE-----------------------------
knitr::opts_chunk$set(echo = TRUE)

#' 
#' 
#' # The data
#' Load the data 
## -------
ratings.train <- read.csv("ratings_train.csv", header=TRUE)
ratings.test <- read.csv("ratings_test.csv", header=TRUE)
movies<-read.csv('movies.csv',header = T)

#' 
## -------
head(ratings.train)

#' 
## -------
head(movies)

#' 
#' # Split ratings_train data into train_data and test_data for evaluation
#' 
## -------
library(caret)
set.seed(42)
test_index <- createDataPartition(y = ratings.train$rating, times = 1, p = 0.2, list = FALSE)
train_data <- ratings.train[-test_index,]
test_data <- ratings.train[test_index,]

#' 
#' 
#' # Movie and User effect Model
#' 
#' # Calculate Global Average Rating
## -------
mu <- mean(train_data$rating)

#' 
#' # Calculate Movie Effects
#' For each movie, calculate its effect by finding the deviation of its average rating from the global average.
#' 
#' 
#' 
## -------
library(dplyr)

movie_effects <- train_data %>%
  group_by(movieId) %>%
  summarise(movie_avg = mean(rating), 
            count = n()) %>%
  mutate(b_i = movie_avg - mu)


#' 
#' # Calculate User Effects
#' For each user, calculate their effect by considering the deviation of their average rating (after removing the movie effect) from the global average.
#' 
## -------
user_effects <- train_data %>%
  left_join(movie_effects, by = "movieId") %>%
  group_by(userId) %>%
  summarise(user_avg = mean(rating - b_i),
            count = n()) %>%
  mutate(b_u = user_avg - mu)


#' 
#' 
#' # Predict Ratings
#' Predict ratings for the test dataset by adding the global average, movie effect, and user effect. If a movie or user is not present in the training set, we implement a fallback strategy.
#' 
## -------
predictions <- test_data %>%
  left_join(movie_effects, by = "movieId", suffix = c("", ".movie")) %>%
  left_join(user_effects, by = "userId", suffix = c("", ".user")) %>%
  rowwise() %>%
  mutate(predicted_rating = mu + coalesce(b_i, 0) + coalesce(b_u, 0)) %>%
  # Adjust the predictions to the nearest 0.5
mutate(adjusted_predictions =round(predicted_rating * 2) / 2) %>% 
  # Restricting predicted ratings to be within the 0.5 to 5.0 range
  mutate(adjusted_predictions = pmax(pmin(adjusted_predictions, 5), 0.5))


#' 
## -------

# Calculate MSE
test_mse <- mean((predictions$adjusted_predictions - test_data$rating)^2)
test_mse

#' run the predictions on ratings_test
#' 
## -------
test_predictions <- ratings.test %>%
  left_join(movie_effects, by = "movieId", suffix = c("", ".movie")) %>%
  left_join(user_effects, by = "userId", suffix = c("", ".user")) %>%
  rowwise() %>%
  mutate(predicted_rating = mu + coalesce(b_i, 0) + coalesce(b_u, 0)) %>%
  # Adjust the predictions to the nearest 0.5
mutate(adjusted_predictions =round(predicted_rating * 2) / 2) %>% 
  # Restricting predicted ratings to be within the 0.5 to 5.0 range
  mutate(adjusted_predictions = pmax(pmin(adjusted_predictions, 5), 0.5))


#' 
## -------
write.csv(test_predictions$adjusted_predictions, file = "film_rating_predictions_group_F_week_4.csv", row.names=FALSE)

#' 
