#' ---
#' title: 'Challenge 2: predicting users’ film ratings'
#' author: Samaher
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
#' # Movie and User effect Model with regularization
#' The primary goal is to find the optimal values for 
#' lambda_1 and lambda_2 that minimize the Mean Squared Error (MSE) between the model's predicted ratings and the actual ratings in the test dataset. 
#' lambda_1 applies to movie effects, and lambda_2 applies to user effects, allowing differentiated control over regularization strength for these two aspects of the model.
#' 
#' begins by calculating the global average rating across all movies and users. This average serves as a baseline for adjusting individual movie and user ratings, helping to normalize the data.
#' 
#' A grid of possible values for 
#' lambda_1 and lambda_2  is defined, covering a specified range with a set increment. 
#' 
#' Through nested loops, each combination of 
#' lambda_1 and lambda_2  is evaluated. For each combination, the model recalculates movie and user effects with the current values of lambda_1 and lambda_2, incorporating regularization into these calculations.
#' 
#' With the regularized movie and user effects, the model predicts ratings for the validation dataset. These predictions are adjusted to conform to the typical rating scale before calculating the MSE for the current combination of lambdas, providing a measure of predictive accuracy.
#' 
#' After evaluating all combinations, the pair of 
#' lambda_1 and lambda_2 that results in the lowest MSE is identified as optimal. This pair represents the best regularization strengths for minimizing overfitting while maintaining or enhancing predictive accuracy.
#' 
#' 
#' 
## -------
library(dplyr)
# Calculate Global Average Rating
mu <- mean(train_data$rating)
# Define a sequence of lambda values to test
lambda_values <- seq(0, 10, by = 0.1)
# Prepare a matrix to store MSE for each lambda
mse_values <- matrix(NA,nrow = length(lambda_values),ncol = length(lambda_values))


# Loop over each lambda to evaluate model performance
for (i in seq_along(lambda_values)) {
  for (j in seq_along(lambda_values)) {
  lambda_1 <- lambda_values[i]
  lambda_2 <- lambda_values[j]
  
    movie_reg_means <- train_data %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu) / (n() + lambda_1), n_i = n()) 
  
  user_reg_means <- train_data %>%
    left_join(movie_reg_means) %>%
    mutate(resids = rating - mu - b_i) %>%
    group_by(userId) %>%
    summarize(b_u = sum(resids) / (n() + lambda_2))
  
  # Predict ratings on the validation set
  joined <- test_data %>%
    left_join(movie_reg_means, by = 'movieId') %>%
    left_join(user_reg_means, by = 'userId') %>%
    tidyr::replace_na(list(b_i = 0, b_u = 0))
  
  predicted_ratings <- mu + joined$b_i + joined$b_u
  
  
  # Adjust the predictions to the nearest 0.5
  adjusted_predictions = round(predicted_ratings * 2) / 2
  # Restricting predicted ratings to be within the 0.5 to 5.0 range
  adjusted_predictions = pmax(pmin(adjusted_predictions, 5), 0.5)
  
  # Calculate MSE for the current lambda
  mse_values[i,j] <- mean((adjusted_predictions - test_data$rating) ^ 2)
  }
}

# Identify the lambda1 and lambda2 with the minimum MSE
min_mse_index <- which(mse_values == min(mse_values), arr.ind = TRUE)
optimal_lambda_1 <- lambda_values[min_mse_index[1]]
optimal_lambda_2 <- lambda_values[min_mse_index[2]]
min_mse <- mse_values[min_mse_index]



# Printing the optimal lambda_1, lambda_2, and their corresponding minimum MSE
print(paste("Optimal lambda_1: ", optimal_lambda_1, "\n","Optimal lambda_2: ", optimal_lambda_2, "\n","MSE: ", min(mse_values)))
 

#' 
#' 
#' 
#' run the predictions on ratings_test
#' 
## -------
# Calcualting the values of B_i and b_u using the chosen lambdas
 movie_reg_means <- train_data %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu) / (n() + optimal_lambda_1), n_i = n()) 
  
  user_reg_means <- train_data %>%
    left_join(movie_reg_means) %>%
    mutate(resids = rating - mu - b_i) %>%
    group_by(userId) %>%
    summarize(b_u = sum(resids) / (n() + optimal_lambda_2))
  
test_predictions <- ratings.test %>%
left_join(movie_reg_means, by = 'movieId') %>%
    left_join(user_reg_means, by = 'userId') %>%
    tidyr::replace_na(list(b_i = 0, b_u = 0))
  
  predicted_ratings <- mu + test_predictions$b_i + test_predictions$b_u
  
  
  # Adjust the predictions to the nearest 0.5
  adjusted_predictions = round(predicted_ratings * 2) / 2
  # Restricting predicted ratings to be within the 0.5 to 5.0 range
  adjusted_predictions = pmax(pmin(adjusted_predictions, 5), 0.5)


#' 
## -------
write.csv(adjusted_predictions, file = "film_rating_predictions_group_F_week_5.csv", row.names=FALSE)

#' 
