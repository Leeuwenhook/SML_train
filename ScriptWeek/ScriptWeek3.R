#' ---
#' title: 'Challenge 2: predicting users’ film ratings'
#' author: Samaher
#' output: html_document
#' ---
#' 
## ----setup, include=FALSE-----------------
knitr::opts_chunk$set(echo = TRUE)

#' 
#' 
#' # The data
#' Load the data 
## -----------------------------------------
ratings.train <- read.csv("ratings_train.csv", header=TRUE)
ratings.test <- read.csv("ratings_test.csv", header=TRUE)
movies<-read.csv('movies.csv',header = T)

#' 
## -----------------------------------------
head(ratings.train)

#' 
## -----------------------------------------
head(movies)

#' 
#' 
#' 
#' 
#' 
#' 
#' Let's construct some features from the timestamp variable
#' 
## -----------------------------------------
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
## -----------------------------------------
library(dplyr)
ratings.train<-ratings.train %>% left_join(movies,'movieId')

#' 
#' 
## -----------------------------------------

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
#' 
#' 
#' 
#' 
#' # Split Data into train and validation
#' 
## -----------------------------------------

library(caret)

library(janitor)
ratings.train.expanded<-clean_names(ratings.train.expanded)



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
#' # Create a formula
## -----------------------------------------
# Define the predictor variables and the target variable
predictors <- c("part_of_day", "is_weekend", "year", "season", "action", "drama", "thriller",
                "comedy", "adventure", "sci_fi", "imax", "fantasy", "romance", "horror",
                "children", "war", "animation", "mystery", "crime", "documentary", "western",
                "musical", "film_noir", "no_genres_listed")
# Create the formula
formula <- as.formula(paste("rating ~", paste(predictors, collapse = " + ")))

#' 
#' 
#' Let's try an xgboost model
#' 
## -----------------------------------------
library(caret)
library(xgboost)
library(doParallel)
library(foreach)

# Register the parallel backend to use
numCores <- detectCores() - 1 # Reserve one core for the system
registerDoParallel(cores=numCores)


# Convert training data to a matrix format expected by xgboost
training_matrix <- model.matrix(formula, trainingData)[, -1]
testing_matrix <- model.matrix(formula, testingData)[, -1]

# Define the trainControl method for cross-validation
train_control <- trainControl(method = "cv", # Cross-validation
                              number = 10,   # Number of folds
                              allowParallel = TRUE, # Parallel processing
                              verboseIter = TRUE, # Print training logs
                              returnData = FALSE) # Do not return training data
                              

# Define the tuning grid
# Define a tuning grid with a range of values for each hyperparameter
tuning_grid <- expand.grid(
  nrounds = c(100), # boosting rounds
  max_depth = c(2:10), #  depths
  eta = c(0.01, 0.1), # learning rates
  gamma = c( 0.1), # minimum loss reduction values
  colsample_bytree = c( 0.8), # subsample ratios of columns
  min_child_weight = c(1), #  minimum child weights
  subsample = c( 0.8) #  subsample ratios
)


# Train the model
xgb_model <- train(x = training_matrix, 
                   y = trainingData$rating,
                   method = "xgbTree", # Method for xgboost
                   trControl = train_control,
                   tuneGrid = tuning_grid,
                   metric = "RMSE")

# Print the best model
print(xgb_model)


#' 
#' 
## -----------------------------------------
varImp(xgb_model)

#' 
#' 
#' 
#' 
## -----------------------------------------
# Make predictions on the test set
predictions <- predict(xgb_model, newdata = testing_matrix)
# Adjust the predictions to the nearest 0.5
adjusted_predictions <- round(predictions * 2) / 2
# Calculate MSE
test_mse <- mean((adjusted_predictions - testingData$rating)^2)
print(paste("Test MSE:", test_mse))


#' 
#' Now we prepare the ratings.test data
#' 
## -----------------------------------------
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
## -----------------------------------------
# Convert factor variables
ratings.test.expanded$part_of_day <- factor(ratings.test.expanded$part_of_day,levels=levels(ratings.train.expanded$part_of_day))

ratings.test.expanded$season <- factor(ratings.test.expanded$season,levels=levels(ratings.train.expanded$season))
ratings.test.expanded$year <- factor(ratings.test.expanded$year,levels = levels(ratings.train.expanded$year))
ratings.test.expanded<-clean_names(ratings.test.expanded)

#' 
#' 
## -----------------------------------------

test.expanded <- model.matrix(~ part_of_day + is_weekend + year + season + action + 
    drama + thriller + comedy + adventure + sci_fi + imax + fantasy + 
    romance + horror + children + war + animation + mystery + 
    crime + documentary + western + musical + film_noir + no_genres_listed,ratings.test.expanded)[, -1]
# Make predictions on the test set
predictions <- predict(xgb_model , newdata = test.expanded )
# Adjust the predictions to the nearest 0.5
adjusted_predictions <- round(predictions * 2) / 2
adjusted_predictions

#' 
## -----------------------------------------
write.csv(adjusted_predictions, file = "film_rating_predictions_group_F_week_3.csv", row.names=FALSE)

#' 
