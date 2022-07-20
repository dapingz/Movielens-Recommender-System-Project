# "Movielens Recommendation System Project" by Daping Zhang,  June 20, 2022

# This report represents the buildings of the Movielens Recommendation System that 
# consists of the preparation of data set, the exploration and analysis of data, 
# the methods and algorithms of building models, and the evaluation and selection 
# of optimization models

# importing the libraeies
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(tidyr)) install.packages("tidyr", repos = "http://cran.us.r-project.org")
if(!require(recosystem)) install.packages("recosystem", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(lubridate)
library(kableExtra)
library(ggplot2)
library(stringr)
library(tidyr)
library(recosystem)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

# importing the Dataset
dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") 
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

# Initial exploring edx dataset and validation dataset

# The edx dataset is a subset of Movielens dataset, which is to use for the building 
# of the models. The edx dataset has 9000055 rows (observations) and 6 columns. 
# Each row represents a rating given by one user for on movie. Columns include 
# userId, movieID, rating, timestamp, title and genres.
glimpse(edx)

# check how many different users and different movies in edx dataset
edx %>% 
  summarize(n_users = n_distinct(userId),
            n_movies = n_distinct(movieId))

# check how many different users and different movies in validation dataset
validation %>% 
  summarize(n_users = n_distinct(userId),
            n_movies = n_distinct(movieId))

# see edx dataset in a table
kableExtra::kable(head(edx))

# Checking missing values

# Is there any missing values in the edx dataset and the validation dataset.
sum(is.na(edx))
sum(is.na(validation))

# Cleaning the Data

# separate multiple genres into individual genres for later analysis
edx_genre <- edx %>% separate_rows(genres, sep = "\\|") 

# extract year released from title for later analysis
edx_year_released <- edx %>% mutate(year_released = as.numeric(str_sub(title, -5, -2)))

# extract year rated from timestamp for later analysis
edx_year_rated <- edx %>% mutate(year_rated = year(as_datetime(timestamp)))

# Splitting the edx set into training set and test set

# test set will be 10% of edx data
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
train_set <- edx[-test_index,]
temp <- edx[test_index,]

# make sure userId and movieId in test set are also in training set
test_set <- temp %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

# add rows removed from test set back into train set
removed <- anti_join(temp, test_set)
train_set <- rbind(train_set, removed)

# remove some datasets that are no longer needed
rm(test_index, temp, removed)

# Data Exploration and Analysis

# Ratings

# plot number of ratings to see the different from 0.5 to 5.0
edx %>% group_by(rating) %>% summarize(count = n()) %>%
  arrange(desc(count))  %>%
  ggplot(aes(rating, count)) +
  geom_col(fill = "#666666") +
  labs(x = "Rating", y = "Count",
       title = "Number of ratings")

edx %>% filter(rating == 0) %>% tally()

# see top 10 movies by ratings in a table
kableExtra::kable(edx %>% 
                    group_by(title) %>% 
                    summarise(count = n()) %>% 
                    arrange(-count) %>%
                    top_n(10, count))

# see top 10 movies by titles on the plot
edx %>%
  group_by(title) %>%
  summarize(count = n()) %>%
  arrange(-count) %>%
  top_n(10, count) %>%
  ggplot(aes(count, reorder(title, count))) +
  geom_bar(stat = "identity") +
  labs(x="Count", y= NULL)

# Movies

# explore which movies are rated higher than others
edx %>% 
  count(movieId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() + 
  ggtitle("Movies")

# calculate the difference between ratings and the average ratings for movie i (b_i) 
mu <- mean(train_set$rating)
movie_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu)) 
movie_avgs %>% qplot(b_i, data = ., bins = 10, color = I("black"))

# Users

# make a plot to show that some users are more active than others when rating movies
edx %>% count(userId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() + 
  ggtitle("Users")

# compute the average rating for user u for those that have rated 100 or more movies:
edx %>% group_by(userId) %>%
  filter(n() >= 100) %>%
  summarise(b_u = mean(rating)) %>%
  ggplot(aes(b_u)) +
  geom_histogram(bins = 30, color = "black")

# Years

# plot the trends in user ratings of movies by year of release
edx_year_released %>% group_by(year_released) %>%
  summarise(rating = mean(rating)) %>%
  ggplot(aes(year_released, rating)) +
  geom_point() +
  geom_smooth()
  
# plot the distribution of the ratings given by year
edx_year_rated %>% mutate(year_rated = year(as_datetime(timestamp))) %>% 
    ggplot(aes(year_rated)) + 
    geom_histogram() + 
    labs(x = "Year", y = "Number of Ratings")

# Genres

# check how many genres there are and which genre has the highest rating
edx_genre_analysis <- edx_genre %>%
  group_by(genres) %>%
  summarize(count = n()) %>%
  arrange(desc(count))
edx_genre_analysis

# Building and Evaluating Models

# defining the RMSE
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# Developing the algorithms and building the models

# Model 1: Simple Model

# This model predicts the sane rating for all movies regardless of user. 

# calculate the average rating mu and the RMSE:
mu_hat <- mean(train_set$rating)
naive_rmse <- RMSE(test_set$rating, mu_hat)
naive_rmse

rmse_results <- data_frame(method = "Just the average", RMSE = naive_rmse)
rmse_results 

# Model 2: Movie Effect Model

# Consider the intrinsic character of a movie affects a movie's ratings, base on 
# the average rating for movie i, add the term b_i the model.

# Calculate every movie's average b_i
movie_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu)) 
movie_avgs %>% qplot(b_i, data = ., bins = 10, color = I("black"))
  
# generate prediction use b_i on the test set then calculate the RMSE
predicted_ratings <- mu + test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  pull(b_i)
model_2_rmse <- RMSE(predicted_ratings, test_set$rating) 

# as we going along, we will be comparing different approaches, now we start by 
# creating a results table
rmse_results <- bind_rows(rmse_results,
                          data.frame(method="Movie Effect Model",
                                      RMSE = model_2_rmse ))
rmse_results %>% knitr::kable()

  
# Model 3: Movie + User Effects Model
    
# We also consider that the user's rating of the movie also has an effect, and 
# add user effect b_u into the previous model to compute average ranking for user u.

# Calculate every user's b_u
user_avgs <- train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))
  
# generate prediction use b_i and b_u on the test set
predicted_ratings <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

# calculate the RMSE
model_3_rmse <- RMSE(predicted_ratings, test_set$rating) 
rmse_results <- bind_rows(rmse_results,
                          data.frame(method="Movie + User Effects Model",
                                      RMSE = model_3_rmse ))
rmse_results %>% knitr::kable()
  
# Model 4: Regularized Movie Effect Model
  
# Use regularization to add a penalty "lambda" to penalizes movies with large 
# estimates from a small sample size. 
  
# use cross-validation to choose a lambda:
lambdas <- seq(0, 10, 0.25)
  
mu <- mean(train_set$rating)
just_the_sum <- train_set %>% 
  group_by(movieId) %>% 
  summarize(s = sum(rating - mu), n_i = n())
  
rmses <- sapply(lambdas, function(l){
  predicted_ratings <- test_set %>% 
    left_join(just_the_sum, by='movieId') %>% 
    mutate(b_i = s/(n_i+l)) %>%
    mutate(pred = mu + b_i) %>%
    pull(pred)
  return(RMSE(predicted_ratings, test_set$rating))
})
  
qplot(lambdas, rmses)  
lambda <- lambdas[which.min(rmses)]  
 
# Using the optimized lambda to perform prediction and evaluate RMSE
mu <- mean(train_set$rating)
movie_reg_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = sum(rating - mu)/(n()+lambda), n_i = n()) 

# generate prediction on the test set
predicted_ratings <- test_set %>% 
  left_join(movie_reg_avgs, by = "movieId") %>%
  mutate(pred = mu + b_i) %>%
  pull(pred)

# calculate the RMSE
model_4_rmse <- RMSE(predicted_ratings, test_set$rating) 
rmse_results <- bind_rows(rmse_results,
                          data.frame(method="Regularized Movie Effects Model",  
                                      RMSE = model_4_rmse ))
rmse_results %>% knitr::kable()

# Model 5: Regularized Movie + User Effects Model
  
# In this model we consider the effect of movie and also consider the effect of users, 
# which is to optimize user effect b_u.

# use cross-validation to choose a lambda by considering b_i and b_u effects
lambdas <- seq(0, 10, 0.25)
rmses <- sapply(lambdas, function(l) {
  mu <- mean(train_set$rating)
    
  b_i <- train_set %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
    
  b_u <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
    
  predicted_ratings <- test_set %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
    
  return(RMSE(predicted_ratings, test_set$rating))
})
  
qplot(lambdas, rmses)  
  
lambda <- lambdas[which.min(rmses)] 
lambda
  
# use the optimized lambda to perform prediction and evaluate RMSE
b_i <- train_set %>% 
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+lambda))
  
b_u <- train_set %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+lambda))

# generate prediction on the test set
predicted_ratings <- 
  test_set %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

# calculate the RMSE
model_5_rmse <- RMSE(predicted_ratings, test_set$rating)

# creating a results table
rmse_results <- bind_rows(rmse_results,
                          data.frame(method="Regularized Movie + User Effects Model",  
                                      RMSE = model_5_rmse ))
rmse_results %>% knitr::kable()
  
# Model 6: Matrix Factorization using recosystem Model
  
# A popular technique to solve the recommender system problem is the matrix factorization. 
# We use the recosystem, Recommender System Using Parallel Matrix Factorization to build a model.

# converting data into recosystem format
set.seed(123, sample.kind="Rounding")
mf_train_set <- with(train_set, data_memory(user_index = userId, 
                                            item_index = movieId, rating = rating))
mf_test_set <- with(test_set, data_memory(user_index = userId, 
                                            item_index = movieId, rating = rating))

# create a model object by calling Reco()
r <- Reco()
  
# call the $tune() method to select best tuning parameters along a set of candidate values
mf_opts <- r$tune(mf_train_set,  opts = list(dim = c(10, 20, 30), 
                                              lrate = c(0.1, 0.2),
                                              costp_l1 = 0, costq_l1 = 0,
                                              nthread = 1, niter = 10))

# train the model by calling the $train() method
r$train(mf_train_set, opts = c(mf_opts$min, nthread = 1, niter = 30))
  
# use the $predict() method to compute predicted values
predicted_ratings <- r$predict(mf_test_set, out_memory())

# calculate the RMSE
model_6_rmse <- RMSE(predicted_ratings, test_set$rating)
  
# creating a results table
rmse_results <- bind_rows(rmse_results,
                          data.frame(method = "Matrix Factorization using recosystem Model", 
                                      RMSE = model_6_rmse)) 
rmse_results %>% knitr::kable()
  
# The Final Evaluation by using validation set
  
# converting data into recosystem format
set.seed(1234, sample.kind="Rounding")
mf_edx <- with(edx, data_memory(user_index = userId, item_index = movieId, 
                                rating = rating))
mf_validation <- with(validation, data_memory(user_index = userId, 
                                              item_index = movieId, 
                                              rating = rating))

# create a model object by calling Reco()
r <- Reco()

# call the $tune() method to select best tuning parameters along a set of candidate values
mf_opts <- r$tune(mf_edx,  opts = list(dim = c(10, 20, 30), 
                                        lrate = c(0.1, 0.2),
                                        costp_l1 = 0, costq_l1 = 0,
                                        nthread = 1, niter = 10))

# train the model by calling the $train() method
r$train(mf_edx, opts = c(mf_opts$min, nthread = 1, niter = 30))

# use the $predict() method to compute predicted values
predicted_ratings <- r$predict(mf_validation, out_memory())
  
# calculate the RMSE
model_62_rmse <- RMSE(predicted_ratings, validation$rating)
  
# creating a results table
rmse_results <- bind_rows(rmse_results,
                          data.frame(method = "Final Matrix Factorization using recosystem Model", 
                                      RMSE = model_62_rmse)) 
rmse_results %>% knitr::kable()

# Appendix

# use cross-validation to choose a lambda by considering b_i and b_u effects
lambdas <- seq(0, 10, 0.25)
rmses <- sapply(lambdas, function(l) {
  mu <- mean(edx$rating)
  
  b_i <- edx %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- edx %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  predicted_ratings <- validation %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, validation$rating))
})

qplot(lambdas, rmses)  

lambda <- lambdas[which.min(rmses)] 

# use the optimized lambda to perform prediction and evaluate RMSE
b_i <- edx %>% 
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+lambda))

b_u <- edx %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+lambda))

# generate prediction on the test set
predicted_ratings <- 
  validation %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

# calculate the RMSE
model_52_rmse <- RMSE(predicted_ratings, validation$rating)
model_52_rmse
