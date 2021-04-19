##########################################################
# MovieLens Final Project 
# Harvard EdX Professional Data Science Certificate 
# Samara Angel 
# Spring 2021
##########################################################

##########################################################
# Data Pre-Processing and Cleaning
##########################################################

#Create edx set, validation set (final hold-out test set)
# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(colorspace)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

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
set.seed(1)
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]
head(temp)

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId") 
head(validation)

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

##############################
# MOVIELENS QUIZ
##############################
#examine dataset 
head(edx)

#determine number of rows and columns
nrow(edx)
ncol(edx)
littletable <- matrix(c(nrow(edx), ncol(edx)), ncol=2, byrow=TRUE)
colnames(littletable) <- c("Number of Rows", "Number of Columns")
rbind(littletable)

#calculate the number of ratings that equal 0 and 3
sum(edx$rating == 0)
sum(edx$rating == 3)

#calculate the number of distinct movieIds and distinct userIds
nrow(as.data.frame(unique(edx$movieId)))
nrow(as.data.frame(unique(edx$userId)))
littletable_2 <- matrix(c(nrow(unique(edx$movieId)), nrow(unique(edx$userId)), ncol=2, byrow=TRUE))
colnames(littletable_2) <- c("Number of Rows", "Number of Columns")
rbind(littletable_2)

#calculate the number of occurences of each specific genre using str_detect
sum(str_detect(edx$genres,"Drama"))
sum(str_detect(edx$genres,"Comedy"))
sum(str_detect(edx$genres,"Thriller"))
sum(str_detect(edx$genres,"Romance"))

#calculate the title with the most ratings and rating given the most frequently
count(edx, title)[order(-n)]
count(edx, rating)[order(-n)]

#graph of the ratings count 
edx %>%
  group_by(rating) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = rating, y = count, color = I("aquamarine4"))) +
  geom_line()

##########################################################
# PROJECT CODE
##########################################################

###################################
#Data Visualization
###################################

#Because the validation set from above should NOT be used until a final check of the chosen RMSE model, 
#here I split the edx set into a test and train set
options(digits=7)

set.seed(1) 
test_index_edx <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
edx_train_set <- edx[-test_index_edx,]
temp <- edx[test_index_edx,]
edx_test_set <- temp %>% 
  semi_join(edx_train_set, by = "movieId") %>%
  semi_join(edx_train_set, by = "userId") #this step ensures I don’t include users and movies in the test set that do not appear in the training set
removed <- anti_join(temp, edx_test_set)
edx_train_set <- rbind(edx_train_set, removed)

rm(test_index_edx, temp, removed)
edx_train_set
edx_test_set

#Start by writing an RMSE function based on ratings and predictors 
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

#Show the head of the edx data
head(edx, 10) %>% kbl(caption = "Head of EdX Data Set") %>%
  kable_classic(full_width = F, html_font = "Cambria")%>% 
  kable_styling(latex_options = "HOLD_position")

#Show the head of the validation data 
head(validation, 10) %>% kbl(caption = "Head of Validation Data Set") %>%
  kable_classic(full_width = F, html_font = "Cambria")%>% 
  kable_styling(latex_options = "HOLD_position")

#Head of the edx_train_set and edx_test_set
head(edx_train_set, 10) %>% kbl(caption = "Head of EdX Train Data Set") %>%
  kable_classic(full_width = F, html_font = "Cambria")%>% 
  kable_styling(latex_options = "HOLD_position")

head(edx_test_set, 10) %>% kbl(caption = "Head of EdX Test Data Set") %>%
  kable_classic(full_width = F, html_font = "Cambria") %>% 
  kable_styling(latex_options = "HOLD_position")

#Number of rows and columns in the edx data 
littletable <- matrix(c(nrow(edx), ncol(edx)), ncol=2, byrow=TRUE)
colnames(littletable) <- c("Number of Rows", "Number of Columns")
littletable %>% kbl(caption = "Number of Rows and Columns in the EdX Data Set") %>%
  kable_classic(full_width = F, html_font = "Cambria") %>% 
  kable_styling(latex_options = "HOLD_position")

#plot of count vs. ratings given 
edx %>%
  group_by(rating) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = rating, y = count, color = I("aquamarine4"))) +ggtitle("Count vs. Rating Given") + 
  xlab("Rating") + 
  ylab("Count") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_line()

#calculate the number of distinct movieIds and distinct userIds
littletable_distinct <- matrix(c(nrow(as.data.frame(unique(edx$movieId))), nrow(as.data.frame(unique(edx$userId)))))
rownames(littletable_distinct) <- c("Unique MovieId", "Unique UserId")
littletable_distinct %>% kbl(caption = "Number of Distinct movieIds and userIds in the EdX Data Set") %>%
  kable_classic(full_width = F, html_font = "Cambria") %>% 
  kable_styling(latex_options = "HOLD_position")

#Show a sample of 100 ratings, with orange representing movies a user has rated
#and white representing movies that haven't been rated
users <- sample(unique(edx$userId), 100)
edx %>% filter(userId %in% users) %>% 
  select(userId, movieId, rating) %>%
  mutate(rating = 1) %>%
  spread(movieId, rating) %>% select(sample(ncol(.), 100)) %>% 
  as.matrix() %>% t(.) %>%
  image(1:100, 1:100,. ,col = hcl.colors(12, "TealGrn", rev = TRUE), xlab="Movies", ylab="Users", main = "Users vs. Movies Sample of 100")
abline(h=0:100+0.5, v=0:100+0.5, col = "grey")

######################################
#Models 
######################################

#Simplest possible recommendation system
#the estimate minimizing RMSE is the least squares estimate (avg of all ratings)
mu_hat <- mean(edx_train_set$rating)
mu_hat

#predict all unknown ratings with mu_hat gives the naive_rmse
naive_rmse <- RMSE(edx_test_set$rating, mu_hat)
naive_rmse

#results table of the naive approach 
rmse_results <- tibble(method = "Avg Naive Model", RMSE = naive_rmse)
rmse_results %>% knitr::kable()

#each movie has a different avg rating, b_i, the bias by movie. Because of the size of the data set 
#we will estimate b_i using the least squares estimate b_i_hat
mu <- mean(edx_train_set$rating) 
mu
movie_avgs <- edx_train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i_hat = mean(rating - mu))

#show variability in these estimates graphically 
qplot(b_i_hat, data = movie_avgs, bins = 30, color = I("blue"), fill = I("light blue"))

#show improved RMSE using mu + b_i_hat
predicted_ratings <- mu + edx_test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  pull(b_i_hat)
avg_movie_rmse <- RMSE(predicted_ratings, edx_test_set$rating)

#updating rmse_results
rmse_results <- rmse_results %>% add_row(method = "Avg Movie Rating Model", RMSE = avg_movie_rmse)
rmse_results %>% knitr::kable()

#factor in user averages to improve RMSE 

#start by computing average rating for user u and visualizing it graphically
edx_train_set %>% 
  group_by(userId) %>% 
  filter(n()>=100) %>%
  summarize(b_u_hat = mean(rating)) %>% 
  ggplot(aes(b_u_hat)) + 
  geom_histogram(bins = 30, color = "blue", fill = "light blue")

#each user has a different avg rating, b_u, the bias by user. Because of the size of the data set 
#we will estimate b_u using the least squares estimate b_u_hat
user_avgs <- edx_train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u_hat = mean(rating - mu - b_i_hat))

#check RMSE 
predicted_ratings <- edx_test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i_hat + b_u_hat) %>%
  pull(pred)
avg_movie_user_rmse<- RMSE(edx_test_set$rating, predicted_ratings)

#updating rmse_results
rmse_results <- rmse_results %>% add_row(method = "Avg Movie + User Rating Model", RMSE = avg_movie_user_rmse)
rmse_results %>% knitr::kable()

###############################
#Incorporating Other Biases 
###############################

#start by computing average rating for genre g and visualizing it graphically in histogram format
edx_train_set %>% 
  group_by(genres) %>% 
  filter(n()>=100) %>% 
  summarize(b_g_hat = mean(rating)) %>% 
  ggplot(aes(b_g_hat)) + 
  geom_histogram(bins = 30, color = "blue", fill = "light blue")

#each genre combo has a different avg rating, b_g, the bias by genre. Because of the size of the data set 
#we will estimate b_g using the least squares estimate b_g_hat
genres_avgs <- edx_train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  group_by(genres) %>%
  summarize(b_g_hat = mean(rating - mu - b_i_hat - b_u_hat))

#check RMSE 
predicted_ratings <- edx_test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genres_avgs, by='genres') %>%
  mutate(pred = mu + b_i_hat + b_u_hat + b_g_hat) %>%
  pull(pred)
avg_movie_user_genres_rmse<- RMSE(edx_test_set$rating, predicted_ratings)

#updating rmse_results
rmse_results <- rmse_results %>% add_row(method = "Avg Movie + User + Genres Rating Model", RMSE = avg_movie_user_genres_rmse)
rmse_results %>% knitr::kable()

#Here, I will separate genre instead of using the combination of the cross-listed genres. For example, 
#Action|Adventure is separated into Action and Adventure. I then look at Action as a separate category from Adventure. 

sep_by_genre <- edx %>% separate_rows(genres, sep = "\\|")
genres <- sep_by_genre %>% group_by(genres)

sep_by_genre_train <- edx_train_set %>% separate_rows(genres, sep = "\\|")
sep_by_genre_test <- edx_test_set %>% separate_rows(genres, sep = "\\|")

grouped_sep_train <- sep_by_genre_train %>% group_by(genres)
grouped_sep_test <- sep_by_genre_test %>% group_by(genres)

grouped_sep_train %>% 
  summarize(count = n()) %>%
  arrange(desc(count))

grouped_sep_test %>% 
  summarize(count = n()) %>%
  arrange(desc(count))

#start by computing average rating for each separated genres and visualizing them graphically
genres %>% 
  summarize(b_g_hat = mean(rating)) %>% 
  ggplot(aes(genres, b_g_hat, color = genres)) + geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#First, I will begin by calculating the average for each individual genre, k.
#each genre combo has a different avg rating, b_g, the bias by genre. Because of the size of the data set 
#we will estimate b_g using the least squares estimate b_g_hat
genres_avgs <- sep_by_genre_train %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  group_by(genres) %>%
  summarize(b_g_hat = mean(rating - mu - b_i_hat - b_u_hat))

#check RMSE 
predicted_ratings <- sep_by_genre_test %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genres_avgs, by='genres') %>%
  mutate(pred = mu + b_i_hat + b_u_hat + b_g_hat) %>%
  pull(pred)
avg_movie_user_sepgenres_rmse<- RMSE(sep_by_genre_test$rating, predicted_ratings)

#updating rmse_results
rmse_results <- rmse_results %>% add_row(method = "Avg Movie + User + Separated Genres Rating Model", RMSE = avg_movie_user_sepgenres_rmse)
rmse_results %>% knitr::kable()

####################################
#Regularization
####################################

#beginning the regularization process
#10 largest mistakes
edx_test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  mutate(residual = rating - (mu + b_i_hat)) %>%
  arrange(desc(abs(residual))) %>%  
  slice(1:10) %>% 
  pull(title)

#finding distinct titles of movies in the data set 
movie_titles <- edx %>% 
  select(movieId, title) %>%
  distinct()

#10 best movies from my estimate
movie_avgs %>% left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i_hat)) %>% 
  slice(1:10)  %>% 
  pull(title)

#10 worst movies from my estimate
movie_avgs %>% left_join(movie_titles, by="movieId") %>%
  arrange(b_i_hat) %>% 
  slice(1:10)  %>% 
  pull(title)

#how often are the 10 best movies rated? 
edx_train_set %>% count(movieId) %>% 
  left_join(movie_avgs, by="movieId") %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i_hat)) %>% 
  slice(1:10) %>% 
  pull(n)

#how often are the 10 worst movies rated? 
edx_train_set %>% count(movieId) %>% 
  left_join(movie_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(b_i_hat) %>% 
  slice(1:10) %>% 
  pull(n)

#choose the optimal lambda by cross-validation for movie method
lambdas <- seq(0, 10, 0.25)

mu <- mean(edx_train_set$rating)
just_the_sum <- edx_train_set %>% 
  group_by(movieId) %>% 
  summarize(s = sum(rating - mu), n_i = n())

rmses <- sapply(lambdas, function(l){
  predicted_ratings <- edx_test_set %>% 
    left_join(just_the_sum, by='movieId') %>% 
    mutate(b_i_hat = s/(n_i+l)) %>%
    mutate(pred = mu + b_i_hat) %>%
    pull(pred)
  return(RMSE(predicted_ratings, edx_test_set$rating))
})
qplot(lambdas, rmses)  
lambda <- lambdas[which.min(rmses)]

#inputting minimum lamda to calculate rmse
mu <- mean(train_set$rating)
regularized_movie <- edx_train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i_hat = sum(rating - mu)/(n()+lambda), n_i = n()) 

predicted_ratings <- edx_test_set %>% 
  left_join(regularized_movie, by = "movieId") %>%
  mutate(pred = mu + b_i_hat) %>%
  pull(pred)

regularized_movie_rmse <- RMSE(predicted_ratings, edx_test_set$rating)

#updating rmse_results
rmse_results <- rmse_results %>% add_row(method = "Regularized Movie Rating Model", RMSE = regularized_movie_rmse)
rmse_results %>% knitr::kable()

#plot of regularized vs. least squares estimates
tibble(original = movie_avgs$b_i_hat, 
       regularlized = movie_reg_avgs$b_i_hat, 
       n = movie_reg_avgs$n_i) %>%
  ggplot(aes(original, regularlized, size=sqrt(n))) + 
  geom_point(shape=1, alpha=0.5)

#top 10 movie estimates with regularization 
edx_train_set %>%
  count(movieId) %>% 
  left_join(movie_reg_avgs, by = "movieId") %>%
  left_join(movie_titles, by = "movieId") %>%
  arrange(desc(b_i_hat)) %>% 
  slice(1:10) %>% 
  pull(title)

#bottom 10 movie estimates with regularization 
edx_train_set %>%
  count(movieId) %>% 
  left_join(movie_reg_avgs, by = "movieId") %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(b_i_hat) %>% 
  select(title, b_i_hat, n) %>% 
  slice(1:10) %>% 
  pull(title)

#choose the optimal lambda by cross-validation for movie and user method
lambdas <- seq(0, 10, 0.25)

rmses <- sapply(lambdas, function(l){
  
  mu <- mean(edx_train_set$rating)
  
  b_i_hat <- edx_train_set %>% 
    group_by(movieId) %>%
    summarize(b_i_hat = sum(rating - mu)/(n()+l))
  
  b_u_hat <- edx_train_set %>% 
    left_join(b_i_hat, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u_hat = sum(rating - b_i_hat - mu)/(n()+l))
  
  predicted_ratings <- 
    edx_test_set %>% 
    left_join(b_i_hat, by = "movieId") %>%
    left_join(b_u_hat, by = "userId") %>%
    mutate(pred = mu + b_i_hat + b_u_hat) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, edx_test_set$rating))
})
qplot(lambdas, rmses)  
lambda <- lambdas[which.min(rmses)]

#inputting minimum lamda to calculate rmse
mu <- mean(train_set$rating)

b_i_hat <- edx_train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i_hat = sum(rating - mu)/(n()+lambda), n_i = n()) 

b_u_hat <- edx_train_set %>% 
  left_join(b_i_hat, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u_hat = sum(rating - b_i_hat - mu)/(n()+lambda), n_i = n())

predicted_ratings <- 
  edx_test_set %>% 
  left_join(b_i_hat, by = "movieId") %>%
  left_join(b_u_hat, by = "userId") %>%
  mutate(pred = mu + b_i_hat + b_u_hat) %>%
  pull(pred)

regularized_movie_user_rmse <- RMSE(predicted_ratings, edx_test_set$rating)

options(pillar.sigfig = 7)

#updating rmse_results
rmse_results <- rmse_results %>% add_row(method = "Regularized Movie + User Rating Model", RMSE = regularized_movie_user_rmse)
rmse_results %>% knitr::kable()

#Success with Regularized Movie + User Rating Model 

###############################################
#Final Model Using the Validation Hold-Out Set
###############################################

#Because the Avg Movie + User + Separated Genres Rating Model had the lowest RMSE of 0.8627, I will now use that model with the validation set. 
#Here, I will separate genre instead of using the combination of the cross-listed genres, this time with the edx and  validation sets. For example, 
#Action|Adventure is separated into Action and Adventure. I then look at Action as a separate category from Adventure. 

sep_by_genre_edx <- edx %>% separate_rows(genres, sep = "\\|")
sep_by_genre_validation <- validation %>% separate_rows(genres, sep = "\\|")
genres_2 <- sep_by_genre_edx %>% group_by(genres)

grouped_sep_edx <- sep_by_genre_edx %>% group_by(genres)
grouped_sep_validation <- sep_by_genre_validation %>% group_by(genres)

grouped_sep_edx %>% 
  summarize(count = n()) %>%
  arrange(desc(count))

grouped_sep_validation %>% 
  summarize(count = n()) %>%
  arrange(desc(count))

#start by computing average rating for each separated genres and visualizing them graphically
genres_2 %>% 
  summarize(b_g_hat = mean(rating)) %>% 
  ggplot(aes(genres, b_g_hat, color = genres)) + geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#I will calculate the average for each individual genre, k.
#each genre combo has a different avg rating, b_g, the bias by genre. Because of the size of the data set 
#we will estimate b_g using the least squares estimate b_g_hat
genres_avgs_final <- sep_by_genre_edx %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  group_by(genres_2) %>%
  summarize(b_g_hat = mean(rating - mu - b_i_hat - b_u_hat))

#check RMSE 
predicted_ratings <- sep_by_genre_validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genres_avgs, by='genres') %>%
  mutate(pred = mu + b_i_hat + b_u_hat + b_g_hat) %>%
  pull(pred)
final_validation_avg_movie_user_sepgenres_rmse<- RMSE(sep_by_genre_validation$rating, predicted_ratings)

#updating rmse_results with Final Validation Set
rmse_results <- rmse_results %>% add_row(method = "Final Validation Avg Movie + User + Separated Genres Rating Model", RMSE = final_validation_avg_movie_user_sepgenres_rmse)
rmse_results %>% knitr::kable()

#This final RMSE of 0.8639 is lower than the target RMSE of 0.8649. This was therefore successful!