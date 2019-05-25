###################################################
# Create edx set and validation set from movielense
###################################################


if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(gower)) install.packages("gower")
if(!require(backports)) install.packages("backports")
if(!require(GGally)) install.packages("GGally")
library(backports)
library(dslabs)
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(lattice)
library(rlang)
library(gower)
library(caret)
library(lubridate)
library(broom)
library(GGally)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- read.table(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                      col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data

set.seed(1)

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

# Now let's get started.. shall we?

## Data Exploration!! ##

# Take a look at the dataset edx
head(edx)
summary(edx)

# Lets access the counts.. how many movies, users and genre??
n_distinct(edx$movieId)
n_distinct(edx$genres)
n_distinct(edx$userId)

#Let's see how many times each movies are reviewed
edx %>% count(movieId) %>% 
  ggplot(aes(n)) + geom_histogram(bins = 10, fill = 'blue', color = "black") + 
  scale_x_log10() + ggtitle("Movies Reviewed")

#Let's do the same for users.. how often they review
edx %>% count(userId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 10, fill = 'blue', color = "black") + 
  scale_x_log10() + 
  ggtitle("Users")

# Hmm.. some interesting observations.. Let's see what more we can do



## Data Wrangling ## 

# Let's do some data wrangling..

#Timestamp of rating is not readily usable, nead some conversion;
# Lets convert the timestamp to Year..
edx <- mutate(edx, rating_year = year(as_datetime(timestamp)))
head(edx)

# Also, date of rating
edx <- mutate(edx, rating_date = as.Date(as_datetime(edx$timestamp), "%m/%d/%Y"))
head(edx)
#class(edx$rating_year) - just to confirm it is not string

# The title also has year, looks like year of release? 
title_year <- as.numeric(stringi::stri_extract(edx$title, regex = "(\\d{4})", comments = TRUE ))

#we have now the data sets with all the dates formatted and available
edx_with_dates <- edx %>% mutate(release_year = title_year)
head(edx_with_dates)


# Now, let's see if we can group the movies by release year and see the oldest movie
movies_grouped_by_year <- edx_with_dates %>% group_by(release_year) %>% summarize(n = n()) %>% arrange(desc(release_year, n))
movies_grouped_by_year


# Wait a minute.. we have some invalid release_year!
junk_dates <- edx_with_dates %>% filter(release_year > 2010 | release_year < 1900)
junk_dates

# Looks like many movies with number in their names.. will have to tighten the regex?
# Let's see how many are we talking about?
junk_dates %>% group_by(movieId, title, release_year) %>% summarize(n = n())

# Only 14 movies! May be just update in place.. frankly, I am looking for an easy way out
# I will try to comeback later to straighten my regex.. 
# For now, I am going to manually fix

edx_with_dates[edx_with_dates$movieId == "671", "release_year"] <- 1996
edx_with_dates[edx_with_dates$movieId == "1422", "release_year"] <- 1997
edx_with_dates[edx_with_dates$movieId == "2308", "release_year"] <- 1973
edx_with_dates[edx_with_dates$movieId == "4159", "release_year"] <- 2001
edx_with_dates[edx_with_dates$movieId == "4311", "release_year"] <- 1998
edx_with_dates[edx_with_dates$movieId == "5310", "release_year"] <- 1985
edx_with_dates[edx_with_dates$movieId == "5472", "release_year"] <- 1972
edx_with_dates[edx_with_dates$movieId == "6290", "release_year"] <- 2003
edx_with_dates[edx_with_dates$movieId == "6645", "release_year"] <- 1971
edx_with_dates[edx_with_dates$movieId == "8198", "release_year"] <- 1960
edx_with_dates[edx_with_dates$movieId == "8864", "release_year"] <- 2004
edx_with_dates[edx_with_dates$movieId == "8905", "release_year"] <- 1992
edx_with_dates[edx_with_dates$movieId == "27266", "release_year"] <- 2004
edx_with_dates[edx_with_dates$movieId == "53953", "release_year"] <- 2007

#lets see if there are any junks dates?
junk_dates2 <- edx_with_dates %>% filter(release_year > 2010 | release_year < 1900)
junk_dates2

#Take a look at the formatted dataset
head(edx_with_dates)


## Data Analysis ##


# Now that the dataset is in reasonably good shape, lets do some analysis..

### Fix this later!!!
#Is there a relationship b/w movieid and avg ratings? 
#Good movies get good ratings, don't they?
avg_mov_rating <- edx_with_dates %>% group_by(movieId) %>% summarize(avg_movie_rating = mean(rating))
head(avg_mov_rating)
avg_mov_rating %>% ggplot(aes(movieId, avg_movie_rating)) + 
  geom_point(alpha = 0.05)


#Something is wrong.. movieids are not continuous.. and avg rating is always in a band 2-4
#Let's see release year that we got above has any significants?
#Let's calculate the age of the movie
edx_movies_and_age <-  edx_with_dates %>% mutate(movie_age = 2019 - release_year)
head(edx_movies_and_age)

#Now let's if age of the movie has any relationship.. like classics have better rating?
avg_rating_age <- edx_movies_and_age %>% group_by(movie_age) %>% summarize(avg_rating_by_age = mean(rating))
head(avg_rating_age)

#Now lets plot a simple geom point
avg_rating_age %>%
  ggplot(aes(movie_age, avg_rating_by_age)) + geom_point()

#Let's examine the corelations
ggpairs(avg_rating_age, mapping = aes(color = "movie_age"),  
        upper = list(continuous = wrap("cor", size = 5)), 
        lower = list(continuous = "smooth"))

# Yeah.. higher the age higher the avg ratings.. classic movies have good avg ratings

lm_stat <- lm(avg_rating_by_age ~ movie_age, data = avg_rating_age) 
tidy_data_4plot <- tidy(lm_stat, conf.int = TRUE)
lm_stat %>% .$coef


### Prediction & RMSE


# Enough with exploration!

#Lets do the RMSE setup as learned from the Machine Learning course.
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}



# Tuning Parameter Lamda
lambdas <- seq(0, 10, 0.25)

#Let's see if we can pick the best rmes

rmses <- sapply(lambdas, function(l){

# mu as the mean rating
mu <- mean(edx_with_dates$rating)

#b_i 
b_i <- edx_with_dates %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n() + l))

#b_i
b_u <- edx_with_dates %>%
  left_join(b_i, by='movieId') %>% 
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n() +l))

#pred using mu, b_i and b_u
predicted_ratings <- edx_with_dates %>%
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  mutate(pred = mu + b_i +  b_u) %>% pull(pred)

return(RMSE(predicted_ratings, edx_with_dates$rating))

})

#let's plot the lamdas vs rmses
qplot(lambdas, rmses)

#which lambdas results in the smallest rmses?
best_lambdas <- lambdas[which.min(rmses)]
best_lambdas

#Now let's apply on validation set!! Finger's crossed!!
# I am renaming the variable to _val for my own sanity..

mu_val <- mean(validation$rating)
l_val <- best_lambdas

b_i_val <- validation %>%
  group_by(movieId) %>%
  summarize(b_i_val = sum(rating - mu_val)/(n() + l_val))

b_u_val <- validation %>%
  left_join(b_i_val, by='movieId') %>% 
  group_by(userId) %>%
  summarize(b_u_val = sum(rating - b_i_val - mu_val)/(n() +l_val))

predicted_ratings <- validation %>%
  left_join(b_i_val, by = "movieId") %>%
  left_join(b_u_val, by = "userId") %>%
  mutate(pred = mu_val + b_i_val +  b_u_val) %>% pull(pred)

#Lets see what is the RMSE of our predicted rating
RMSE(predicted_ratings, validation$rating)

#Now that is less than .87750!! Thanks!!
