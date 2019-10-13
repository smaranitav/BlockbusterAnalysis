library(ggplot2) # visualization
library(ggrepel)
library(ggthemes) # visualization
library(scales) # visualization
library(dplyr) # data manipulation
library(data.table)
library(formattable)
library(plotly)
library(corrplot)
library(caret)
#DATA PREPROCESSING
IMDB <- read.csv("movie_metadata.csv",header = TRUE,sep=",")
str(IMDB)
# delete duplicate rows
IMDB <- IMDB[!duplicated(IMDB), ]
IMDB <- IMDB[!is.na(IMDB$gross), ]
IMDB <- IMDB[!is.na(IMDB$budget), ]
# replace NA with column average for facenumber_in_poster

9

IMDB$facenumber_in_poster[is.na(IMDB$facenumber_in_poster)] <-
  round(mean(IMDB$facenumber_in_poster, na.rm = TRUE))
# convert 0s into NAs for other predictors
IMDB[,c(5,6,8,13,24,26)][IMDB[,c(5,6,8,13,24,26)] == 0] <- NA
# impute missing value with column mean
IMDB$num_critic_for_reviews[is.na(IMDB$num_critic_for_reviews)] <-
  round(mean(IMDB$num_critic_for_reviews, na.rm = TRUE))
IMDB$cast_total_facebook_likes[is.na(IMDB$cast_total_facebook_likes)] <-
  round(mean(IMDB$cast_total_facebook_likes, na.rm = TRUE))
IMDB$movie_facebook_likes[is.na(IMDB$movie_facebook_likes)] <-
  round(mean(IMDB$movie_facebook_likes, na.rm = TRUE))
#Select only required attributes
IMDB <- subset(IMDB, select
               =c(gross,imdb_score,num_critic_for_reviews,num_voted_users,cast_total_facebook_likes,facenumber_in_poster,num_user_for_reviews,budget,movie_facebook_likes))
#Here we split data into training, validation and test sets with the ratio of 6:2:2
set.seed(45)
train.index <- sample(row.names(IMDB), dim(IMDB)[1]*0.6)
valid.index <- sample(setdiff(row.names(IMDB), train.index), dim(IMDB)[1]*0.2)
test.index <- setdiff(row.names(IMDB), union(train.index, valid.index))
train <- IMDB[train.index, ]
valid <- IMDB[valid.index, ]
test <- IMDB[test.index, ]