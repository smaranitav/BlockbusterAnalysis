#We will convert IMDB score into movie rating
convert<-function(x)
{
  if(x<2.5)
    return("Flop")
  else if(x<5)
    return("Watchable")
  else if(x<7.5)
    return("Good")
  
  else
    return("Amazing")
}

View(IMDB)

movie_rating<-sapply(IMDB$imdb_score,convert)
movie_rating

IMDB<-cbind(IMDB,movie_rating)
View(IMDB)
#Here we split data into training, validation and test sets with the ratio of 6:2:2
set.seed(45)
train.index <- sample(row.names(IMDB), dim(IMDB)[1]*0.6)
valid.index <- sample(setdiff(row.names(IMDB), train.index), dim(IMDB)[1]*0.2)


test.index <- setdiff(row.names(IMDB), union(train.index, valid.index))
train <- IMDB[train.index, ]
valid <- IMDB[valid.index, ]
test <- IMDB[test.index, ]
# Logistic Regression
library(nnet)
# Fitting Multinomial Logistic Regression to the Training set
classifier = multinom(formula = movie_rating ~ num_critic_for_reviews + num_voted_users
                      + cast_total_facebook_likes + facenumber_in_poster+ num_user_for_reviews+ budget+
                        movie_facebook_likes ,data=train)

summary(classifier)
test

probability<-predict(classifier,test,"probs")
View(probability)

predicted_rating<-predict(classifier,test)
predicted_rating

imdb_predicted<-cbind(test,predicted_rating)
View(imdb_predicted)

library(caret)
#Now we will check the accuracy
confusionMatrix(imdb_predicted$predicted_rating,imdb_predicted$movie_rating)