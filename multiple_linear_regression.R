findComp1<-function(x) {
  return (x-xbar)
}
findComp2<-function(y) {
  return (y-ybar)
}
findComp3<-function(x) {
  return ((x-xbar)*(x-xbar))
}
meanimdb<-mean(train$imdb_score)
meancritics<-mean(train$num_critic_for_reviews)
meanusers<-mean(train$num_voted_users)
meanfblikescast<-mean(train$cast_total_facebook_likes)
meanposterfaces<-mean(train$facenumber_in_poster)
meanuserreviews<-mean(train$num_user_for_reviews)
meanbudget<-mean(train$budget)
meanmovielikes<-mean(train$movie_facebook_likes)
ybar<-meanimdb
#number of critics for reviews

xbar<-meancritics
num1<-sapply(train$num_critic_for_reviews,findComp1)
num2<-sapply(train$imdb_score,findComp2)
num3<-num1*num2
numerator<-sum(num3)
den<-sapply(train$num_critic_for_reviews, findComp3)
deno<-sum(den)
b1<-numerator/deno
b1
#number of voted users
xbar<-meanusers
#to find xi-xbar
num1<-sapply(train$num_voted_users,findComp1)
num1
#(xi-xbar)(yi-ybar)
num3<-num1*num2
num3
#summation of (xi-xbar)(yi-ybar)
numerator<-sum(num3)
numerator


den<-sapply(train$num_voted_users, findComp3)
den
deno<-sum(den)
deno
b2<-numerator/deno
b2
#number of facebook likes for cast
xbar<-meanfblikescast
num1<-sapply(train$cast_total_facebook_likes,findComp1)
num3<-num1*num2
numerator<-sum(num3)
den<-sapply(train$cast_total_facebook_likes, findComp3)
deno<-sum(den)
b3<-numerator/deno
b3
#number of faces in the poster
xbar<-meanposterfaces
num1<-sapply(train$facenumber_in_poster,findComp1)
num3<-num1*num2
numerator<-sum(num3)


den<-sapply(train$facenumber_in_poster, findComp3)
deno<-sum(den)
b4<-numerator/deno
b4
#number of user reviews
xbar<-meanuserreviews
num1<-sapply(train$num_user_for_reviews,findComp1)
num3<-num1*num2
numerator<-sum(num3)
den<-sapply(train$num_user_for_reviews, findComp3)
deno<-sum(den)
b5<-numerator/deno
b5
#budget spent on the movie
xbar<-meanbudget
num1<-sapply(train$budget,findComp1)
num3<-num1*num2
numerator<-sum(num3)
den<-sapply(train$budget, findComp3)
deno<-sum(den)



b6<-numerator/deno
b6
#number of likes on fb of the movie
xbar<-meanmovielikes
#to find xi-xbar
num1<-sapply(train$movie_facebook_likes,findComp1)
#(xi-xbar)(yi-ybar)
num3<-num1*num2
#summation of (xi-xbar)(yi-ybar)
numerator<-sum(num3)
den<-sapply(train$movie_facebook_likes,findComp3)
deno<-sum(den)
b7<-numerator/deno
#prediction
b0<-ybar-(b1*meancritics)-(b2*meanusers)-(b3*meanfblikescast)-(b4*meanposterfaces)-
  (b5*meanuserreviews)-(b6*meanbudget)-(b7*meanmovielikes)
#predicting imdb score
critics<-392
users<-471220
fblikescast<-48350
posterfaces<-0



userreviews<-1238
budget<-300000000
movielikes<-0
ycap1<-
  b0+(b1*critics)+(b2*users)+(b3*fblikescast)+(b4*posterfaces)+(b5*userreviews)+(b6*budge
                                                                                 t)+(b7*movielikes)
if(ycap1>7.50){
  print("Amazing")
}else if (ycap1>5.00){
  print("Good")
}else if(ycap1>2.5){
  print("Watchable")
}else{
  print("Flop")
}
#built-in function
model <- lm(imdb_score ~ num_critic_for_reviews + num_voted_users +
              cast_total_facebook_likes + facenumber_in_poster+ num_user_for_reviews+ budget+
              movie_facebook_likes, data = train)
summary(model)
predicted<-predict(model,test)
IMDB_predicted<-cbind(test,predicted)

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
predicted_movie_rating<-sapply(IMDB_predicted$predicted,convert)
predicted_movie_rating
movie_rating<-sapply(IMDB_predicted$imdb_score,convert)
movie_rating

IMDB<-cbind(IMDB_predicted,predicted_movie_rating,movie_rating)
View(IMDB)

sub<-subset(IMDB,select=c(1,3,4,5,6,7,8,9,2,10,11,12))
View(sub)
library(caret)
#Now we will check the accuracy
confusionMatrix(IMDB$predicted_movie_rating,IMDB$movie_rating)