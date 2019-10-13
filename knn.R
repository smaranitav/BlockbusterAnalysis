IMDB$imdb<-c('A')
for(m in IMDB$imdb_score){
  if(m<2.5)
    IMDB$imdb[i]<-'Flop'
  else if(m<5)
    IMDB$imdb[i]<-'Watchable'
  else if(m<7.5)
    IMDB$imdb[i]<-'Good'
  else
    IMDB$imdb[i]<-'Amazing'
  i<-i+1
}

#Here we split data into training, validation and test sets with the ratio of 7:3
set.seed(45)
train.index <- sample(row.names(IMDB), dim(IMDB)[1]*0.7)


test.index <- setdiff(row.names(IMDB),train.index)
train <- IMDB[train.index, ]
test <- IMDB[test.index, ]
dim(IMDB)
dim(train)
dim(test)

train1<-train
train1$dist<-c(0)
test$res<-c('GG')

#budget and num_critic_for_reviews
for(j in 1:nrow(train1))
{
  train1$dist[j]=sqrt((test$budget[1]-train$budget[j])^2+(test$num_critic_for_reviews[1]-
                                                            train$num_critic_for_reviews[j])^2)
}
train1=train1[with(train1, order(train1$dist,train1$imdb)),]

#budget and num_user_for_reviews
for(j in 1:nrow(train1))
{
  train1$dist[j]=sqrt((test$budget[1]-train$budget[j])^2+(test$num_user_for_reviews[1]-
                                                            train$num_user_for_reviews[j])^2)
}
train1=train1[with(train1, order(train1$dist,train1$imdb)),]


#budget and movie_facebook_likes
for(j in 1:nrow(train1))
{
  train1$dist[j]=sqrt((test$budget[1]-train$budget[j])^2+(test$movie_facebook_likes[1]-
                                                            train$movie_facebook_likes[j])^2)
}
train1=train1[with(train1, order(train1$dist,train1$imdb)),]

#budget and num_voted_users
for(j in 1:nrow(train1))
{
  train1$dist[j]=sqrt((test$budget[1]-train$budget[j])^2+(test$num_voted_users[1]-
                                                            train$num_voted_users[j])^2)
}
train1=train1[with(train1, order(train1$dist,train1$imdb)),]

flop<-0
watchable<-0
good<-0
amazing<-0

i<-1
for(i in 1:3)
{
  if(train1$imdb[i]=="Flop")
    
    
  
  flop=flop+1
  else if(train1$imdb[i]=="Watchable")
    watchable=watchable+1
  else if(train1$imdb[i]=="Good")
    good=good+1
  else if(train1$imdb[i]=="Amazing")
    amazing=amazing+1
}
flop
watchable
good
amazing

res<-c("flop","watchable","good","amazing")[which.max(c(flop,watchable,good,amazing))]
res