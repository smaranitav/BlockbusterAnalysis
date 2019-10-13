success=0
predicted=0
IMDB$success=success
IMDB$predicted=predicted
IMDB$predicted=sapply(IMDB$predicted, as.character)
library("data.table")
for(i in 1:nrow(IMDB)){
  v=IMDB[i,2]
  if(v<3.9){
    IMDB[i,10]="FLOP"
  }
  else if(v>3.9 & v<6.9)
  {
    IMDB[i,10]="WATCHABLE"
  }
  else if(v>=6.9 & v<8.2)
  {
    IMDB[i,10]="GOOD"
  }
  else
  {
    IMDB[i,10]="AMAZING"
  }
}
train <- function (dt) {
  return(list(meanGross=mean(dt$gross), varGross=sd(dt$gross),
              meanImbd=mean(dt$imdb_score), varImdb=sd(dt$imdb_score),
              meannum_critic=mean(dt$num_critic_for_reviews),
              varnum_critic=sd(dt$num_critic_for_reviews),
              
              
              
              meanNum_voted=mean(dt$num_voted_users),varNum_voted=sd(dt$num_voted_users),
              meanTotalLikes=mean(dt$cast_total_facebook_likes),varTotalLikes=sd(dt$cast_total_facebo
                                                                                 ok_likes),
              meanBudget=mean(dt$budget),varBudget=sd(dt$budget),
              meanFaceNo=mean(dt$facenumber_in_poster),
              varFaceNo=sd(dt$facenumber_in_poster),
              meanUserReview=mean(dt$num_user_for_reviews),
              varUserReview=sd(dt$num_user_for_reviews),
              
              meanfblikes=mean(dt$movie_facebook_likes),varfblikes=sd(dt$movie_facebook_likes)
  ))
}

library("data.table")

IMDB<-data.table(IMDB)
class_flop <- IMDB[which(IMDB$success == 'FLOP'),]
class_watchable <- IMDB[which(IMDB$success == 'WATCHABLE'),]
class_good<-IMDB[which(IMDB$success == 'GOOD'),]
class_amazing<-IMDB[which(IMDB$success == 'AMAZING'),]
classifier1 <- train(class_flop)
classifier2 <- train(class_watchable)
classifier3 <- train(class_good)
classifier4 <- train(class_amazing)

#p_height <- dnorm(sample$gross, classifier1$meanGross, classifier1$varGross)

for(i in 1:nrow(IMDB))
{
  
  sample=data.table(gross=IMDB[i]$gross,Imdb=IMDB[i]$imdb_score,critic=IMDB[i]$num_
                    critic_for_reviews,voted=IMDB[i]$num_voted_users,faceNo=IMDB[i]$facenumber_in_post
                    er,user_reviews=IMDB[i]$num_user_for_reviews,likes=IMDB[i]$cast_total_facebook_likes,
                    Budget=IMDB[i]$budget,movie_likes=IMDB[i]$movie_facebook_likes)
  
  p_fblikes<-dnorm(sample$movie_likes,classifier1$meanfblikes,classifier1$varfblikes)
  p_weight <- dnorm(sample$Imdb, classifier1$meanImbd, classifier1$varImdb)
  p_footsize <- dnorm(sample$critic, classifier1$meannum_critic, classifier1$varnum_critic)
  p_voted<-dnorm(sample$voted,classifier1$meanNum_voted,classifier1$varNum_voted)
  p_likes<-dnorm(sample$likes,classifier1$meanTotalLikes,classifier1$varTotalLikes)
  p_budget<-dnorm(sample$Budget,classifier1$meanBudget,classifier1$varBudget)
  p_face <- dnorm(sample$faceNo, classifier1$meanFaceNo, classifier1$varFaceNo)
  p_reviews <- dnorm(sample$user_reviews, classifier1$meanUserReview,
                     classifier1$varUserReview)
  prior_flop1 <-nrow(class_flop)/nrow(IMDB)
  r1=p_fblikes*p_face*p_reviews*prior_flop1 * p_weight *
    p_footsize*p_voted*p_likes*p_budget
  
  p_fblikes<-dnorm(sample$movie_likes,classifier2$meanfblikes,classifier2$varfblikes)
  p_weight <- dnorm(sample$Imdb, classifier2$meanImbd, classifier2$varImdb)
  p_footsize <- dnorm(sample$critic, classifier2$meannum_critic, classifier2$varnum_critic)
  p_voted<-dnorm(sample$voted,classifier2$meanNum_voted,classifier2$varNum_voted)
  p_likes<-dnorm(sample$likes,classifier2$meanTotalLikes,classifier2$varTotalLikes)
  p_budget<-dnorm(sample$Budget,classifier2$meanBudget,classifier2$varBudget)
  p_face <- dnorm(sample$faceNo, classifier2$meanFaceNo, classifier2$varFaceNo)
  p_reviews <- dnorm(sample$user_reviews, classifier2$meanUserReview,
                     classifier2$varUserReview)
  prior_flop1 <-nrow(class_watchable)/nrow(IMDB)
  r2=p_fblikes*p_face*p_reviews*prior_flop1 * p_weight *
    p_footsize*p_voted*p_likes*p_budget
  
  22
  
  p_fblikes<-dnorm(sample$movie_likes,classifier3$meanfblikes,classifier3$varfblikes)
  p_weight <- dnorm(sample$Imdb, classifier3$meanImbd, classifier3$varImdb)
  p_footsize <- dnorm(sample$critic, classifier3$meannum_critic, classifier3$varnum_critic)
  p_voted<-dnorm(sample$voted,classifier3$meanNum_voted,classifier3$varNum_voted)
  p_likes<-dnorm(sample$likes,classifier3$meanTotalLikes,classifier3$varTotalLikes)
  p_budget<-dnorm(sample$Budget,classifier3$meanBudget,classifier3$varBudget)
  p_face <- dnorm(sample$faceNo, classifier3$meanFaceNo, classifier3$varFaceNo)
  p_reviews <- dnorm(sample$user_reviews, classifier3$meanUserReview,
                     classifier3$varUserReview)
  prior_flop1 <-nrow(class_good)/nrow(IMDB)
  r3=p_fblikes*p_face*p_reviews*prior_flop1 * p_weight *
    p_footsize*p_voted*p_likes*p_budget
  p_fblikes<-dnorm(sample$movie_likes,classifier4$meanfblikes,classifier4$varfblikes)
  p_weight <- dnorm(sample$Imdb, classifier4$meanImbd, classifier4$varImdb)
  p_footsize <- dnorm(sample$critic, classifier4$meannum_critic, classifier4$varnum_critic)
  p_voted<-dnorm(sample$voted,classifier4$meanNum_voted,classifier4$varNum_voted)
  p_likes<-dnorm(sample$likes,classifier4$meanTotalLikes,classifier4$varTotalLikes)
  p_budget<-dnorm(sample$Budget,classifier4$meanBudget,classifier4$varBudget)
  p_face <- dnorm(sample$faceNo, classifier4$meanFaceNo, classifier4$varFaceNo)
  p_reviews <- dnorm(sample$user_reviews, classifier2$meanUserReview,
                     classifier2$varUserReview)
  prior_flop1 <-nrow(class_amazing)/nrow(IMDB)
  r4=p_fblikes*p_face*p_reviews*prior_flop1 * p_weight *
    p_footsize*p_voted*p_likes*p_budget
  test<-c(r1,r2,r3,r4)
  answ=c("FLOP","WATCHABLE","GOOD","AMAZING")
  IMDB[i,11]=answ[which.max(test)]
}

library(e1071)
library(caTools)


library(caret)
IMDB=data.frame(IMDB)

y=data.frame(IMDB[,10])
ypred=data.frame(IMDB[,11])

cm=table(as.factor(unlist(y)),as.factor(unlist(ypred)))
cm