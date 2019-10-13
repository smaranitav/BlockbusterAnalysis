#Bin Response Variable
IMDB$binned_score <- cut(IMDB$imdb_score, breaks = c(0,4,6,8,10))
IMDB <- IMDB[,c(9,4,5,14,12,2,3,13,1,6,10,7,8,11,15)]
colnames(IMDB) <- c("budget", "gross", "user_vote", "critic_review_ratio",
                    "movie_fb", "director_fb", "actor1_fb", "other_actors_fb",
                    "duration", "face_number", "year", "country", "content",
                    "imdb_score", "binned_score")
IMDB
#split DAta
set.seed(45)
train.index <- sample(row.names(IMDB), dim(IMDB)[1]*0.6)
valid.index <- sample(setdiff(row.names(IMDB), train.index), dim(IMDB)[1]*0.2)
test.index <- setdiff(row.names(IMDB), union(train.index, valid.index))
train <- IMDB[train.index, ]
valid <- IMDB[valid.index, ]
test <- IMDB[test.index, ]
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)
class.tree <- rpart(binned_score ~ . -imdb_score, data = train, method = "class")
## plot tree
prp(class.tree, type = 1, extra = 1, under = TRUE, split.font = 2, varlen = 0)
set.seed(51)
cv.ct <- rpart(binned_score ~ . -imdb_score, data = train, method = "class",
               cp = 0.00001, minsplit = 5, xval = 5)



printcp(cv.ct)
#The 8th Tree has lowest cross validation score
# prune by lowest cp
pruned.ct <- prune(cv.ct,
                   cp = cv.ct$cptable[which.min(cv.ct$cptable[,"xerror"]),"CP"])
length(pruned.ct$frame$var[pruned.ct$frame$var == "<leaf>"])
prp(pruned.ct, type = 1, extra = 1, split.font = 1, varlen = -10)
# apply model on training set
tree.pred.train <- predict(pruned.ct, train, type = "class")
# generate confusion matrix for training data
confusionMatrix(tree.pred.train, train$binned_score)
# apply model on validation set
tree.pred.valid <- predict(pruned.ct, valid, type = "class")
# generate confusion matrix for validation data
confusionMatrix(tree.pred.valid, valid$binned_score)
# apply model on test set
tree.pred.test <- predict(pruned.ct, test, type = "class")
# generate confusion matrix for test data
confusionMatrix(tree.pred.test, test$binned_score)
