library(party)
library(partykit)
library(tree)
library(rpart) 
library(rpart.plot)
library(randomForest)
library(rsample)

#TREE
train2 <- df_reg[training_obs, ]
test2 <- df_reg[-training_obs, ]
dev.off()
tree1 = ctree(Fertility~., data = df_reg)
tree1
summary(tree1)
plot(tree1)

tree2  = rpart(Fertility~., data = df_reg)
rpart.plot(tree2, cex=0.6)

#terzo albero con solo trian
tree3=rpart(Fertility~., data = train2)
rpart.plot(tree3)
pred.tree = predict(tree3, test2)
mse <- mean((pred.tree - test2$Fertility)^2)
mse
RMSE<- sqrt(mse)
RMSE
printcp(tree3)
plotcp(tree3)
bestcp <- tree3$cptable[which.min(tree3$cptable[,"xerror"]),"CP"]
bestcp
pruned.tree <- prune(tree3, cp = bestcp)
pruned.tree <- prune(tree3, cp =  0.022120)
rpart.plot(pruned.tree)
pred.prune = predict(pruned.tree, test2)
mse <- mean((pred.prune - test2$Fertility)^2)

RMSE <- sqrt(mse)
RMSE

tree4 =tree(Fertility~., data = train2)
tree4
plot(tree4)
text(tree4)
yhat = predict(tree4, test2)
mse <- mean((yhat-test2$Fertility)^2)
sqrt(mse)

cvtree = cv.tree(tree4)
plot(cvtree$size, cvtree$dev, type="b")
prunetree = prune.tree(tree4, best=4)
plot(prunetree)
yhat2 = predict(prunetree, test2)
mean((yhat2-test2$Fertility)^2)


#BAGGING AND RANDOM FOREST
#For random forest we don't use all the variables, we can specify
#which variables we want to use
#sqrt of the numbers of predictors
#variables highly correlated, using samples we can avoid
#dubset to avoid overfitting and decorellated 
#facciamo random forest e plottiamo variance importance delle variabili
set.seed(1)
dim(train2)
bag.Fertility=randomForest(Fertility~.,data=train2,mtry=10,importance=TRUE)
bag.Fertility
yhat.bag = predict(bag.Fertility,newdata=test2)
plot(yhat.bag, test2$Fertility)
abline(0,1)
mse <- mean((yhat.bag-test2$Fertility)^2)
sqrt(mse)
#smallest mse
png("varImpPlot.png", width = 800, height = 600)  # Set the desired dimensions
par(mar = c(5, 4, 4, 2))  # Adjust plot margins
varImpPlot(bag.Fertility)  # Generate variable importance plot
dev.off()  
varImpPlot(bag.Fertility)
#avoiding overfitting

set.seed(1)
dim(train2)
rf.Fertility=randomForest(Fertility~.,data=train2,importance=TRUE)
rf.Fertility
yhat.rf = predict(rf.Fertility,newdata=test2)
plot(yhat.rf, test2$Fertility)
abline(0,1)
mse<- mean((yhat.rf-test2$Fertility)^2)
sqrt(mse)
varImpPlot(rf.Fertility)
importance(rf.Fertility)
oob.err=double(10)
test.err=double(10)
for(mtry in 1:10){
  fit=randomForest(Fertility~.,data=train2,mtry=mtry,ntree=400)
  oob.err[mtry]=fit$mse[400]
  pred=predict(fit,test2)
  test.err[mtry]=with(test2,mean((Fertility-pred)^2))
  cat(mtry," ")
}
matplot(1:mtry,cbind(test.err,oob.err),pch=19,col=c("red","blue"),type="b",ylab="Mean Squared Error")
legend("topright",legend=c("Test","OOB"),pch=19,col=c("red","blue"))


#you have not only one tree, you don't have the
#visualization
#bootstrap of 500 times the initial set and in each
#run only 3 variable are selected in a random way

