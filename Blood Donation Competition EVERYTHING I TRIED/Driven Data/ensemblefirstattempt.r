library(class)
library(MASS)
library(xgboost)
library(e1071)
library(caret)
library(ggplot2)
library(deepboost)
library(glmnet)
library(ISLR)
library(leaps)
library(pls)
library(randomForest)


train <-read.csv("bllodtrain.csv")

test <- read.csv("bllodtest.csv")


train$Total.Volume.Donated..c.c.. <- NULL
test$Total.Volume.Donated..c.c.. <- NULL

Target <- as.numeric(as.character(train$Made.Donation.in.March.2007))

Ratio <- train$Months.since.Last.Donation/train$Months.since.First.Donation

AvgDon <- train$Number.of.Donations / train$Months.since.First.Donation

AvgWait <- train$Months.since.First.Donation / train$Number.of.Donations

partone <- data.frame("Months.since.Last.Donation"=train$Months.since.Last.Donation,"Number.of.Donations"=train$Number.of.Donations,"Months.since.First.Donation"= train$Months.since.First.Donation,"Ratio"=Ratio, "AverageDonation"=AvgDon, "AverageWait"=AvgWait)


normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

dfNorm <- as.data.frame(lapply(partone, normalize))
head(dfNorm)
finaltrain <- data.frame(dfNorm,Target)
head(finaltrain)


#############
#Do same with test data


Ratio <- test$Months.since.Last.Donation/test$Months.since.First.Donation

AvgDon <- test$Number.of.Donations / test$Months.since.First.Donation

AvgWait <- test$Months.since.First.Donation / test$Number.of.Donations

partwo <- data.frame("Months.since.Last.Donation"=test$Months.since.Last.Donation,"Number.of.Donations"=test$Number.of.Donations,"Months.since.First.Donation"= test$Months.since.First.Donation,"Ratio"=Ratio, "AverageDonation"=AvgDon, "AverageWait"=AvgWait)


normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

partthree <- as.data.frame(lapply(partwo, normalize))

head(partthree)
finaltest <- data.frame(partthree)
head(finaltest)

################We are done preparing train and test
#Let's train our model
#Glm model
x <- as.matrix(finaltrain[,1:6])
y <- as.matrix(finaltrain[,7])
# fit model
glm.fit <- glmnet(x, y, family="binomial", alpha=1, lambda=0.001)
# summarize the fit
summary(glm.fit)
# make predictions
glmpredictions <- predict(glm.fit,as.matrix(finaltest),type = "response")

###################
#second model Xgboost
param <- list("objective" = "binary:logistic",
              "eval_metric" = "logloss",
              "eta" = 1, "max.depth" = 8, 
              "outputmargin"=FALSE)
bst.cv = xgb.cv(params =param, data = as.matrix(finaltrain[,1:6]), label =finaltrain$Target, nfold = 10, nrounds = 50)

summary(bst.cv)


bst <- xgboost(data = as.matrix(finaltrain[,1:6]), label =as.numeric( finaltrain$Target), max.depth = 24, eta = 0.04, nround = 35,nthread = 4, objective = "binary:logistic")

xgboostpredictions<-predict(bst,as.matrix(finaltest))

head(xgboostpredictions)
############################
#Third model deepboost
deepboostmodel <- deepboost(finaltrain$Target~., data = finaltrain,instance_weights = NULL, tree_depth =8,num_iter = 35,beta = 0, lambda = 0.001,loss_type = 'l',verbose = T )

deepeval <- deepboost.evaluate(object = deepboostmodel, data = finaltrain)

deepbostpredictions <- deepboost.predict(deepboostmodel, newdata = finaltest, type = "response")
dpbpredictions <- deepbostpredictions[,2]
dpbpredictions

###################
##Next model pcr
pcr
pcr.fit <- pcr(finaltrain$Target~.,data=finaltrain, validation="CV")

summary(pcr.fit)

validationplot(pcr.fit)
coefplot(pcr.fit)

pcr.predictions <- predict(pcr.fit, finaltest, ncomp = 3, type ="response" )

head(pcr.predictions)
#################################
#Fianlly Random Forest
mt <- trunc(sqrt(ncol(finaltrain))) +1

rfmodel <- randomForest(finaltrain$Target~., data = finaltrain,mtry=mt,importance=TRUE)

importance(rfmodel)
varImpPlot(rfmodel)
rfpredictions <- predict(rfmodel, finaltest, type="response")
head(rfpredictions)
length(rfpredictions)

#Let's stack all the models'

#Method-1
#Averageofallpredictions

#Check classes of all
#> class(glmpredictions)
#[1] "matrix"
#> class(xgboostpredictions
#        + )
#[1] "numeric"
#> class(dpbpredictions)
#[1] "numeric"
#> class(pcr.predictions)
#[1] "array"
#> class(rfpredictions)
#[1] "numeric"

#covert all classes into numeric if they are not
glmpredictions_num <- as.numeric(glmpredictions)
pcr.pred_num <- as.numeric(pcr.predictions)


a <- (glmpredictions_num +xgboostpredictions+dpbpredictions+pcr.pred_num+rfpredictions)/5 

submission <- data.frame(test$X, a)

myglm <- write.csv(file = "mthd1avgall.csv", submission)

#This gave us 0.4859
#Let's combine glm and pcr 
glscalenum <- as.numeric(glmpredictions)
pcrscalenum <- as.numeric(pcr.predictions)
b <- (glscalenum+pcrscalenum)/2
submission2 <- data.frame(test$X, b)

myglmpc <- write.csv(file = "scaledorgnlstandrdizglmpcrncomp3.csv", submission2)
#############
####
#This gave us 0.4325
##### Hurreeeyyyy!!!!!!!!!!!!
#this is because we combined glm and pcr
##########
#####

#Let's submit only using pcr see what we get
submission2 <- data.frame(test$X, pcr.pred_num)

myglmpc <- write.csv(file = "faktapcr.csv", submission2)
#4378 not that good
#Let's try regsubsets and lasso

regmodel <- regsubsets(finaltrain$Target~., data = finaltrain, method = "forward")
coef(regmodel,4)
#(Intercept)  Months.since.Last.Donation 
#0.4884039                  -0.1776634 
#Number.of.Donations Months.since.First.Donation 
#1.0856576                  -0.5921894 
#Ratio 
#-0.2885511 

#use this function
predict.regsubsets= function(object, newdata, id,...)
{
  form = as.formula(object$call[[2]]) # extract model ("call")
  mat = model.matrix(form, newdata)
  coefi= coef(object, id=id)
  xvars= names(coefi)
  mat[,xvars] %*% coefi
}

#Now perform regsubsets again
regmodel <- regsubsets(finaltrain$Target~., data = finaltrain, nvmax = 6)

coef(regmodel,3)
#(Intercept)         Number.of.Donations 
#0.5006208                   1.1297329 
#Months.since.First.Donation                       Ratio 
#-0.6556935                  -0.3288215 

#See the difference?
#set up a grid of values for lambda; the higher the lambda, the greater the shrinkage penalty
grid <- 10^seq(10,-2,length=100)
grid

lasso.mod <-glmnet(x,y,alpha = 1, lambda = grid , standardize = F)
plot(lasso.mod)

set.seed(7)

cv.out<-cv.glmnet(x,y,alpha = 1)
plot(cv.out)

bestlam<-cv.out$lambda.min
lasso.pred<-predict(lasso.mod, s = bestlam, newx=finaltest)

mean((lasso.pred-y.test)^2)
#[1] 152424.1

bestlam
[1] 0.0002188833
log(bestlam)
[1] -8.426972

newglm <- glmnet(x,y, family = "binomial", alpha=1, lambda = 0.0002188833)


newglmpred <- predict(newglm,as.matrix(finaltest),type = "response")

submission <- data.frame(test$X, newglmpred)

myglm <- write.csv(file = "myglmnetLassoalphazero.csv", submission)

###umn not good 
#Let's combine this with elastic net and see the solutioni if works well, we will combine lasso, elastic net and pcr solution Okay! 

elastmodel <- glmnet(x,y, family = "binomial", alpha = 0.5, lambda = 0.00036)

elastpred <- predict(elastmodel,as.matrix(finaltest),type = "response")

elastic  <- as.numeric(elastpred)
glmpredictions_num
pcr

#We are combining Lasso, elastic, and pcr

newmutant <- (elastic+glmpredictions_num+pcr.pred_num)/3

submission <- data.frame(test$X, newmutant)

myglm <- write.csv(file = "laselaspcr.csv", submission)
#This gave us 84 rank, stil not good
#Let's try 
#plsr 

form <-  formula(finaltrain$Target~.)

myplsr <- plsr(form,scale = FALSE, data=finaltrain, validation="CV")

myplsrpred <- predict(myplsr, newdata=finaltest, type="response")

pls.RMSEP = RMSEP(myplsr, estimate="CV")
plot(pls.RMSEP, main="RMSEP PLS Solubility", xlab="components")
min_comp = which.min(pls.RMSEP$val)
points(min_comp, min(pls.RMSEP$val), pch=1, col="red", cex=1.5)
#min_comp = 6
plot(myplsr, ncomp=6, asp=1, line=TRUE)

myplsrpred <- predict(myplsr, newdata=finaltest, ncomp=6, type="response")

myplsrpred

submission <- data.frame(test$X, myplsrpred)

sub <- write.csv(file = "onlyplsr.csv", submission)
#This gave us 84 rank 

#Let's combine plsr, glm(lasso) and pcr 

myplsrprednum <- as.numeric(myplsr)

a <- (glmpredictions_num+pcr.pred_num + myplsrpred)/3
submission <- data.frame(test$X, a)

sub <- write.csv(file = "glmlassopcrplsr.csv", submission)

#This gave 0.4380 not good 

#So far glm (lasso) and pcr gave us best score 79
#####Stack model Method 2  Predict using each base layer model for training data and test data
####
#AV

num_models <- 6
itertions <- 1000

library(Metrics)

rmsle_mat <- matrix(0,num_models,2)
rmsle_mat[,2] <- 1:num_models

for(i in 1:num_models)
  {
  rmsle_mat[i,1] <- rmsle(finaltrain[,i],finaltrain[,num_models+1])
  print(rmsle(finaltrain[,i],finaltrain[,num_models+1]))
}

best_model_no <- rmsle_mat[rmsle_mat[,1] == min(rmsle_mat[,1]),2]
best_model_no




amat <- matrix(0,1000,itertions)
prediction_test <- matrix(0,nrow(finaltest),1)
prediction_train <- matrix(0,nrow(finaltest),1)

for (j in 1:itertions){
  rmsle_in <- 1
  rmsle_new <- matrix(0,num_models,2)
  rmsle_new[,2] <- 1:num_models
  print(j)
  t = 1
  set.seed(j*121)
  train1 <- finaltrain[sample(1:nrow(finaltrain), 10000,replace=FALSE),]
  for(i in 1:num_models){
    rmsle_mat[i,1] <- rmsle(train1[,i],train1[,num_models+1])
  }
  best_model_no <- rmsle_mat[rmsle_mat[,1] == min(rmsle_mat[,1]),2]
  prediction <- train1[,best_model_no]
  prediction_1 <- test[,best_model_no]
  prediction_2 <- train[,best_model_no]
  amat[t,j] <- best_model_no
  while(-1 < 0) {
    t <- t + 1
    prediction1 <- prediction
    for (i in 1:num_models){
      prediction1 <- ((t*prediction) + train1[,i])/(t+1)
      rmsle_new[i,1] <- rmsle(prediction1,train1[,num_models+1])
    }
    rmsle_min <- min(rmsle_new[,1])
    model_no <- rmsle_new[rmsle_new[,1] == min(rmsle_new[,1]),2]
    if(rmsle_in < rmsle_min) {break} else {
      rmsle_in <- rmsle_min
      prediction <- (((t-1)*prediction) + train1[,model_no])/t
      prediction_1 <- (((t-1)*prediction_1) + finaltest[,model_no])/t
      prediction_2 <- (((t-1)*prediction_2) + train[,model_no])/t
      amat[t,j] <- model_no
      print(rmsle_in)
    }
  }
  prediction_test <- cbind(prediction_test,prediction_1)
  prediction_train <- cbind(prediction_train,prediction_2)
  }