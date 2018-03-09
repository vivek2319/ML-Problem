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


glm.fit <- glmnet(x, y, family="binomial", alpha=0, lambda=0.001)
# summarize the fit
summary(glm.fit)
# make predictions
glmpredictions <- predict(glm.fit,as.matrix(finaltest),type = "response")


pcr.fit <- pcr(finaltrain$Target~.,data=finaltrain, validation="CV")

summary(pcr.fit)

validationplot(pcr.fit)
coefplot(pcr.fit)

pcr.predictions <- predict(pcr.fit, finaltest, ncomp = 3, type ="response" )

head(pcr.predictions)


glscalenum <- as.numeric(glmpredictions)
pcrscalenum <- as.numeric(pcr.predictions)


b <- (glscalenum+pcrscalenum)/2
submission2 <- data.frame(test$X, b)

myglmpc <- write.csv(file = "glmnetlambdacvinfuncpcrncmpo3.csv", submission2)



#This gave us 0.4378 150 rank
library(mlbench)

#Now let


fit <- knnreg(x, y, k=13)
# summarize the fit
print(fit)
# make predictions
predictions <- predict(fit, finaltest)
# summarize accuracy



bab <- 

knnregnum <- as.numeric(predictions)

bab <- (glscalenum + pcrscalenum + knnregnum)/3

submission2 <- data.frame(test$X, bab)

myglmpc <- write.csv(file = "knnregglmpcr.csv", submission2)

#This gave us rank 96


#

pcr.fit <- pcr(finaltrain$Target~.,data=finaltrain, validation="CV")

summary(pcr.fit)

validationplot(pcr.fit)
coefplot(pcr.fit)


pcr.predictions <- predict(pcr.fit, finaltest, ncomp = 2, type ="response" )

head(pcr.predictions)

submission <- data.frame(test$X, pcr.predictions)

write.csv(file="faktapcr2ncomp.csv",submission)

#This gave us 150 score 0.4378
################################################################
#Let's combine glm lasso and pcr 

glscalenum <- as.numeric(glmpredictions)
pcrscalenum <- as.numeric(pcr.predictions)

bab <-  (glscalenum+pcrscalenum)/2

submission <- data.frame(test$X, bab)

write.csv(file="glmlasso001and2ncomp.csv",submission)

##############################

#Try stacking on your own 

data <- data.frame(glscalenum, pcrscalenum)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

newone <- apply(data,1,getmode)

head(newone)

newsub <- data.frame(test$X, newone)

write.csv(file = "AvgVotglmnetlassopcrncomp3.csv", newsub)
# This gave us 0.4390

#Try weighted average 

newweight <- ((glscalenum*0.75)+(pcrscalenum*0.25)/2)

newweightdatafram <- data.frame(test$X, newweight)

write.csv(file = "weighted75glmspcr25.csv", newweightdatafram)
#This gave us 0.4368
#Try new average 

newweight2 <- ((glscalenum*0.25)+(pcrscalenum*0.75)/2)
newweightdatafram <- data.frame(test$X, newweight2)

write.csv(file = "weighted25glmspcr75.csv", newweightdatafram)
#This gave us 4385
#Try using xgboost

param <- list("booster"="gbtree",
  "objective" = "binary:logistic",
              "eval_metric" = "logloss",
              "eta" = 0.3,
                  gamma=0,
                 "max.depth" = 7,
               min_child_weight=1, subsample=1,colsample_bytree=1,
              "outputmargin"=FALSE)


bst.cv = xgb.cv(params =param, data = as.matrix(finaltrain[,1:6]), label =finaltrain$Target, nfold = 10, nrounds = 150, early_stopping_rounds = 20, maximize = F,verbose = TRUE, showsd = TRUE, stratified = TRUE)

summary(bst.cv)

min(bst.cv$test-logloss)

bst <- xgboost(data = as.matrix(finaltrain[,1:6]), label =as.numeric( finaltrain$Target), max.depth = 24, eta = 0.04, nround = 6,nthread = 4, objective = "binary:logistic", maximize = F, verbose = T,"eval_metric" = "logloss" )

xgboostpredictions<-predict(bst,as.matrix(finaltest))

head(xgboostpredictions)

mat <- xgb.importance(feature_names = colnames(finaltrain), model = bst)
xgb.plot.importance(importance_matrix = mat)



library(mlr)


lrn <- makeLearner("classif.xgboost",predict.type = "response")
lrn$par.vals <- list( objective="binary:logistic", eval_metric="logloss", nrounds=100L, eta=0.1)

#set parameter space
params <- makeParamSet( makeDiscreteParam("booster",values = c("gbtree","gblinear")), makeIntegerParam("max_depth",lower = 3L,upper = 10L), makeNumericParam("min_child_weight",lower = 1L,upper = 10L), makeNumericParam("subsample",lower = 0.5,upper = 1), makeNumericParam("colsample_bytree",lower = 0.5,upper = 1))

#set resampling strategy
rdesc <- makeResampleDesc("CV",stratify = T,iters=5L)


ctrl <- makeTuneControlRandom(maxit = 10L)

library(parallel)
library(parallelMap)

parallelStartSocket(cpus = detectCores())

#create tasks
traintask <- makeClassifTask (data = finaltrain,target = finaltrain$Target)
testtask <- makeClassifTask (data = test,target = "target")

#do one hot encoding`<br/> 
> traintask <- createDummyFeatures (obj = traintask,target = "target") 
> testtask <- createDummyFeatures (obj = testtask,target = "target")


#parameter tuning
mytune <- tuneParams(learner = lrn, task = y, resampling = rdesc, measures = acc, par.set = params, control = ctrl, show.info = T)
mytune$y 


#####################
#Let's try LDA

ldamodel <- lda(finaltrain$Target ~., data = finaltrain)

ldapred2 <- predict(object = ldamodel, finaltest, CV=T,prior=c(0.50,0.50))

#When prior probabilities are not set
aebaba <- ldapred$posterior[,2] 
subaebaba <- data.frame(cbind(test$X, aebaba))
write.csv(subaebaba,"aebabanustaldawithoutprior.csv")
#This gave us 0.4714
#When prior probabilities are set
aebaba2 <- ldapred2$posterior[,2]
subaebaba <- data.frame(cbind(test$X, aebaba2))
write.csv(subaebaba,"aebabanustaldawithprior.csv")
#this gave us 0.6794


#Let's try svm
x <- finaltrain[,1:6]
y <- finaltrain[,7]

modelsvm <- svm(finaltrain$Target~., data=finaltrain)

svmpred <- predict(modelsvm, finaltest, probability = TRUE, na.action=na.omit)


subsvm <- data.frame(cbind(test$X, svmpred))

write.csv(subaebaba,"nustasvm.csv")

#This gave us 0.6794

#Only lasso

faktalasso <- data.frame(test$X, glscalenum)
write.csv(faktalasso, "faktalasso.csv")
#This gave us 0.4389

#Only Ridge
glm.fitridge <- glmnet(x, y, family="binomial", alpha=1, lambda=0.001)
# summarize the fit
summary(glm.fitridge)
# make predictions
glmpredictionsridge <- predict(glm.fitridge,as.matrix(finaltest),type = "response")

glsnumridge <- data.frame(test$X, glmpredictionsridge)


write.csv(glsnumridge,"faktaridge.csv")

#This gave us 0.4371

# combine ridge and pcr 


newstack <- ((glmpredictionsridge+pcrscalenum)/2)

sue <- data.frame(test$X, newstack)

write.csv(sue, "ridgeandpcr.csv")

#This gave us 0.4323 79th rank
