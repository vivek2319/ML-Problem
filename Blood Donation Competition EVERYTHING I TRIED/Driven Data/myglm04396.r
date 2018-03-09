library(MASS)

train <-read.csv("Train.csv")

test <- read.csv("Test.csv")


train$Total.Volume.Donated..c.c.. <- NULL
test$Total.Volume.Donated..c.c.. <- NULL






ID <- train$X
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


names(finaltrain)

[1] "ID"                          "Months.since.Last.Donation" 
[3] "Number.of.Donations"         "Months.since.First.Donation"
[5] "Ratio"                       "AverageDonation"            
[7] "AverageWait"                 "Target"    



    

######
ID1 <- test$X

Ratio <- test$Months.since.Last.Donation/test$Months.since.First.Donation

AverageDonation <- test$Number.of.Donations / test$Months.since.First.Donation

AverageWait <- test$Months.since.First.Donation / test$Number.of.Donations



df <- data.frame(test[,2:4], Ratio, AverageDonation, AverageWait)

dftestnorm <- as.data.frame(lapply(df, normalize))
newtest <- data.frame(dftestnorm)
head(newtest)
names(newtest)

[1] "ID"                          "Months.since.Last.Donation" 
[3] "Number.of.Donations"         "Months.since.First.Donation"
[5] "Ratio"                       "AverageDonation"            
[7] "AverageWait"                
################
'''
param <- list("objective" = "binary:logistic",
              "eval_metric" = "logloss",
              "eta" = 1, "max.depth" = 8, 
              "outputmargin"=FALSE)
bst.cv = xgb.cv(params=param, data=as.matrix(finaltrain[,1:5]), label =finaltrain$Target, nfold = 10, nrounds = 20)


bst <- xgboost(data = as.matrix(finaltrain[,1:5]), label= finaltrain$Target, max.depth = 24, eta = 0.04, nround = 15,nthread = 4, objective = "binary:logistic")

preds=predict(bst,as.matrix(newtest))

submission <- data.frame(newtest$ID, preds)

newxgb <- write.csv(file = "Ratio.csv", submission)


###################################################
'''


model <- glm(finaltrain$Target~.,data = finaltrain, family = binomial(link="logit"))


preds <- predict(model,newdata = newtest, type = "response")

head(preds)

submission <- data.frame(ID1, preds)


myglm <- write.csv(file = "myglm2removedID.csv", submission)



