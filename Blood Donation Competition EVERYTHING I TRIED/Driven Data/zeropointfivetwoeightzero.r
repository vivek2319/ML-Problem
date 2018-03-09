install.packages("drat", repos="https://cran.rstudio.com")
drat:::addRepo("dmlc")
install.packages("xgboost", repos="http://dmlc.ml/drat/", type = "source")

require(xgboost)
set.seed(1234)
train <- read.csv("Train.csv")
train1 <- train[-c(4)]
test  <- read.csv("Test.csv")
test1 <- test[-4]

#Please use mlogloss eval_metric here
#FNN package install kara pan use kar

param <- list("objective" = "binary:logistic",
              "eval_metric" = "logloss",
              "eta" = 1, "max.depth" = 8, 
              "outputmargin"=FALSE)
bst.cv = xgb.cv(params =param, data = as.matrix(train1[,1:4]), label =train1$Made.Donation.in.March.2007, nfold = 10, nrounds = 20)


bst <- xgboost(data = as.matrix(train1[,1:4]), label =as.numeric( train1$Made.Donation.in.March.2007), max.depth = 24, eta = 0.04, nround = 20,nthread = 4, objective = "binary:logistic")

preds=predict(bst,as.matrix(test1))

submission <- data.frame(test1$X, preds)

newxgb <- write.csv(file = "MondayBoost.csv", submission)
