install.packages("drat", repos="https://cran.rstudio.com")
drat:::addRepo("dmlc")
install.packages("xgboost", repos="http://dmlc.ml/drat/", type = "source")

require(xgboost)
set.seed(1234)
train <- read.csv("Train.csv")
test  <- read.csv("Test.csv")

param <- list("objective" = "binary:logistic",
              "eval_metric" = "logloss",
              "eta" = 1, "max.depth" = 8, 
              "outputmargin"=FALSE)
bst.cv = xgb.cv(params =param, data = as.matrix(train[,1:5]), label =train$Made.Donation.in.March.2007, nfold = 10, nrounds = 20)


bst <- xgboost(data = as.matrix(train[,1:5]), label =as.numeric( train$Made.Donation.in.March.2007), max.depth = 24, eta = 0.04, nround = 20,nthread = 4, objective = "binary:logistic")

preds=predict(bst,as.matrix(test))

submission <- data.frame(test$X, preds)

newxgb <- write.csv(file = "newxgb1.csv", submission)
