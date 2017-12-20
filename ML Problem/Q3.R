BMOP <- read.csv("NIJ_BMOP.csv")

BMOP1 <- BMOP

library(mice)

md.pattern(BMOP[,2:10])

BMOP <- BMOP[,-5]

md.pattern(BMOP[,10:20])
BMOP <- BMOP[,-14]

md.pattern(BMOP[,20:30])
BMOP <- BMOP[,-23]

md.pattern(BMOP[,30:35])
#p35 p34 p36
BMOP <- BMOP[,- (33:35)]

md.pattern(BMOP[,35:40])
BMOP <- BMOP[,-39]

md.pattern(BMOP[,40:50])
BMOP <- BMOP[,-39]

md.pattern(BMOP[,50:60])
#p62 p55
BMOP <- BMOP[,-c(57,50)]

md.pattern(BMOP[,60:70])
#p75
BMOP <- BMOP[,-68]
md.pattern(BMOP1[,80:93])

drop <-c("p75","p80" ,"p82", "p83", "p81", "p79", "p86", "p90")
BMOP <- BMOP[,!names(BMOP) %in% drop]

a <- BMOP[,2:74]
a <- data.frame(apply(a,2,as.numeric))
a <- apply(a, 2, function(x){x <- replace(x, is.na(x), mean(x, na.rm=TRUE))})
BMOP2 <- data.frame(a)


###############################################################

###############GBM################
set.seed (32323)
library(caret)
trCtrl = trainControl (method = "cv", number = 10)

boostFit = train (out ~ ., trControl = trCtrl,
                  method = "gbm", data = BMOP2, verbose = FALSE)
boostFit

###############GBM################




###############LDA################
library(MASS)
ED.R1 <- lda(out ~ .,data=BMOP2)

cm <- confusionMatrix (BMOP2$out, predict (ED.R1, BMOP2)$class)
Accuracies <- cm$overall["Accuracy"]
Accuracies
###############LDA################

###############KNN################

model_knn <- train(out ~ .,data=BMOP2, method='knn')
model_knn

###############KNN################


###############ANN################
library(nnet)
modelfit <- multinom(out ~ .,data=BMOP2,maxit=5773, MaxNWts=6000,trace=F)
cm <- confusionMatrix (BMOP2$out, predict (modelfit, BMOP2))
Accuracies <- cm$overall["Accuracy"]
Accuracies
###############ANN################


###############naiveBayes################
library("e1071")
nb_laplace1 <- naiveBayes(out ~ .,data=BMOP2, laplace=1)
cm <- confusionMatrix (BMOP2$out, predict (nb_laplace1, BMOP2))
RMSE = sqrt(mean((BMOP2$out - Yhat).^2))

###############naiveBayes################


###############SVM################

svm_model <- svm(out ~ .,data=BMOP2)
AIC(svm_model)
svm_model
###############SVM################

LinearModl <- lm(out ~ .,data=BMOP2)
stepAIC(LinearModl)