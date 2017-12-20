diab <- read.csv("PD.csv")
diab1 <- diab

library(mice)

md.pattern(diab)

# bmi bp skinth

library(Hmisc)

diab$bp <- as.numeric(diab$bp)
diab$skinth <- as.numeric(diab$skinth)
diab$bmi <- as.numeric(diab$bmi)

diab$skinth <- impute(diab$skinth, mean)
diab$bmi <- impute(diab$bmi, mean)
diab$bp <- impute(diab$bp, mean)

# # a <-data.frame(apply(diab,2,function(x) { boxplot.stats(diab[x])$out}))
# "npreg"    "glucose"  "bp"       "skinth"   "bmi"      "pedprob"  "age"    
# [8] "Diabetes"
# 
diab$bp <- as.numeric(diab$bp)
diab$skinth <- as.numeric(diab$skinth)
diab$bmi <- as.numeric(diab$bmi)


########LDA############
library(caret)
require(MASS)
ED.R1 <- lda(Diabetes ~ .,data=diab)

pred <- predict(ED.R1,diab,type = 'class')
cm <- confusionMatrix(pred$class, diab$Diabetes)
Accuracies <- cm$overall["Accuracy"]

Accuracies
summary(Accuracies)

################kNN##############

model_knn <- train(Diabetes ~ . ,data=diab, method='knn')
cm <- confusionMatrix(predict(model_knn,type="raw",newdata = diab), diab$Diabetes)
Accuracies <- cm$overall["Accuracy"]

Accuracies
summary(Accuracies)


################SVM ##############
library("e1071")
x <- svm(Diabetes ~ ., data=n)
cm <- confusionMatrix(predict(x,diab), diab$Diabetes)
Accuracies <- cm$overall["Accuracy"]

Accuracies
summary(Accuracies)

################GBM ##############
library("e1071")
boostFit = train (Diabetes ~ .,method = "gbm", data = diab, verbose = FALSE)
cm <- confusionMatrix(predict(boostFit,diab), diab$Diabetes)
Accuracies <- cm$overall["Accuracy"]

Accuracies
summary(Accuracies)

###############ANN################
library(nnet)

modelfit <- multinom(Diabetes ~ ., data = diab, maxit=500, trace=T)
cm <- confusionMatrix(predict(modelfit,diab), diab$Diabetes)
Accuracies <- cm$overall["Accuracy"]
Accuracies
summary(Accuracies)

###############naiveBayes################

modelfit <- naiveBayes(Diabetes ~ ., data = diab,laplace=1)

cm <- confusionMatrix(predict(modelfit,diab) ,diab$Diabetes)

Accuracies <- cm$overall["Accuracy"]
Accuracies
####################LOGISTIC##################################
aaa <- diab[,1:8]
modelfit <- glm(Diabetes ~ ., data = aaa,family = binomial(logit))
step(modelfit)
summ <-glm(Diabetes ~ npreg + glucose + bmi + pedprob,, data = aaa,family = binomial(logit))
modelfit <- glm(Diabetes ~ ., data = diab,family = binomial(logit))

diab$pred <- predict(modelfit,type="response")
diab$Pre_Diab <- ifelse(diab$pred>.5,"Yes","No")
cm <- confusionMatrix(diab$Pre_Diab  ,diab$Diabetes)

Accuracies <- cm$overall["Accuracy"]
Accuracies