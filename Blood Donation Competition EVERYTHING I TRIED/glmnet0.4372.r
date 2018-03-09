

library(glmnet)

# load data
data(longley)
x <- as.matrix(finaltrain[,1:6])
y <- as.matrix(finaltrain[,7])
# fit model
fit <- glmnet(x, y, family="binomial", alpha=1, lambda=0.001)
# summarize the fit
summary(fit)
# make predictions
predictions <- predict(fit, x, type="link")
predictions <- predict(fit,as.matrix(newtest),type = "response")
submission <- data.frame(ID1, predictions)

myglm <- write.csv(file = "myglmnet.csv", submission)


plot(fit)




grid = expand.grid(.n.trees=seq(100,500, by=200), .interaction.depth=seq(1,4, by=1), .shrinkage=c(.001,.01,.1), .n.minobsinnode=10)

