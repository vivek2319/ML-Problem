fit2 <- glmnet(x, y, family="binomial")

best lambada = 0.0008051

newpred <- predict(fit2, newx = as.matrix(newtest),type="response", s=0.0008051)

head(newpred)
submission <- data.frame(ID1, newpred)

myglm1 <- write.csv(file = "myglmnetwithautomatedlasso.csv", submission)
