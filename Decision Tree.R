library(rpart)
library(rpart.plot)

fit.tree <- rpart(f1,
                  avo_train,
                  control = rpart.control(cp = 0.001))

par(xpd = TRUE)
plot(fit.tree, compress=TRUE)
text(fit.tree, use.n=TRUE)
rpart.plot(fit.tree, type = 1)

yhat.train.tree <- predict(fit.tree, avo_train)
mse.train.tree <- mean((avo_train$AveragePrice - yhat.train.tree)^2)
mse.train.tree
# 0.03348875

yhat.test.tree <- predict(fit.tree, avo_test)
mse.test.tree <- mean((avo_test$AveragePrice - yhat.test.tree)^2)
mse.test.tree
# 0.1470199

