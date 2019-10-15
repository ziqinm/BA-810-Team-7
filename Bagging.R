library(randomForest)

set.seed (1)
bag.avo =randomForest(f1, data = avo_train, 
                           mtry = 20, importance =TRUE)
bag.avo

yhat.bag = predict (bag.avo, newdata = avo_test)
plot(yhat.bag, avo_test$AveragePrice)
abline (0,1)
mse_bag_test <- mean((yhat.bag - avo_test$AveragePrice)^2)
mse_bag_test
