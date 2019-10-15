library(tree)

tree.avo =tree(f1, avo_train)
summary(tree.avo)

plot(tree.avo)
text(tree.avo)

cv.avo =cv.tree(tree.avo)

prune.avo =prune.tree(tree.avo,best =5)
plot(prune.avo)
text(prune.avo,pretty =0)

mse_regreTree_test <- mean((yhat - avo_test$AveragePrice)^2)
mse_regreTree_test
