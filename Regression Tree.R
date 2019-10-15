library(tree)

tree.avo =tree(f1, avo_train)
summary(tree.avo)

plot(tree.avo)
text(tree.avo)

cv.avo =cv.tree(tree.avo)

prune.avo =prune.tree(tree.avo,best =5)
plot(prune.avo)
text(prune.avo,pretty =0)

yhat = predict(tree.avo ,newdata = avo_test)
plot(yhat, avo_test$AveragePrice)
abline (0,1)
mean((yhat - avo_test$AveragePrice)^2)
