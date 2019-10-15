boostingcv <- gbm(f1,
                  data = avo_train,
                  distribution = "gaussian",
                  n.trees = 150,
                  interaction.depth = 4,
                  cv.folds = 10,
                  shrinkage = 0.1)
relative.influence(boostingcv)

yhat_btree <- predict(boostingcv, avo_train, n.trees = 150)
mse_btree <- mean((yhat_btree - y1_train) ^ 2)
print(mse_btree)
# 0.03000924

yhat_btree_test <- predict(boostingcv, avo_test, n.trees = 150)
mse_btree_test <- mean((yhat_btree_test - y1_test) ^ 2)
print(mse_btree_test)
# 0.129119

avo_train$prediction_btree <- yhat_btree
avo_test$prediction_btree <- yhat_btree_test

avo_plot <- rbind(avo_train, avo_test)

btree_plot <- avo_plot %>% 
  group_by(Date) %>% 
  summarise(meanAvg = mean(AveragePrice),
            meanAvg_hat = mean(prediction_btree)) %>% 
  ggplot() +
  geom_line(aes(Date, meanAvg), col = "darkseagreen") + 
  geom_line(aes(Date, meanAvg_hat), col = "#F2E880") + 
  labs(title = "Boosting with k = 10",
       subtitle = "ntree = 150, depth= 4, shrinkage = 0.1",
       y = "Mean average price") +
  theme_clean()

btree_plot + 
  geom_vline(aes(xintercept = as.numeric(Date[113])),
             linetype = "dashed", size = 1, col = "#666666")
