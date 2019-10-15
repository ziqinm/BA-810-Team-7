options(stringsAsFactors = FALSE)
install.packages("fastDummies")
install.packages("glmnet")
install.packages("caret")
install.packages("leaps")
library(readr)
library(tidyverse)
library(lubridate)
library(fastDummies)
library(glmnet)
library(caret)
library(leaps)
ds <- read_csv("avocado.csv")


# Switch the order of rows
ds <- ds %>% 
  group_by(type, region) %>% 
  select(X1, year, Date, type, region, everything()) %>% 
  arrange(Date)

# Change `X1` to `ID`
colName <- names(ds)
colName[1] <- "ID"
names(ds) <- colName

# Assign distinct number id to each observation in `ID` column
ds$ID <- seq(nrow(ds))
glimpse(ds)

# Add a `month` column
ds$month <- month(ds$Date)
ds <- ds %>% 
  select(ID, year, month, everything())

# Subset `type` column into `conventional` and `organic`
dsNew <- dummy_cols(ds, select_columns = "type") %>% 
  select(ID, year, month, region, type_conventional, type_organic, 
         everything(), -type)

# Other types
dsNew$other_PLU <- dsNew$`Total Volume` - dsNew$`4046` - dsNew$`4225` - dsNew$`4770`

dsNew <- dsNew %>% 
  select(1:3, Date, everything())

# Categorize `region` into US Areas
uniqueRegion <- unique(dsNew$region)
uniqueRegion <- as.data.frame(uniqueRegion)
uniqueRegion$Area <- NA
uniqueRegion$Area[1] <- "NewEngland"
uniqueRegion$Area[2] <- "Southeast"
uniqueRegion$Area[3] <- "Mideast"
uniqueRegion$Area[4] <- "RockyMountain"
uniqueRegion$Area[5] <- "NewEngland"
uniqueRegion$Area[6] <- "Mideast"
uniqueRegion$Area[7] <- "FarWest"
uniqueRegion$Area[8] <- "Southeast"
uniqueRegion$Area[9] <- "GreatLakes"
uniqueRegion$Area[10] <- "GrateLakes"
uniqueRegion$Area[11] <- "GrateLakes"
uniqueRegion$Area[12] <- "Southwest"
uniqueRegion$Area[13] <- "RockyMountain"
uniqueRegion$Area[14] <- "GrateLakes"
uniqueRegion$Area[15] <- "GrateLakes"
uniqueRegion$Area[16] <- "GrateLakes"
uniqueRegion$Area[17] <- "Mideast"
uniqueRegion$Area[18] <- "NewEngland"
uniqueRegion$Area[19] <- "Southeast"
uniqueRegion$Area[20] <- "GrateLakes"
uniqueRegion$Area[21] <- "Southeast"
uniqueRegion$Area[22] <- "FarWest"
uniqueRegion$Area[23] <- "FarWest"
uniqueRegion$Area[24] <- "Southeast"
uniqueRegion$Area[25] <- "Southeast"
uniqueRegion$Area[26] <- "Southeast"
uniqueRegion$Area[27] <- "Southeast"
uniqueRegion$Area[28] <- "Southeast"
uniqueRegion$Area[29] <- "Mideast"
uniqueRegion$Area[30] <- "NewEngland"
uniqueRegion$Area[31] <- "NewEngland"
uniqueRegion$Area[32] <- "Southeast"
uniqueRegion$Area[33] <- "Mideast"
uniqueRegion$Area[34] <- "Southwest"
uniqueRegion$Area[35] <- "Mideast"
uniqueRegion$Area[36] <- "Plains"
uniqueRegion$Area[37] <- "FarWest"
uniqueRegion$Area[38] <- "Southeast"
uniqueRegion$Area[39] <- "Southeast"
uniqueRegion$Area[40] <- "Southeast"
uniqueRegion$Area[41] <- "FarWest"
uniqueRegion$Area[42] <- "FarWest"
uniqueRegion$Area[43] <- "FarWest"
uniqueRegion$Area[44] <- "FarWest"
uniqueRegion$Area[45] <- "Southeast"
uniqueRegion$Area[46] <- "Southeast"
uniqueRegion$Area[47] <- "Southeast"
uniqueRegion$Area[48] <- "FarWest"
uniqueRegion$Area[49] <- "Plains"
uniqueRegion$Area[50] <- "Mideast"
uniqueRegion$Area[51] <- "Southeast"
uniqueRegion$Area[52] <- "TotalUS"
uniqueRegion$Area[53] <- "FarWest"
uniqueRegion$Area[54] <- "Southwest"
names(uniqueRegion)[1] <- "region"

avo <- dsNew %>% 
  left_join(uniqueRegion, by = "region") %>% 
  select(1:5, Area, everything())

View(avo)

avo <- dummy_cols(avo, select_columns = "Area")

View(avo)

##### Formatting Done #####


### Rename Column Names ##3
avoFormat <- avo
colnames(avoFormat)

names(avo)[10] <- "TotalVolume"
names(avo)[14] <- "TotalBags"
names(avo)[15] <- "SmallBags"
names(avo)[16] <- "LargeBags"
names(avo)[17] <- "XLargeBags"
names(avo)[11] <- "PLU4046"
names(avo)[12] <- "PLU4225"
names(avo)[13] <- "PLU4770"

colnames(avo)

### Split the dataset into train and test sets ###
set.seed(1234)
avo_train <- avo %>% filter(as.Date(Date) < "2017-03-01")
avo_train %>%
  filter(year == 2017, month == 2)
avo_test <- avo %>% filter(as.Date(Date) >= "2017-03-01")
avo_test %>%
  filter(year == 2018, month == 3)


## Stepwise backward selection
regfit.bwd = regsubsets(f1, data = avo_train, nvmax = 19, method = "backward")
summary(regfit.bwd)

sub_model<-lm(f1, data = avo_train)
yhat_train_stepwise <- predict(sub_model, avo_train)
MSE_train_stepwise <- mean((avo_train$AveragePrice - yhat_train_stepwise)^2)
MSE_train_stepwise
yhat_test_stepwise <- predict(sub_model, avo_test)
MSE_test_stepwise <- mean((avo_test$AveragePrice - yhat_test_stepwise)^2)
MSE_test_stepwise

## based on the coefficients, eliminate "Area_TotalUS", "type_organic".
f1_1 <- as.formula(AveragePrice ~ month + type_conventional + TotalVolume + 
                      PLU4046 + PLU4770 + PLU4225 + SmallBags + LargeBags + XLargeBags + 
                      + Area_NewEngland + Area_Southeast
                    + Area_Mideast + Area_RockyMountain
                    + Area_FarWest + Area_GreatLakes
                    + Area_Southwest
                    + Area_Plains)
regfit.bwd1 = regsubsets(f1_1, data = avo_train, nvmax = 19, method = "backward")
summary(regfit.bwd1)

sub_model1<-lm(f1_1, data = avo_train)
yhat_train_stepwise1 <- predict(sub_model1, avo_train)
MSE_train_stepwise1 <- mean((avo_train$AveragePrice - yhat_train_stepwise1)^2)
MSE_train_stepwise1
yhat_test_stepwise1 <- predict(sub_model1, avo_test)
MSE_test_stepwise1 <- mean((avo_test$AveragePrice - yhat_test_stepwise1)^2)
MSE_test_stepwise1

## continue to use backward selection to train the model
## eliminate "LargeBags"
f1_2 <- as.formula(AveragePrice ~ month + type_conventional + TotalVolume + 
                     PLU4046 + PLU4770 + PLU4225 + SmallBags + XLargeBags
                     + Area_NewEngland +
                   Area_Mideast + Area_RockyMountain
                   + Area_FarWest + Area_GreatLakes + Area_Southwest + Area_Plains
                   )
regfit.bwd2 = regsubsets(f1_2, data = avo_train, nvmax = 19, method = "backward")
summary(regfit.bwd2)

sub_model2<-lm(f1_2, data = avo_train)
yhat_train_stepwise2 <- predict(sub_model2, avo_train)
MSE_train_stepwise2 <- mean((avo_train$AveragePrice - yhat_train_stepwise2)^2)
MSE_train_stepwise2
yhat_test_stepwise2 <- predict(sub_model2, avo_test)
MSE_test_stepwise2 <- mean((avo_test$AveragePrice - yhat_test_stepwise2)^2)
MSE_test_stepwise2

## calculate the yhat price for the avo dataset
yhat_avo_avgprice <- predict(sub_model2, avo)
library(ggplot2)
df1_bwd<-avo %>% 
  select(Date, AveragePrice)
df2_bwd<-cbind(df1_bwd, yhat_avo_avgprice)
colnames(df2_bwd)
names(df2_bwd)[3]<-"AveragePrice_hat"
## Plot the actual average price and the predictive average price
plot1_bwd <- df2_bwd %>% 
  group_by(Date) %>% 
  summarize(
    MeanAvg=mean(AveragePrice),
    MeanAvg_hat=mean(AveragePrice_hat))%>% 
    ggplot()+
    geom_line(aes(Date, MeanAvg),color = "blue")+
    geom_line(aes(Date, MeanAvg_hat), color = "red")+
    theme_classic()

plot1_bwd + geom_vline(aes(xintercept = as.numeric(Date[113])), 
                   linetype = "dashed", size = 1,
                   color = "lightgreen")
