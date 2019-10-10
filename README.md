# BA-810-Team-7: Avodaco Prices

----- Contributors -----

Ziqin Ma, Qiaoling Huang, Shihan Li, Chenran Peng, Elmira Ushirova

----- File Index -----

`avocado.csv`: the original unedited file from Kaggle

`Presentation 1.R`: the script for plots we used in the first presentation

`Final Sciript Formatted.R`: formatted version of `avocado.csv` for modeling

----- October 3, 2019 -----

This is Cohort A Team 7 from Business Analytics program in Questrom School of Business, Boston University. We are creating this repository for the BA 810 (Supervised Machine Learning) team project. The dataset we are using is the `Avocado Price` dataset from [Kaggle](https://www.kaggle.com/neuromusic/avocado-prices).

This project is launched on the assumption that we are going to open an "Everything Avocado" store, primarily selling avocado products including avocado toasts, avocado smoothies, and guacamole & chips. We would like to make a budget on the price and volumn (numbers) of raw avocados. Given the `Avodaco Price` dataset, we would like to build models to predict these factors and apply those predictions to adjust our plan.

This project contains two phases. Phase I is a brief presentation of choice of dataset and the business problem. The team accomplished that on September 30, 2019 and got approval from the instructor.

The link will direct to the Phase I [slides](https://docs.google.com/presentation/d/1g5iKTYi_I7GHrR_ptDDSRX2YzNs0mWYvDxJCneS36Dc/edit#slide=id.p).

Phase II is the final presentation with the submission of our model script, which will due on October 16, 2019.

----- October 9 -----

Confirmed with Professor on how to use `Date` column in a time-series prediction. Since we have observed that our train data outcomes (i.e. average price) displays certain trends with time, the main criteria for choosing the optimal model is to find the one that has predictions with the same seasonality as the actual price in the test set. If there exist multiple of models, we will then compare the MSEs and find the one with the lowest.

We will write codes for several machine learning models, including least square, Ridge regression, Lasso regression, Decision tree/Random forest, and Boosting. 

Before getting started, we reformatted the original dataset for our convinience and split the train and test sets. The script for this step is included in `Formatted Script Final.R`.

----- October 10 -----


