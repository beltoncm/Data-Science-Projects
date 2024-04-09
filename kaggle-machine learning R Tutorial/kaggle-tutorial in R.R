
setwd = ("C:/Users/belto/Documents/Data-Science-Projects/kaggle-machine learning R Tutorial")


# ORIGINAL
#library(tidyverse)
#library(reshape2)

# Housecleaning for running out-of-the-box with
#unknown configuration

if(!require('tidyverse')) {
  install.packages('tidyverse')
  library('tidyverse')
}

if(!require('reshape2')) {
  install.packages('reshape2')
  library('reshape2')
}



housing = read.csv('C:/Users/belto/Documents/Data-Science-Projects/kaggle-machine learning R Tutorial/housing.csv')

head(housing)


summary(housing)
####unique(housing$ocean_proximity)

par(mfrow=c(2,5))
colnames(housing)

ggplot(data = melt(housing), mapping = aes(x = value)) + 
  geom_histogram(bins = 30) + facet_wrap(~variable, scales = 'free_x')


###housing$total_rooms

#head(housing)


housing$total_bedrooms[is.na(housing$total_bedrooms)] = median(housing$total_bedrooms , na.rm = TRUE)

###summary(housing$total_bedrooms)


#Fix the total columns - make them means
# Make colums for a mean number of bedrooms and rooms.

housing$mean_bedrooms = housing$total_bedrooms/housing$households
housing$mean_rooms = housing$total_rooms/housing$households

# drop the original total bedrooms (this is actually totally bedrooms in a district)
# drop the original total_rooms (this is actually totally bedrooms in a district)
drops = c('total_bedrooms', 'total_rooms')

housing = housing[ , !(names(housing) %in% drops)]

head(housing)

# RECOMMEDNDATION OF NOT HAVING TO USE LOOP
##https://stackoverflow.com/questions/59955941/groups-in-r-housing-data
# one hot encoder in R -- https://datatricks.co.uk/one-hot-encoding-in-r-three-simple-methods
#also at end if needed cbind

categories = unique(housing$ocean_proximity)
#split the categories off
cat_housing = data.frame(ocean_proximity = housing$ocean_proximity)

#help on rep fx
for(cat in categories){
  cat_housing[,cat] = rep(0, times= nrow(cat_housing))
}
head(cat_housing) #see the new columns on the right

for(i in 1:length(cat_housing$ocean_proximity)){
  cat = as.character(cat_housing$ocean_proximity[i])
  cat_housing[,cat][i] = 1
}

head(cat_housing)

#dropping the original Ocean Proximity

cat_columns = names(cat_housing)
keep_columns = cat_columns[cat_columns != 'ocean_proximity']
cat_housing = select(cat_housing,one_of(keep_columns))

tail(cat_housing)

#Scale the numerical variables
#Note here I scale every one of 
#the numericals except for 
#'median_house_value' as this is 
#what we will be working to predict. 
#The x values are scaled so that 
#coefficients in things like support 
#vector machines are given equal weight, 
#but the y value scale doen't affect the 
#learning algorithms in the same way 
#(and we would just need to re-scale 
#the predictions at the end which is 
#another hassle).


colnames(housing)

drops = c('ocean_proximity','median_house_value')
housing_num =  housing[ , !(names(housing) %in% drops)]


head(housing_num)

scaled_housing_num = scale(housing_num)

head(scaled_housing_num)

cleaned_housing = cbind(cat_housing, scaled_housing_num, median_house_value=housing$median_house_value)


head(cleaned_housing)

#Step 3. Create a test set of data
#We pull this subsection from the main 
#dataframe and put it to the side to not be looked at prior to testing our models. 
#Don't look at it, as #snooping the test data introduces a bias to your work!

#This is the data we use to validate our model, when we train a machine learning algorithm 
#the goal is usually to make an algorithm that predicts #well on data it hasn't seen before. 
#To assess this feature, we pull a set of data to validate the models as accurate/inaccurate 
#once we have #completed the training process.


set.seed(1738) # Set a random seed so that same sample can be reproduced in future runs

sample = sample.int(n = nrow(cleaned_housing), size = floor(.8*nrow(cleaned_housing)), replace = F)
train = cleaned_housing[sample, ] #just the samples
test  = cleaned_housing[-sample, ] #everything but the samples


#I like to use little sanity checks like the ones below 
#to make sure the manipulations have done what I want. 
#With big dataframes you need find ways to be sure that 
#don't involve looking at the whole thing every step!

#Note that the train data below has all the columns we want, 
#and also that the index is jumbled (so we did take a random sample). 
#The second check makes sure that the length of the train and test 
#dataframes equals the length of the dataframe they were split from, 
#which shows we didn't lose data or make any up by accident!

#has expected col and layout

head(train)

# total of training and test is equal to the number of observations

nrow(train) + nrow(test) == nrow(cleaned_housing)

#Step 4. Test some predictive models.
#We start here with just a simple linear model using 3 of the avaliable predictors. 
#Median income, total rooms and population. This serves as an entry point to 
#introduce the topic of cross validation and a basic model. We want a model 
#that makes good predictions on data that it has not seen before. A model 
#that explains the variation in the data it was trained on well, but does 
#not generalize to external data is referred to as being overfit. You may 
#thin "that's why we split off some test data!" but we don't want to repeatedly 
#assess against our test set, as then the model can just become overfit to that 
#set of data thus moving and not solving the problem.

#So here we do cross validation to test 
#the model using the training data itself. 
#Our K is 5, what this means is that the 
#training data is split into 5 equal portions. 
#One of the 5 folds is put to the side 
#(as a mini test data set) and then the model 
#is trained using the other 4 portions. 
#After that the predictions are made on the 
#folds that was withheld, and the process 
#is repeated for each of the 5 folds and 
#the average predictions produced from the 
#iterations of the model is taken. This 
#gives us a rough understanding of how well 
#the model predicts on external data!

library('boot')

?cv.glm # note the K option for K fold cross validation

glm_house = glm(median_house_value~median_income+mean_rooms+population, data=cleaned_housing)
k_fold_cv_error = cv.glm(cleaned_housing , glm_house, K=5)

k_fold_cv_error$delta


#The first component is the raw cross-validation 
#estimate of prediction error. The second component 
#is the adjusted cross-validation estimate.

glm_cv_rmse = sqrt(k_fold_cv_error$delta)[1]
glm_cv_rmse #off by about $83,000... it is a start


names(glm_house) #what parts of the model are callable?


glm_house$coefficients

#Since we scaled the imputs 
#we can say that of the three we looked at, 
#median income had the biggest effect on housing price... 
#but I'm always very careful and google lots before 
#intrepreting coefficients!


#Random forest model
library('randomForest')

?randomForest

names(train)


set.seed(1738)

train_y = train[,'median_house_value']
train_x = train[, names(train) !='median_house_value']

head(train_y)
head(train_x)


#some people like weird r format like this... I find it causes headaches
#rf_model = randomForest(median_house_value~. , data = train, ntree =500, importance = TRUE)

#original
#rf_model = randomForest(train_x, y = train_y , ntree = 500, importance = TRUE)

#revised to select 4 variables.
rf_model = randomForest(train_x, y = train_y , ntree = 500, mtry=4,importance = TRUE)


names(rf_model) #these are all the different things you can call from the model.

rf_model$importance



#Percentage included mean squared error 
#is a measure of feature importance. 
#It is defined as the measure of 
#the increase in mean squared error of 
#predictions when the given variable is shuffled, 
#thereby acting as a metric of that given 
#variable’s importance 
#in the performance of the model. 
#So higher number == more important predictor.

#The out-of-bag (oob) error estimate
#In random forests, there is no need 
#for cross-validation or a separate test 
#set to get an unbiased estimate of the 
#test set error. It is estimated internally, 
#during the run, as follows:
  
#Each tree is constructed using a different 
#bootstrap sample from the original data. 
#About one-third of the cases are left out 
#of the bootstrap sample and not 
#used in the construction of the 
#kth tree.

oob_prediction = predict(rf_model) #leaving out a data source forces OOB predictions

#you may have noticed that this is avaliable using the $mse in the model options.
#but this way we learn stuff!
train_mse = mean(as.numeric((oob_prediction - train_y)^2))
oob_rmse = sqrt(train_mse)
oob_rmse

#So even using a random forest 
#of only 1000 decision trees we are 
#able to predict the median price of a house 
#in a given district to within $49,000 
#of the actual median house price. 
#This can serve as our bechmark 
#moving forward and 
#trying other models.

#How well does the model 
#predict on the test data?
  
test_y = test[,'median_house_value']
test_x = test[, names(test) !='median_house_value']


y_pred = predict(rf_model , test_x)
test_mse = mean(((y_pred - test_y)^2))
test_rmse = sqrt(test_mse)
test_rmse

#Well that looks great! Our model scored 
#roughly the same on the training 
#and testing data, 
#suggesting that it is not overfit and 
#that it makes good predictions.


#Suggestions on ways to improve the results¶
#Why not use your R skills to build new data! 
#One suggestion would be to take the longitude 
#and latitude and work with these data. 
#You could try to find things like 'distance 
#to closest city with 1 million people' 
#or other location based stats. 
#This is called feature engineering 
#and data scientists get paid big bucks 
#to do it effectively!


