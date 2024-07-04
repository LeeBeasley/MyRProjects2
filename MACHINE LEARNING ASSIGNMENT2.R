#Loading libraries
library(tidyverse)
library(reshape2)

#Loading data
housing = read.csv('housing.csv')
head(housing)

#Summarizing Data
summary(housing)

#organizing categories
par(mfrow=c(2,5))
colnames(housing)

# Basic plot of housing data

ggplot(housing, aes(x = longitude, y = latitude, color = median_house_value, size = population)) +
  geom_point(alpha = 0.5) +  # Adjust alpha to ensure points are not too overwhelming
  scale_color_viridis_c() +  # Color scale for house values
  scale_size(range = c(1, 10), name = "Population") +  # Adjust the range based on your population data scale
  labs(title = "Geographical Distribution of Median House Value",
       subtitle = "Point size represents population size",
       x = "Longitude",
       y = "Latitude",
       color = "Median House Value") +
  theme_minimal() +
  theme(legend.position = "right")

#Imputing missing values
housing$total_bedrooms[is.na(housing$total_bedrooms)] = median(housing$total_bedrooms , na.rm = TRUE)

#Fixing colums by making them means

housing$mean_bedrooms = housing$total_bedrooms/housing$households
housing$mean_rooms = housing$total_rooms/housing$households

drops = c('total_bedrooms', 'total_rooms')

housing = housing[ , !(names(housing) %in% drops)]
head(housing)

#Turn categoricals into booleans
categories = unique(housing$population)

#split the categories off
cat_housing = data.frame(population = housing$population)

housing$population_category <- cut(housing$population, 
                                   breaks = quantile(housing$population, 
                                                     probs = seq(0, 1, length.out = 6), # Creating 5 bins
                                                     na.rm = TRUE),
                                   include.lowest = TRUE,
                                   labels = FALSE)

# Get unique categories
categories <- unique(housing$population_category)

# Converting categories to character if not already, ensuring valid column names
categories <- paste("PopCat", categories, sep="_")

# Initialize cat_housing with columns for each category, all set to 0
cat_housing <- data.frame(matrix(0, nrow = nrow(housing), ncol = length(categories)))
names(cat_housing) <- categories

# Use the loop to populate 'cat_housing'
for (cat in categories) {
  cat_housing[, cat] <- rep(0, times = nrow(cat_housing))
}

print(categories)

str(cat_housing)

head(cat_housing)

library(dplyr)

# Assume cat_housing is already defined and loaded
cat_columns = names(cat_housing)
keep_columns = cat_columns[cat_columns != 'population']  # Change 'ocean_proximity' to 'population'
cat_housing = select(cat_housing, one_of(keep_columns))

#Generalizing column for removal 

column_to_remove = 'population'  # Can set this to any column name as needed

cat_columns = names(cat_housing)
keep_columns = cat_columns[cat_columns != column_to_remove]
cat_housing = select(cat_housing, one_of(keep_columns))

colnames(housing)

drops = c('population','median_house_value')
housing_num =  housing[ , !(names(housing) %in% drops)]

head(housing_num)

numeric_columns <- sapply(housing_num, is.numeric)
housing_num_numeric <- housing_num[, numeric_columns]
scaled_housing_num <- scale(housing_num_numeric)

# Checking the first few rows of the scaled data
head(scaled_housing_num)

housing_num_non_numeric <- housing_num[, !numeric_columns]

# Combine scaled numeric data with non-numeric data
housing_combined <- cbind(as.data.frame(scaled_housing_num), housing_num_non_numeric)

cleaned_housing = cbind(cat_housing, scaled_housing_num, median_house_value=housing$median_house_value)

head(cleaned_housing)

#Creating a test of data

set.seed(1738) # Set a random seed so that same sample can be reproduced in future runs

sample = sample.int(n = nrow(cleaned_housing), size = floor(.8*nrow(cleaned_housing)), replace = F)
train = cleaned_housing[sample, ] #just the samples
test  = cleaned_housing[-sample, ] #everything but the samples

head(train)

nrow(train) + nrow(test) == nrow(cleaned_housing)

#testing some predictive models

library('boot')
?cv.glm 

glm_house = glm(median_house_value~median_income+mean_rooms+population, data=cleaned_housing)
k_fold_cv_error = cv.glm(cleaned_housing , glm_house, K=5)

if (!require(boot)) {
  install.packages("boot", dependencies = TRUE)
}

library('boot')

k_fold_cv_error = cv.glm(cleaned_housing, glm_house, K=5)

glm_house = glm(median_house_value~median_income+population, data=cleaned_housing)
k_fold_cv_error = cv.glm(cleaned_housing , glm_house, K=5)

k_fold_cv_error$delta

glm_cv_rmse = sqrt(k_fold_cv_error$delta)[1]
glm_cv_rmse

names(glm_house) #what parts of the model are callable?

glm_house$coefficients

#Random forest model
library('randomForest')

?randomForest

names(train)

set.seed(1738)

train_y = train[,'median_house_value']
train_x = train[, names(train) !='median_house_value']

head(train_y)
head(train_x)

rf_model$importance

oob_prediction = predict(rf_model) #leaving out a data source forces OOB predictions

train_mse = mean(as.numeric((oob_prediction - train_y)^2))
oob_rmse = sqrt(train_mse)
oob_rmse

test_y = test[,'median_house_value']
test_x = test[, names(test) !='median_house_value']


y_pred = predict(rf_model , test_x)
test_mse = mean(((y_pred - test_y)^2))
test_rmse = sqrt(test_mse)
test_rmse
