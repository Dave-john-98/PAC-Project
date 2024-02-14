# ensure analysisData.csv and scoringData.csv are in your working directory

# following code will read data
data = read.csv('/Users/dave.john.98/Desktop/APAN Fall 23/APAN5200 Frameworks I/PAC/analysisData.csv')


str(data)
summary(data)
sum(is.na(data))

library(dplyr)
library(purrr)
library(stringr)
library(ggplot2)
library(scales)

#Distribution of Car Prices - Histogram
ggplot(data, aes(x = price)) + 
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  scale_x_continuous(labels = comma) +  # Formats the x-axis labels to display actual numbers
  theme_minimal() +
  labs(title = "Distribution of Car Prices", x = "Price", y = "Count")

# Price vs. Mileage
ggplot(data, aes(x = mileage, y = price)) + 
  geom_point(alpha = 0.5) +
  scale_x_continuous(labels = comma) +  # Formats the x-axis labels to display actual numbers
  scale_y_continuous(labels = comma) +  # Formats the y-axis labels to display actual numbers   
  theme_minimal() +
  labs(title = "Car Price vs. Mileage", x = "Mileage", y = "Price")

# Price vs. Year
ggplot(data, aes(x = year, y = price)) + 
  geom_point(alpha = 0.5) +
  scale_y_continuous(labels = comma) +  # Formats the y-axis labels to display actual numbers
  theme_minimal() +
  labs(title = "Car Price vs. Year", x = "Year", y = "Price")

# Price vs. Horsepower
ggplot(data, aes(x = horsepower, y = price)) + 
  geom_point(alpha = 0.5) +
  scale_y_continuous(labels = comma) +  # Formats the y-axis labels to display actual numbers
  theme_minimal() +
  labs(title = "Car Price vs. Horsepower", x = "Horsepower", y = "Price")


# Create a function that can calculate mode
getmode <- function(v) {
  uniqv <- unique(na.omit(v))
  return(uniqv[which.max(tabulate(match(v, uniqv)))])}

data_clean <- data %>%
  # Impute columns to not have blank fields throughout the dataset, changed to NA
  mutate_if(is.character, ~na_if(trimws(.), "")) %>% 
  # Impute Numerical columns that have NA values to mean of their respective columns
  mutate_if(is.numeric, ~ifelse(is.na(.), mean(., na.rm = TRUE), .)) %>%
  # Transform the character columns to logical columns if it's only TRUE, FALSE, or NA values 
  mutate_if(~is.character(.) && all(na.omit(.) %in% c("TRUE", "FALSE")), as.logical) %>% 
  # Impute Logical columns that have NA values to say "FALSE"
  mutate_if(is.logical, ~ifelse(is.na(.), FALSE, .)) %>% 
  # Streamlined the engine_type column to only show the first word in each field
  mutate(engine_type = str_split(engine_type, " ", simplify = TRUE)[, 1]) %>% 
  # Impute Character/Categorical columns that have NA values to mode of their respective columns
  mutate_if(is.character, ~ifelse(is.na(.), getmode(.), .)) 


str(data_clean)
sum(is.na(data_clean))

# read in scoring data and apply model to generate predictions
scoringData = read.csv('/Users/dave.john.98/Desktop/APAN Fall 23/APAN5200 Frameworks I/PAC/scoringData.csv')

#Clean the dataset as you did with the analysis_data
scoringData_clean <- scoringData %>%
  mutate_if(is.character, ~na_if(trimws(.), "")) %>%
  mutate_if(is.numeric, ~ifelse(is.na(.), mean(., na.rm = TRUE), .)) %>%
  mutate_if(~is.character(.) && all(na.omit(.) %in% c("TRUE", "FALSE")), as.logical) %>%
  mutate_if(is.logical, ~ifelse(is.na(.), FALSE, .)) %>%
  mutate(engine_type = str_split(engine_type, " ", simplify = TRUE)[, 1]) %>%
  mutate_if(is.character, ~ifelse(is.na(.), getmode(.), .))

install.packages('xgboost')
install.packages('vtreat')

library(xgboost)
library(vtreat)

#construct a  XGBOOST model
model = designTreatmentsZ(dframe = data_clean,
                          varlist =  names(data_clean)[c(2,5,7,8,9, 12,14, 16,23:25, 31,34:36,39,42,44,45)])
newvars = model$scoreFrame[model$scoreFrame$code%in% c('clean','lev'),'varName']

model_input = prepare(treatmentplan = model,
                      dframe = data_clean,
                      varRestriction = newvars)
model_boost = xgboost(data=as.matrix(model_input),
                      label = data_clean$price,
                      nrounds=10000,
                      verbose = 0,
                      early_stopping_rounds = 100)

model_boost$best_iteration

score_input = prepare(treatmentplan = model,
                      dframe = scoringData_clean,
                      varRestriction = newvars)

summary(model)


pred = predict(model_boost,newdata=as.matrix(score_input))

# construct submission from predictions
submissionFile = data.frame(id = scoringData_clean$id, pred_price = pred)
write.csv(submissionFile, 'submission_9.csv',row.names = F)

summary(submissionFile)


# Merging the two dataframes on 'id' or the appropriate key
results <- merge(submissionFile, data, by = "id")

# Calculating RMSE
rmse <- sqrt(mean((results$pred_price - results$price)^2))
print(rmse)

# Make sure to change 'pred_price' to 'price' as the column header in the Excel file before submitting on Kaggle


