#add install.packages code for all the packages
install.packages("dplyr")
library(dplyr)
library(tidyverse)
library(naniar)
library(purrr)
library(ggplot2)
#reading housing dataset
data <- read.csv('house-data.csv')
#dropping index column
data <- data[,-1]
attach(data)
#TASK-1 
# (Provide numerical and graphical summaries of the data set
# and make any initial comments that you deem appropriate.)

#descriptive analysis of data
summary(data)

# Get the count of categorical and numerical columns
cat_cols <- sapply(data, is.character)
num_cols <- sapply(data, is.numeric)
cat_count <- sum(cat_cols)
num_count <- sum(num_cols)
table_data <- data.frame(Category = c("Qualitative", "Quantitative"), Count = c(cat_count, num_count))
rownames(table_data) <- NULL
View(table_data)

#numeric columns in housing data
numeric_columns <- select_if(data, is.integer)
#house prices density plot based on overall quality and overall condition
ggplot(data, aes(x=OverallQual, y=SalePrice)) +
  geom_density_2d() +
  labs(x="Overall Quality", y="Sale Price") +
  ggtitle("Overall Quality vs Sale Price")

ggplot(data, aes(x=OverallCond, y=SalePrice)) +
  geom_density_2d() +
  labs(x="Overall Condition", y="Sale Price") +
  ggtitle("Overall Condition vs Sale Price")

dev.off()

#create the pair-plot of few important housing data features to check relationship
numeric_columns <- numeric_columns[, !(names(numeric_columns) %in% c("PoolArea", "MiscVal","MoSold","Fireplaces","FullBath","BedroomAbvGr", "KitchenAbvGr" ))]

pairs(numeric_columns, main = "Pairplot of the Data")
#neighborhood average sales price table
mean_table <- data %>% group_by(Neighborhood) %>% 
              summarise(MeanSalesPrice=mean(SalePrice),
              .groups = 'drop')
neighbor_sales_price <- mean_table %>% as.data.frame()
View(neighbor_sales_price)

#create a bar plot of neighbor_sales_price
# png("myplot.png", width = 8, height = 6, units = "in", res = 300)
# par(mar = c(5, 6, 4, 2) + 0.1)
barplot(neighbor_sales_price$MeanSalesPrice, names.arg = neighbor_sales_price$Neighborhood, xlab = "Neighborhood", ylab = "Mean Sale Price",cex.names = 0.6, las = 2, cex.axis = 0.6)
#data-cleaning of housing dataset
#checking proportion of missing values columns in dataset
apply(X = is.na(data), MARGIN = 2, FUN = mean)
#visualising missing values percentage
gg_miss_var(data, show_pct = TRUE)
#data-imputation for missing value columns 
data$Alley <- ifelse(is.na(Alley), "None", Alley)
data$MasVnrArea <- ifelse(is.na(data$MasVnrArea), 0, data$MasVnrArea)
data$BsmtQual <- ifelse(is.na(data$BsmtQual), "None", data$BsmtQual)
data$BsmtCond <- ifelse(is.na(data$BsmtCond), "None", data$BsmtCond)
data$GarageType <- ifelse(is.na(data$GarageType), "None", data$GarageType)
data$GarageCond <- ifelse(is.na(data$GarageCond), "None", data$GarageCond)
data$PoolQC <- ifelse(is.na(data$PoolQC), "None", data$PoolQC)
data$Fence <- ifelse(is.na(data$Fence), "None", data$Fence)
data$MiscFeature <- ifelse(is.na(data$MiscFeature), "None", data$MiscFeature)
#LotFrontage Column has approximately 18% missing values MCAR
#Firstly checking for normal distribution to use mean imputation
#Shapiro-Wilk Normality Test -> The test rejects the hypothesis of normality 
#when the p-value is less than or equal to 0.05.
shapiro.test(data$LotFrontage)
data <- data %>%
  group_by(Neighborhood) %>%
  mutate(LotFrontage = ifelse(is.na(LotFrontage), median(LotFrontage, na.rm = TRUE), LotFrontage)) %>%
  ungroup()
#checking if all missing values handled or not
gg_miss_var(data, show_pct = TRUE)

#Task 2a
#dividing houses based on their overall condition (OverallCond) as follows:
data <- data %>%
  mutate(OverallCond = case_when(
    OverallCond %in% 1:3 ~ "Poor",
    OverallCond %in% 4:6 ~ "Average",    
    OverallCond %in% 7:10 ~ "Good"
  ))
#converting categorical variables to factors in housing data
data <- data %>% mutate_if(is.character, as.factor)

str(data)
library(caret)
training <- caret::createDataPartition(data$OverallCond, p = 0.8, list = FALSE)
train_data <- data[training,]
test_data <- data[-training,]

logr_model <- nnet::multinom(OverallCond ~ ., data = train_data, family="binomial")

predicted_class <- predict(logr_model, test_data)
head(predicted_class)
summary(logr_model)

logr_conf_mat <- confusionMatrix(predicted_class, test_data$OverallCond)

ggplot(data = as.data.frame(logr_conf_mat$table), aes(x = Prediction, y = Reference, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "steelblue") +
  geom_text(aes(label = Freq), size = 16) +
  labs(x = "Predicted", y = "Actual", title = "Confusion Matrix")

logr_accuracy <- logr_conf_mat$overall["Accuracy"]*100
print(logr_accuracy)

#Task 2b

library(randomForest)
rfc_model <- randomForest(OverallCond~., data = train_data, ntree = 400, mtry = sqrt(ncol(train_data)))
summary(rfc_model)
predicted <- predict(rfc_model, test_data)
head(predicted)
rfc_conf_mat <- confusionMatrix(predicted, test_data$OverallCond)

ggplot(data = as.data.frame(rfc_conf_mat$table), aes(x = Prediction, y = Reference, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "steelblue") +
  geom_text(aes(label = Freq), size = 16) +
  labs(x = "Predicted", y = "Actual", title = "Confusion Matrix")

rfc_accuracy <- rfc_conf_mat$overall["Accuracy"]*100
print(rfc_accuracy)

#Task 3a

#detecting outliers in SalePrice
boxplot(SalePrice, main = "Boxplot of SalePrice")
dev.off()
# Calculate the lower and upper bounds for removing outliers
q1 <- quantile(data$SalePrice, 0.25)
q3 <- quantile(data$SalePrice, 0.75)
iqr <- q3 - q1
lower <- q1 - 1.5 * iqr
upper <- q3 + 1.5 * iqr

# Remove outliers based on the upper and lower bounds
clean_data <- subset(data, SalePrice >= lower & SalePrice <= upper)
training_obv <- caret::createDataPartition(clean_data$SalePrice, p=0.8, list = FALSE)
train_data_1 <- clean_data[training_obv,]
test_data_1 <- clean_data[-training_obv,]

rfr_model <- randomForest(SalePrice ~ ., data = train_data_1, ntree = 400, mtry = sqrt(ncol(train_data_1)))
summary(rfr_model)
predicted.randomForest <- predict(rfr_model, test_data_1)
head(predicted.randomForest)

rfr_accuracy <- R2(pred = predicted.randomForest, obs = test_data_1$SalePrice)
print(rfr_accuracy*100)

library(e1071)
svmr_model <- svm(SalePrice~., data=train_data_1)
predicted_values <- predict(svmr_model, test_data_1)

svm_accuracy <- R2(pred = predicted_values, obs = test_data_1$SalePrice)
print(svm_accuracy*100)

# task 3b

install.packages("doParallel")
library(doParallel)
library(boot)
num_cores <- detectCores()
registerDoParallel(cores=num_cores)
cv <- trainControl(method = "cv", number = 10)

# Define the models to fit
rf_model <- train(SalePrice ~ ., data = train_data_1, method = "rf", trControl = cv, importance = TRUE)
install.packages("kernlab")
library(kernlab)
svm_model <- train(SalePrice ~ ., data = train_data_1, method = "svmRadial", trControl = cv)

# Estimate test error using cross-validation with parallel processing
rf_error <- sqrt(mean((predict(rf_model, test_data_1) - test_data_1$SalePrice)^2))
svm_error <- sqrt(mean((predict(svm_model, test_data_1) - test_data_1$SalePrice)^2))

cat("Random Forest test error:", rf_error, "\n")
cat("SVM test error:", svm_error, "\n")

# Define the bootstrap resampling scheme with parallel processing
boot <- trainControl(method = "boot", number = 100, allowParallel = TRUE)

# Estimate test error using bootstrap resampling with parallel processing
set.seed(123)
rf_boot_error <- sqrt(mean((predict(rf_model, train_data_1[unlist(createResample(nrow(train_data_1), times = boot$number)),]) - train_data_1$SalePrice)^2))
svm_boot_error <- sqrt(mean((predict(svm_model, train_data_1[unlist(createResample(nrow(train_data_1), times = boot$number)),]) - train_data_1$SalePrice)^2))


cat("Random Forest bootstrap test error:", rf_boot_error, "\n")
cat("SVM bootstrap test error:", svm_boot_error, "\n")

# Stop parallel processing
stopImplicitCluster()


# task 4
# here we are researching on whether the year built of the house has any effect on the sale price or not.
# Null Hypothesis: year built of the house will have no effect on sale price
# Alternative Hypothesis: The year built of the house has a negative effect on the sale price.

linear_model <- lm(SalePrice ~ YearBuilt, data = train_data_1)

# Check the assumptions of the model
par(mfrow = c(2, 2))
plot(linear_model)

# Make predictions on the test data
lm_predictions <- predict(linear_model, newdata = test_data)

# Calculate the root mean squared error
lm_rmse <- RMSE(lm_predictions, test_data$SalePrice)
lm_rmse

# Fit a random forest model
rf_model <- train(SalePrice ~ YearBuilt, data = train_data, method = "rf")

# Make predictions on the test data
rf_predictions <- predict(rf_model, newdata = test_data)

# Calculate the root mean squared error
rf_rmse <- RMSE(rf_predictions, test_data$SalePrice)
rf_rmse

# Compare the root mean squared errors of the two models
lm_rmse
rf_rmse

# Conduct a t-test to compare the means of the two models
t_test <- t.test(lm_predictions, rf_predictions)
View(t_test)

# Conclusion: Based on the results of the t-test, we reject the null hypothesis that the two models have equal mean RMSEs. 
# The random forest model has a significantly lower RMSE than the linear regression model, suggesting that it is a better model for 
# predicting house prices based on the age of the house at the time of sale.

# The RMSE for the two models is significantly different, indicating that they have different levels of accuracy in predicting the sale price 
# of houses. However, the t-test results indicate that there is not a statistically significant difference between the mean predicted sale prices 
# of the two models. This suggests that while the models may have different levels of accuracy, their average predictions are not significantly 
# different from each other.

#<------------------------------------------End Here----------------------------------------------------->


