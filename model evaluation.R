library(dplyr)
library(lubridate)
library(janitor)

corn <- read.csv("predction_dataset.csv")
head(corn)

# Assuming 'sd' and 'silking_date' are in a suitable date format, if not, you'll need to convert them first
corn$sd <- as.Date(corn$sd, format = "%Y-%m-%d")
corn$silking_date <- as.Date(corn$silking_date, format = "%Y-%m-%d")

# Transform 'sd' and 'silking_date' to day of the year
corn$sd_day_of_year <- yday(corn$sd)
corn$silking_day_of_year <- yday(corn$silking_date)

# Check the changes
head(corn[c("sd", "sd_day_of_year", "silking_date", "silking_day_of_year")])
summary(corn)

#Model evaluation
library(caret)
library(randomForest)
library(gbm)
library(glmnet)
library(dplyr)

# Removing unnecessary columns
corn_data <- corn %>%
  select(
    -number,
    -year,
    -vom,
    -silking_date,
    -sd,
    -Sum_EPE_i_Month_1,
    -Sum_EPE_i_Month_2,
    -Sum_EPE_i_Month_3,
    -Sum_EPE_i_Month_4,
    -Sum_ETE_i_Month_1,
    -Sum_ETE_i_Month_2,
    -Sum_ETE_i_Month_3,
    -Sum_ETE_i_Month_4)

# Convert factors to numeric values for modeling (if not already numeric)
corn_data <- corn_data %>%
  mutate(across(where(is.factor), as.numeric))
summary(corn_data)

# Remove rows with NA in specified columns
corn_data <- corn_data %>%
  filter(!is.na(chu), !is.na(silk_chu), !is.na(total_chu),
         !is.na(sd_day_of_year), !is.na(silking_day_of_year))

summary(corn_data)
str(corn_data)

# Splitting data into training and testing sets
set.seed(123)  # for reproducibility
train_index <- createDataPartition(corn_data$silking_day_of_year, p = 0.8, list = FALSE)
train_data <- corn_data[train_index, ]
test_data <- corn_data[-train_index, ]

# Model 1: Random Forest
rf_model <- randomForest(silking_day_of_year ~ ., data = train_data, ntree = 500)
rf_predictions <- predict(rf_model, test_data)

# Model 2: Gradient Boosting Machine
gbm_model <- gbm(silking_day_of_year ~ ., data = train_data, distribution = "gaussian", n.trees = 500, interaction.depth = 4)
gbm_predictions <- predict(gbm_model, test_data, n.trees = 500)

# Model 3: Elastic Net
x_train <- model.matrix(silking_day_of_year ~ ., data = train_data)[, -1]  # removing intercept
y_train <- train_data$silking_day_of_year
x_test <- model.matrix(silking_day_of_year ~ ., data = test_data)[, -1]  # removing intercept

# Fitting Elastic Net model
cv_model <- cv.glmnet(x_train, y_train, alpha = 0.5)  # alpha = 0.5 for Elastic Net
best_lambda <- cv_model$lambda.min
enet_model <- glmnet(x_train, y_train, alpha = 0.5, lambda = best_lambda)
enet_predictions <- predict(enet_model, s = best_lambda, newx = x_test)

# Assessing model performance
rf_mse <- mean((rf_predictions - test_data$silking_day_of_year)^2)
gbm_mse <- mean((gbm_predictions - test_data$silking_day_of_year)^2)
enet_mse <- mean((enet_predictions - test_data$silking_day_of_year)^2)

print(list(
  RF_MSE = rf_mse,
  GBM_MSE = gbm_mse,
  ENet_MSE = enet_mse
))


#### assessing model performance.
library(caret)
library(randomForest)
library(gbm)
library(glmnet)

set.seed(123)  # For reproducibility
control <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 5,
  savePredictions = "final",
  summaryFunction = defaultSummary
)
#Random Forest
rf_model <- train(
  silking_day_of_year ~ ., 
  data = corn_data, 
  method = "rf",
  trControl = control,
  ntree = 500
)

#Gradient Boosting Machine (GBM)
gbm_model <- train(
  silking_day_of_year ~ ., 
  data = corn_data, 
  method = "gbm",
  trControl = control,
  verbose = FALSE,
  tuneGrid = expand.grid(interaction.depth = c(1, 5, 9),
                         n.trees = (1:3) * 50,
                         shrinkage = 0.1,
                         n.minobsinnode = 10)
)
#Elastic Net
enet_model <- train(
  silking_day_of_year ~ ., 
  data = corn_data, 
  method = "glmnet",
  trControl = control,
  tuneLength = 10
)

#Extract results and compute statistics.
# Summarize results
rf_results <- rf_model$results
gbm_results <- gbm_model$results
enet_results <- enet_model$results

# Compare models
comparison <- resamples(list(RandomForest = rf_model, GBM = gbm_model, ElasticNet = enet_model))
summary(comparison)
bwplot(comparison)

library(caret)
library(randomForest)
library(gbm)
library(glmnet)
library(dplyr)

# Assuming 'corn_data' is already preprocessed and ready for modeling
set.seed(123)  # for reproducibility

# Define the control function for repeated cross-validation
train_control <- trainControl(
  method = "repeatedcv", 
  number = 10,  # number of folds in k-fold cross-validation
  repeats = 5,  # number of repeats
  savePredictions = "final",
  summaryFunction = defaultSummary  # collects RMSE, Rsquared, and MAE
)

# Model 1: Random Forest
rf_model <- train(
  silking_day_of_year ~ ., 
  data = corn_data, 
  method = "rf",
  trControl = train_control,
  tuneLength = 5
)

# Model 2: GBM
gbm_model <- train(
  silking_day_of_year ~ ., 
  data = corn_data, 
  method = "gbm",
  trControl = train_control,
  verbose = FALSE,
  tuneGrid = expand.grid(.interaction.depth = c(1, 5, 9), 
                         .n.trees = c(100, 500),
                         .shrinkage = c(0.01, 0.1),
                         .n.minobsinnode = 10)
)

# Model 3: Elastic Net
enet_model <- train(
  silking_day_of_year ~ ., 
  data = corn_data, 
  method = "glmnet",
  trControl = train_control,
  tuneLength = 10
)

# Define common metrics
common_metrics <- c("RMSE", "Rsquared", "MAE")

# Combine results into one dataframe
combined_results <- rbind(
  transform(rf_model$results[, c(common_metrics)], Model = "RandomForest"),
  transform(gbm_model$results[, c(common_metrics)], Model = "GBM"),
  transform(enet_model$results[, c(common_metrics)], Model = "ElasticNet")
)

# Check results
print(combined_results)# Extracting results
results <- list(
  RandomForest = rf_model$results,
  GBM = gbm_model$results,
  ElasticNet = enet_model$results
)

# Check the combined data structure to ensure it is correct
str(combined_results)

# saving the results
library(tidyr)
library(ggplot2)

# Reshape data to long format, focusing on selected metrics
selected_long_results <- pivot_longer(
  combined_results,
  cols = c("RMSE", "Rsquared", "MAE"),  # Include only the desired metrics
  names_to = "Metric",
  values_to = "Value"
)
write.csv(selected_long_results, "ME.csv", row.names = FALSE)

data <- read.csv("ME.csv")
str(data)

library(ggplot2)

# Plot for RMSE
p_rmse <- ggplot(data[data$Metric == "RMSE", ], aes(x = Model, y = Value, fill = Model)) +
  geom_boxplot(outlier.shape = 16, outlier.size = 1, outlier.alpha = 0.3, width=0.5) +
  labs(title = "", x = "models", y = "RMSE (days)") +
  theme_bw() + 
  theme(legend.position="none") + scale_fill_brewer(palette = "Dark2")
ggsave(p_rmse, file="RMSE.tiff", width=4, height=4, dpi = 300)

# Plot for Rsquared
p_rsquared <- ggplot(data[data$Metric == "Rsquared", ], aes(x = Model, y = Value, fill = Model)) +
  geom_boxplot(outlier.shape = 16, outlier.size = 1, outlier.alpha = 0.4, width=0.5) +
  labs(title = "", x = "models", y = "coefficient of determination") +  
theme_bw() +
  theme(legend.position="none") + scale_fill_brewer(palette = "Dark2")
ggsave(p_rsquared, file="R2.tiff", width=4, height=4, dpi = 300)

# Plot for MAE
p_mae <- ggplot(data[data$Metric == "MAE", ], aes(x = Model, y = Value, fill = Model)) +
  geom_boxplot(outlier.shape = 16, outlier.size = 1, outlier.alpha = 0.3, width=0.5) +
  labs(title = "", x = "models", y = "MAE (days)") +
  theme_bw() +
  theme(legend.position="none") + scale_fill_brewer(palette = "Dark2")
ggsave(p_mae, file="MAE.tiff", width=4, height=4, dpi = 300)
# You can print each plot separately or arrange them together if needed
print(p_rmse)
print(p_rsquared)
print(p_mae)

#feature importance
# Install packages if not already installed
if (!require("caret")) install.packages("caret")
if (!require("randomForest")) install.packages("randomForest")
if (!require("gbm")) install.packages("gbm")
if (!require("glmnet")) install.packages("glmnet")
if (!require("vip")) install.packages("vip")
if (!require("doParallel")) install.packages("doParallel")

# Removing unnecessary columns
corn_data <- corn %>%
  select(
    -number,
    -year,
    -vom,
    -silking_date,
    -sd,
    -Sum_EPE_i_Month_1,
    -Sum_EPE_i_Month_2,
    -Sum_EPE_i_Month_3,
    -Sum_EPE_i_Month_4,
    -Sum_ETE_i_Month_1,
    -Sum_ETE_i_Month_2,
    -Sum_ETE_i_Month_3,
    -Sum_ETE_i_Month_4)

# Convert factors to numeric values for modeling (if not already numeric)
corn_data <- corn_data %>%
  mutate(across(where(is.factor), as.numeric))
summary(corn_data)

# Remove rows with NA in specified columns
corn_data <- corn_data %>%
  filter(!is.na(chu), !is.na(silk_chu), !is.na(total_chu),
         !is.na(sd_day_of_year), !is.na(silking_day_of_year))

summary(corn_data)
str(corn_data)

set.seed(123)  # For reproducibility

# (Re)create the random forest model if not already in the current session
if (!exists("rf_model")) {
  rf_model <- randomForest(silking_day_of_year ~ ., data = corn_data)
}

# Confirm that rf_model exists and its class
print(ls())
print(class(rf_model))

# Compute permutation feature importance
rf_perm <- vi_permute(
  rf_model,
  train = corn_data,
  target = "silking_day_of_year",
  metric = "rmse",
  nsim = 5,  # Number of permutations per feature (increase for more stable estimates)
  type = "difference",  # Use "difference" instead of "regression"
  pred_wrapper = function(object, newdata) predict(object, newdata)
)

# Permutation importance for the GBM model
set.seed(123)
gbm_model <- gbm(
  formula = silking_day_of_year ~ .,
  data = corn_data,
  distribution = "gaussian",  # use "gaussian" for regression
  n.trees = 100,              # adjust the number of trees as needed
  interaction.depth = 3       # adjust interaction depth as needed
)

gbm_perm <- vi_permute(
  gbm_model,
  train = corn_data,
  target = "silking_day_of_year",
  metric = "rmse",
  nsim = 5,
  type = "difference",  # "difference" is appropriate here
  pred_wrapper = function(object, newdata) predict(object, newdata)
)

# If needed, train the Elastic Net model
predictors <- corn_data[, setdiff(names(corn_data), "silking_day_of_year")]
x <- as.matrix(predictors)
y <- corn_data$silking_day_of_year

set.seed(123)
cv_fit <- cv.glmnet(x, y, alpha = 0.5)
elasticnet_model <- cv_fit  # store the model

# Compute permutation feature importance for the Elastic Net model
elasticnet_perm <- vi_permute(
  elasticnet_model,
  train = corn_data,
  target = "silking_day_of_year",
  metric = "rmse",
  nsim = 5,
  type = "difference",
  pred_wrapper = function(object, newdata) {
    newx <- as.matrix(newdata[, setdiff(names(newdata), "silking_day_of_year")])
    as.vector(predict(object, newx, s = "lambda.min"))
  }
)


# add a column with a plain character value
rf_perm$model <- "Random Forest"
gbm_perm$model <- "GBM"
elasticnet_perm$model <- "Elastic Net"

library(dplyr)
# Add a model column to each permutation importance data frame
rf_perm <- rf_perm %>% mutate(model = "Random Forest")
gbm_perm <- gbm_perm %>% mutate(model = "GBM")
elasticnet_perm <- elasticnet_perm %>% mutate(model = "Elastic Net")

# Combine the results into one data frame
importance_all <- bind_rows(rf_perm, gbm_perm, elasticnet_perm)
importance_all <- as.data.frame(importance_all)
importance_all$model <- factor(importance_all$model)

# Recode the feature names (Variable column) to your desired names
importance_all$Variable <- recode(importance_all$Variable,
                                  "chu"               = "hybrid maturity CHU",
                                  "silk_chu"          = "silking CHU",
                                  "total_chu"         = "growing season CHU",
                                  "Mean_PP_Month_1"   = "Jan PP",
                                  "Mean_PP_Month_2"   = "Feb PP",
                                  "Mean_PP_Month_3"   = "Mar PP",
                                  "Mean_PP_Month_4"   = "Apr PP",
                                  "Mean_PP_Month_5"   = "May PP",
                                  "Mean_PP_Month_6"   = "Jun PP",
                                  "Mean_PP_Month_7"   = "Jul PP",
                                  "Mean_Rad_Month_1"  = "Jan Rad",
                                  "Mean_Rad_Month_2"  = "Feb Rad",
                                  "Mean_Rad_Month_3"  = "Mar Rad",
                                  "Mean_Rad_Month_4"  = "Apr Rad",
                                  "Mean_Rad_Month_5"  = "May Rad",
                                  "Mean_Rad_Month_6"  = "Jun Rad",
                                  "Mean_Rad_Month_7"  = "Jul Rad",
                                  "Mean_SWE_Month_1"  = "Jan SW",
                                  "Mean_SWE_Month_2"  = "Feb SW",
                                  "Mean_SWE_Month_3"  = "Mar SW",
                                  "Mean_SWE_Month_4"  = "Apr SW",
                                  "Mean_SWE_Month_5"  = "May SW",
                                  "Mean_VPD_Month_1"  = "Jan VPD",
                                  "Mean_VPD_Month_2"  = "Feb VPD",
                                  "Mean_VPD_Month_3"  = "Mar VPD",
                                  "Mean_VPD_Month_4"  = "Apr VPD",
                                  "Mean_VPD_Month_5"  = "May VPD",
                                  "Mean_VPD_Month_6"  = "Jun VPD",
                                  "Mean_VPD_Month_7"  = "Jul VPD",
                                  "Mean_Tmean_Month_1"= "Jan Tmean",
                                  "Mean_Tmean_Month_2"= "Feb Tmean",
                                  "Mean_Tmean_Month_3"= "Mar Tmean",
                                  "Mean_Tmean_Month_4"= "Apr Tmean",
                                  "Mean_Tmean_Month_5"= "May Tmean",
                                  "Mean_Tmean_Month_6"= "Jun Tmean",
                                  "Mean_Tmean_Month_7"= "Jul Tmean",
                                  "Sum_EPE_i_Month_5" = "May EPE",
                                  "Sum_EPE_i_Month_6" = "Jun EPE",
                                  "Sum_EPE_i_Month_7" = "Jul EPE",
                                  "Sum_ETE_i_Month_5" = "May ETE",
                                  "Sum_ETE_i_Month_6" = "Jun ETE",
                                  "Sum_ETE_i_Month_7" = "Jul ETE",
                                  "sd_day_of_year"    = "Planting doy"
)

# Get the unique model names from your data
models <- unique(importance_all$model)

# Create an empty list to store plots
plots <- list()

# Loop over each model and create a plot
for (m in models) {
  plot_data <- importance_all %>% filter(model == m)
  p <- ggplot(plot_data, aes(x = reorder(Variable, Importance), y = Importance)) +
    geom_point(size = 2, color = "black") +
    geom_errorbar(aes(ymin = Importance - StDev, ymax = Importance + StDev), 
                  width = 0.2, color = "red4") +
    coord_flip() +
    labs(
      title = paste("Permutation Feature Importance -", m),
      x = "Feature",
      y = "Increase in RMSE"
    ) +
    theme_minimal()
  plots[[m]] <- p
}

# Display each plot separately
plots[["Random Forest"]]
plots[["GBM"]]
plots[["Elastic Net"]]


# Save each plot individually
ggsave(filename = "Permutation_Feature_Importance_RandomForest.tiff",
       plot = plots[["Random Forest"]],
       width = 8, height = 6, dpi = 300)

ggsave(filename = "Permutation_Feature_Importance_GBM.tiff",
       plot = plots[["GBM"]],
       width = 8, height = 6, dpi = 300)

ggsave(filename = "Permutation_Feature_Importance_ElasticNet.tiff",
       plot = plots[["Elastic Net"]],
       width = 8, height = 6, dpi = 300)
