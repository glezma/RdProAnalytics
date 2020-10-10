
library(tidyverse)
library(caret)
library(glmnet)

set.seed(45)
data("Boston", package = "MASS")

dat <- Boston

index = sample(1:nrow(dat),0.7*nrow(dat))
train.data.raw  = dat[index, ]
test.data.raw = dat[-index,]

pre_proc_val <- preProcess(train.data.raw, method = c("center", "scale"))

train.data = predict(pre_proc_val, train.data.raw)
test.data = predict(pre_proc_val, test.data.raw)


# Predictor variables
x <- model.matrix(medv~., train.data)[,-1]
# Outcome variable
y <- train.data$medv


# 1 Ridge Regression

# Find the best lambda using cross-validation
set.seed(123) 
cv <- cv.glmnet(x, y, alpha = 0)
# Display the best lambda value
cv$lambda.min
# Fit the final model on the training data
model <- glmnet(x, y, alpha = 0, lambda = cv$lambda.min)
# Display regression coefficients
# coef(model)

# Make predictions on the test data
x.test <- model.matrix(medv ~., test.data)[,-1]
predictions <- model %>% predict(x.test) %>% as.vector()
# Model performance metrics on the test data
data.frame(
  RMSE = RMSE(predictions, test.data$medv),
  Rsquare = R2(predictions, test.data$medv)
)

# 2 Lasso

# Find the best lambda using cross-validation
set.seed(123) 
cv <- cv.glmnet(x, y, alpha = 1)
# Display the best lambda value
cv$lambda.min
# Fit the final model on the training data
model <- glmnet(x, y, alpha = 1, lambda = cv$lambda.min)
# Dsiplay regression coefficients
# coef(model)

# Make predictions on the test data
x.test <- model.matrix(medv ~., test.data)[,-1]
predictions <- model %>% predict(x.test) %>% as.vector()
# Model performance metrics
data.frame(
  RMSE = RMSE(predictions, test.data$medv),
  Rsquare = R2(predictions, test.data$medv)
)


# 3 Elastic NEt
# Build the model using the training set
set.seed(123)
model <- train(
  medv ~., data = train.data, method = "glmnet",
  trControl = trainControl("cv", number = 10),
  tuneLength = 10
)
# Best tuning parameter
model$bestTune

# Coefficient of the final model. You need
# to specify the best lambda
# coef(model$finalModel, model$bestTune$lambda)

#Make predictions on the test data
x.test <- model.matrix(medv ~., test.data)[,-1]
predictions <- model %>% predict(x.test)
# Model performance metrics
data.frame(
  RMSE = RMSE(predictions, test.data$medv),
  Rsquare = R2(predictions, test.data$medv)
)

# Comparison

# Comparing all model.response(
lambda <- 10^seq(2, 3, length = 10)


# Build the model
set.seed(123)
ridge <- train(
  medv ~., data = train.data, method = "glmnet",
  trControl = trainControl("cv", number = 10),
  tuneGrid = expand.grid(alpha = 0, lambda = lambda)
)

# LASSO

# Build the model
set.seed(123)
lasso <- train(
  medv ~., data = train.data, method = "glmnet",
  trControl = trainControl("cv", number = 10),
  tuneGrid = expand.grid(alpha = 1, lambda = lambda)
)

# Build the model
set.seed(123)
elastic <- train(
  medv ~., data = train.data, method = "glmnet",
  trControl = trainControl("cv", number = 10),
  tuneLength = 10
)

models <- list(ridge = ridge, lasso = lasso, elastic = elastic)
resamples(models) %>% summary( metric = "RMSE")


