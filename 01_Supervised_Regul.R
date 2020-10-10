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
# Ridge regression

# Find the best lambda using cross-validation
set.seed(123) 
cv <- cv.glmnet(x, y, alpha = 0)
# Display the best lambda value
cv$lambda.min

# Fit the final model on the training data
model <- glmnet(x, y, alpha = 0, lambda = cv$lambda.min)
# Display regression coefficients
coef(model)

# Make predictions on the test data
x.test <- model.matrix(medv ~., test.data)[,-1]
predictions <- model %>% predict(x.test) %>% as.vector()
# Model performance metrics
data.frame(
  RMSE = RMSE(predictions, test.data$medv),
  Rsquare = R2(predictions, test.data$medv)
)
