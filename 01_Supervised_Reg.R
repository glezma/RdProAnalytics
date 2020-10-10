# 1) Fitting a linear model to in the mtcars data
# Fitting a linear model to the mtcars data
data(mtcars)

model <-lm(mpg ~ hp, mtcars[1:20,])

# Predict in-sample (Getting RMSE for the training sample)
predicted <- predict(
  model, mtcars[1:20,],type='response'
)

# Computing rmse
actual <- mtcars[1:20,'mpg']
sqrt(mean((predicted-actual)^2))


# 2) Fitting a linear model to in the diamonds data
# Fit lm model: model
model <- lm(price ~ . ,diamonds)

# Predict on full data: p
predicted <- predict(model,
                     diamonds, type='response')

# Compute errors: error
actual <- diamonds$price
errors = predicted-actual

# Calculate RMSE
sqrt(mean(errors^2))



data(mtcars)
model <- lm(mpg ~hp, mtcars[1:20,])

# Prediction out of sample 
predicted <- predict(
  model, mtcars[21:32,],type = "response"
)

# compite mse error
actual <- mtcars[21:32, "mpg"]
sqrt(mean((predicted - actual) ^ 2))

# ORDER A DF RANDOMLY
# Set seed
set.seed(42)


# Shuffle row indices: rows

rows <- sample(nrow(diamonds))

# Randomly order data
shuffled_diamonds <- diamonds[rows,]


# DO AN 80/20 SPLIT

# Determine row to split on: split
split <- round(nrow(diamonds) * 0.80)

# Create train
train <- diamonds[1:split, ]

# Create test
test <-diamonds[(split + 1):nrow(diamonds), ]


# PREDICT ON A TEST SET

# Fit lm model on train: model
model <- lm(price~.,train)

# Predict on test: p
p <- predict(model, test)

# Compute errors: error
error <- p-test$price

# Calculate RMSE
sqrt(mean(error^2))

library(caret)
# CROSS VALIDATION
set.seed(42)

# Fit linear  re gresssion model
model <- train(
  mpg ~ hp, mtcars, 
  method = 'lm',
  trControl = trainControl(
    method = 'cv',
    number = 10,
    verboseIter = TRUE
  )
)

model <- train(
  mpg ~ hp, 
  mtcars,
  method = "lm",
  trControl = trainControl(
    method = "repeatedcv", 
    number = 5,
    repeats = 5, 
    verboseIter = TRUE
  )
)
summary(model)

