data(Sonar)

# Shuffle row indices: permuted_rows
permuted_rows <- sample(nrow(Sonar))

# Randomly order data: Sonar
Sonar_shuffled <- Sonar[permuted_rows,]

# Identify row to split on: split
split <- round(n_obs * 0.6)

# Create train
train <- Sonar_shuffled[1:split,]

# Create test
test <- Sonar_shuffled[(split+1):n_obs,]

# Fit glm model: model
model <-glm(Class ~.,family = "binomial", train)

# Predict on test: p
p<-predict(model, test, type = "response")


# CONFUSUION MATRIX
# If p exceeds threshold of 0.5, M else R: m_or_r 
m_or_r <- ifelse(p>0.5,"M","R")
# Convert to factor: p_class
p_class <- factor(m_or_r,levels = levels(test[['Class']]))
# Create confusion matrix
confusionMatrix(p_class,test[["Class"]])


