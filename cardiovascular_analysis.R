# Load the dataset
cardio_data <- read.csv(
  "cardio_train.csv",
  header = TRUE,
  sep = ";",
  stringsAsFactors = FALSE
)

# View basic dataset information
dim(cardio_data)
names(cardio_data)
head(cardio_data)
str(cardio_data)

# Check missing values
colSums(is.na(cardio_data))
sum(is.na(cardio_data))

# Remove missing values
cardio_clean <- na.omit(cardio_data)

# Check dataset size after removing missing values
dim(cardio_clean)

# Create target labels
cardio_clean$cardio_label <- factor(
  cardio_clean$cardio,
  levels = c(0, 1),
  labels = c("No cardiovascular disease", "Cardiovascular disease")
)

# Convert age from days into years
cardio_clean$age_years <- floor(cardio_clean$age / 365.25)

# Create age groups
cardio_clean$age_group <- cut(
  cardio_clean$age_years,
  breaks = c(29, 39, 49, 59, 69),
  labels = c("30-39", "40-49", "50-59", "60-69"),
  include.lowest = TRUE
)

# Create cholesterol labels
cardio_clean$cholesterol_label <- factor(
  cardio_clean$cholesterol,
  levels = c(1, 2, 3),
  labels = c("Normal", "Above normal", "Well above normal")
)

# Target variable visualisation
target_counts <- table(cardio_clean$cardio_label)

barplot(
  target_counts,
  main = "Cardiovascular Disease Distribution",
  xlab = "Cardiovascular disease status",
  ylab = "Number of patients",
  names.arg = names(target_counts)
)

# Relationship between age group and cardiovascular disease
age_relationship <- aggregate(
  cardio ~ age_group,
  data = cardio_clean,
  FUN = mean
)

age_relationship$percentage <- age_relationship$cardio * 100

barplot(
  age_relationship$percentage,
  names.arg = age_relationship$age_group,
  main = "Cardiovascular Disease Rate by Age Group",
  xlab = "Age group",
  ylab = "Cardiovascular disease rate (%)"
)

# Relationship between cholesterol and cardiovascular disease
cholesterol_relationship <- aggregate(
  cardio ~ cholesterol_label,
  data = cardio_clean,
  FUN = mean
)
 
cholesterol_relationship$percentage <-
  cholesterol_relationship$cardio * 100

barplot(
  cholesterol_relationship$percentage,
  names.arg = cholesterol_relationship$cholesterol_label,
  main = "Cardiovascular Disease Rate by Cholesterol Level",
  xlab = "Cholesterol level",
  ylab = "Cardiovascular disease rate (%)"
)



# Check and remove duplicate records


# Count duplicate rows
sum(duplicated(cardio_clean))

# Remove duplicate rows
cardio_clean <- cardio_clean[!duplicated(cardio_clean), ]

# Check the dataset size
dim(cardio_clean)



# Check unusual or incorrect values


# View summaries of important variables
summary(cardio_clean$height)
summary(cardio_clean$weight)
summary(cardio_clean$ap_hi)
summary(cardio_clean$ap_lo)

# Remove clearly unrealistic values
cardio_clean <- cardio_clean[
  cardio_clean$height >= 120 &
    cardio_clean$height <= 220 &
    cardio_clean$weight >= 30 &
    cardio_clean$weight <= 200 &
    cardio_clean$ap_hi >= 70 &
    cardio_clean$ap_hi <= 250 &
    cardio_clean$ap_lo >= 40 &
    cardio_clean$ap_lo <= 150 &
    cardio_clean$ap_hi > cardio_clean$ap_lo,
]

# Check the dataset size after removing unusual values
dim(cardio_clean)



# Calculate BMI


cardio_clean$BMI <- cardio_clean$weight /
  (cardio_clean$height / 100)^2

# View BMI summary
summary(cardio_clean$BMI)

# Remove unrealistic BMI values
cardio_clean <- cardio_clean[
  cardio_clean$BMI >= 10 &
    cardio_clean$BMI <= 60,
]

# Check the final dataset size
dim(cardio_clean)



# Descriptive statistics


# Summary of selected numerical variables
summary(
  cardio_clean[, c(
    "age_years",
    "height",
    "weight",
    "BMI",
    "ap_hi",
    "ap_lo"
  )]
)

# Mean values
mean(cardio_clean$age_years)
mean(cardio_clean$height)
mean(cardio_clean$weight)
mean(cardio_clean$BMI)
mean(cardio_clean$ap_hi)
mean(cardio_clean$ap_lo)

# Median values
median(cardio_clean$age_years)
median(cardio_clean$BMI)
median(cardio_clean$ap_hi)
median(cardio_clean$ap_lo)

# Standard deviation
sd(cardio_clean$age_years)
sd(cardio_clean$BMI)
sd(cardio_clean$ap_hi)
sd(cardio_clean$ap_lo)



# BMI and cardiovascular disease


boxplot(
  BMI ~ cardio_label,
  data = cardio_clean,
  main = "BMI by Cardiovascular Disease Status",
  xlab = "Cardiovascular disease status",
  ylab = "BMI"
)



# Systolic blood pressure and cardiovascular disease


boxplot(
  ap_hi ~ cardio_label,
  data = cardio_clean,
  main = "Systolic Blood Pressure by Disease Status",
  xlab = "Cardiovascular disease status",
  ylab = "Systolic blood pressure"
)



# Physical activity and cardiovascular disease


activity_table <- table(
  cardio_clean$active,
  cardio_clean$cardio_label
)

barplot(
  activity_table,
  beside = TRUE,
  main = "Physical Activity and Cardiovascular Disease",
  xlab = "Cardiovascular disease status",
  ylab = "Number of patients",
  legend.text = c("Not active", "Active"),
  args.legend = list(title = "Physical activity")
)



# Smoking and cardiovascular disease


smoking_table <- table(
  cardio_clean$smoke,
  cardio_clean$cardio_label
)

barplot(
  smoking_table,
  beside = TRUE,
  main = "Smoking and Cardiovascular Disease",
  xlab = "Cardiovascular disease status",
  ylab = "Number of patients",
  legend.text = c("Non-smoker", "Smoker"),
  args.legend = list(title = "Smoking status")
)



# Correlation analysis


numeric_data <- cardio_clean[, c(
  "age_years",
  "height",
  "weight",
  "BMI",
  "ap_hi",
  "ap_lo",
  "cholesterol",
  "gluc",
  "smoke",
  "alco",
  "active",
  "cardio"
)]

# Calculate correlations
correlation_matrix <- cor(
  numeric_data,
  use = "complete.obs"
)

# Display correlation values
round(correlation_matrix, 2)



library(corrplot)

# Create correlation heatmap
corrplot(
  correlation_matrix,
  method = "color",
  type = "upper",
  addCoef.col = "black",
  number.cex = 0.6,
  tl.cex = 0.8
)



# Statistical tests


# Cholesterol and cardiovascular disease
cholesterol_test <- chisq.test(
  table(
    cardio_clean$cholesterol,
    cardio_clean$cardio
  )
)

cholesterol_test


# Smoking and cardiovascular disease
smoking_test <- chisq.test(
  table(
    cardio_clean$smoke,
    cardio_clean$cardio
  )
)

smoking_test


# Physical activity and cardiovascular disease
activity_test <- chisq.test(
  table(
    cardio_clean$active,
    cardio_clean$cardio
  )
)

activity_test


# Glucose and cardiovascular disease
glucose_test <- chisq.test(
  table(
    cardio_clean$gluc,
    cardio_clean$cardio
  )
)

glucose_test


# Compare BMI between the two target groups
bmi_test <- t.test(
  BMI ~ cardio,
  data = cardio_clean
)

bmi_test


# Compare systolic blood pressure
blood_pressure_test <- t.test(
  ap_hi ~ cardio,
  data = cardio_clean
)

blood_pressure_test



# Prepare data for machine learning


# Convert categorical variables into factors
cardio_clean$gender <- factor(cardio_clean$gender)
cardio_clean$cholesterol <- factor(cardio_clean$cholesterol)
cardio_clean$gluc <- factor(cardio_clean$gluc)
cardio_clean$smoke <- factor(cardio_clean$smoke)
cardio_clean$alco <- factor(cardio_clean$alco)
cardio_clean$active <- factor(cardio_clean$active)

# Create a simple target factor
cardio_clean$target <- factor(
  cardio_clean$cardio,
  levels = c(0, 1),
  labels = c("No", "Yes")
)

# Select variables for machine learning
model_data <- cardio_clean[, c(
  "age_years",
  "gender",
  "BMI",
  "ap_hi",
  "ap_lo",
  "cholesterol",
  "gluc",
  "smoke",
  "alco",
  "active",
  "target"
)]

# Check the modelling data
head(model_data)
str(model_data)



# Split data into training and testing sets


# Set seed to make results repeatable
set.seed(123)

# Select 80% of rows for training
training_rows <- sample(
  1:nrow(model_data),
  size = 0.80 * nrow(model_data)
)

# Create training and testing datasets
train_data <- model_data[training_rows, ]
test_data <- model_data[-training_rows, ]

# Check dataset sizes
dim(train_data)
dim(test_data)

# Check target distribution
prop.table(table(train_data$target))
prop.table(table(test_data$target))



# Logistic Regression


logistic_model <- glm(
  target ~ age_years + gender + BMI +
    ap_hi + ap_lo + cholesterol + gluc +
    smoke + alco + active,
  data = train_data,
  family = binomial
)

# View model results
summary(logistic_model)

# Generate probability predictions
logistic_probability <- predict(
  logistic_model,
  newdata = test_data,
  type = "response"
)

# Convert probability into Yes or No
logistic_prediction <- ifelse(
  logistic_probability >= 0.50,
  "Yes",
  "No"
)

logistic_prediction <- factor(
  logistic_prediction,
  levels = c("No", "Yes")
)

# Logistic Regression confusion matrix
logistic_matrix <- table(
  Actual = test_data$target,
  Predicted = logistic_prediction
)

logistic_matrix



# Logistic Regression evaluation metrics


logistic_TN <- logistic_matrix["No", "No"]
logistic_FP <- logistic_matrix["No", "Yes"]
logistic_FN <- logistic_matrix["Yes", "No"]
logistic_TP <- logistic_matrix["Yes", "Yes"]

logistic_accuracy <- (
  logistic_TP + logistic_TN
) / sum(logistic_matrix)

logistic_precision <- logistic_TP / (
  logistic_TP + logistic_FP
)

logistic_recall <- logistic_TP / (
  logistic_TP + logistic_FN
)

logistic_f1 <- 2 * (
  logistic_precision * logistic_recall
) / (
  logistic_precision + logistic_recall
)

logistic_accuracy
logistic_precision
logistic_recall
logistic_f1



# Decision Tree model


# The rpart package is normally available with R
library(rpart)

decision_tree_model <- rpart(
  target ~ age_years + gender + BMI +
    ap_hi + ap_lo + cholesterol + gluc +
    smoke + alco + active,
  data = train_data,
  method = "class",
  control = rpart.control(
    cp = 0.01,
    minsplit = 20
  )
)

# Display the Decision Tree
plot(
  decision_tree_model,
  uniform = TRUE,
  margin = 0.1
)

text(
  decision_tree_model,
  use.n = TRUE,
  all = TRUE,
  cex = 0.7
)

# Generate Decision Tree predictions
tree_prediction <- predict(
  decision_tree_model,
  newdata = test_data,
  type = "class"
)

# Decision Tree confusion matrix
tree_matrix <- table(
  Actual = test_data$target,
  Predicted = tree_prediction
)

tree_matrix



# Decision Tree evaluation metrics


tree_TN <- tree_matrix["No", "No"]
tree_FP <- tree_matrix["No", "Yes"]
tree_FN <- tree_matrix["Yes", "No"]
tree_TP <- tree_matrix["Yes", "Yes"]

tree_accuracy <- (
  tree_TP + tree_TN
) / sum(tree_matrix)

tree_precision <- tree_TP / (
  tree_TP + tree_FP
)

tree_recall <- tree_TP / (
  tree_TP + tree_FN
)

tree_f1 <- 2 * (
  tree_precision * tree_recall
) / (
  tree_precision + tree_recall
)

tree_accuracy
tree_precision
tree_recall
tree_f1



# Random Forest model



library(randomForest)

set.seed(123)

random_forest_model <- randomForest(
  target ~ age_years + gender + BMI +
    ap_hi + ap_lo + cholesterol + gluc +
    smoke + alco + active,
  data = train_data,
  ntree = 100,
  importance = TRUE
)

# View model details
random_forest_model

# Generate Random Forest predictions
forest_prediction <- predict(
  random_forest_model,
  newdata = test_data
)

# Random Forest confusion matrix
forest_matrix <- table(
  Actual = test_data$target,
  Predicted = forest_prediction
)

forest_matrix



# Random Forest evaluation metrics


forest_TN <- forest_matrix["No", "No"]
forest_FP <- forest_matrix["No", "Yes"]
forest_FN <- forest_matrix["Yes", "No"]
forest_TP <- forest_matrix["Yes", "Yes"]

forest_accuracy <- (
  forest_TP + forest_TN
) / sum(forest_matrix)

forest_precision <- forest_TP / (
  forest_TP + forest_FP
)

forest_recall <- forest_TP / (
  forest_TP + forest_FN
)

forest_f1 <- 2 * (
  forest_precision * forest_recall
) / (
  forest_precision + forest_recall
)

forest_accuracy
forest_precision
forest_recall
forest_f1



# Random Forest feature importance


importance(random_forest_model)

varImpPlot(
  random_forest_model,
  main = "Random Forest Feature Importance"
)



# Compare all three models


model_comparison <- data.frame(
  Model = c(
    "Logistic Regression",
    "Decision Tree",
    "Random Forest"
  ),
  Accuracy = c(
    logistic_accuracy,
    tree_accuracy,
    forest_accuracy
  ),
  Precision = c(
    logistic_precision,
    tree_precision,
    forest_precision
  ),
  Recall = c(
    logistic_recall,
    tree_recall,
    forest_recall
  ),
  F1_Score = c(
    logistic_f1,
    tree_f1,
    forest_f1
  )
)

# Round results to three decimal places
model_comparison[, 2:5] <- round(
  model_comparison[, 2:5],
  3
)

# Display model comparison
model_comparison



# Compare model accuracy visually


barplot(
  model_comparison$Accuracy,
  names.arg = model_comparison$Model,
  main = "Accuracy Comparison of Machine Learning Models",
  xlab = "Machine-learning model",
  ylab = "Accuracy",
  ylim = c(0, 1),
  las = 2
)



# ROC curve and AUC




library(pROC)

# Logistic Regression ROC
logistic_roc <- roc(
  test_data$target,
  logistic_probability,
  levels = c("No", "Yes")
)

# Decision Tree probabilities
tree_probability <- predict(
  decision_tree_model,
  newdata = test_data,
  type = "prob"
)[, "Yes"]

tree_roc <- roc(
  test_data$target,
  tree_probability,
  levels = c("No", "Yes")
)

# Random Forest probabilities
forest_probability <- predict(
  random_forest_model,
  newdata = test_data,
  type = "prob"
)[, "Yes"]

forest_roc <- roc(
  test_data$target,
  forest_probability,
  levels = c("No", "Yes")
)

# Display AUC values
auc(logistic_roc)
auc(tree_roc)
auc(forest_roc)

# Plot the ROC curves
plot(
  logistic_roc,
  main = "ROC Curves for the Three Models"
)

plot(
  tree_roc,
  add = TRUE
)

plot(
  forest_roc,
  add = TRUE
)

legend(
  "bottomright",
  legend = c(
    "Logistic Regression",
    "Decision Tree",
    "Random Forest"
  ),
  lty = 1
)