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
