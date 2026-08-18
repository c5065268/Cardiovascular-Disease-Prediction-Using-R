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
