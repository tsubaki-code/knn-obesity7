# Load the necessary libraries
rm(list = ls()) 
#install.packages('corrplot')
library(class)
library(dplyr)
library(caret)
library(cvms)
library(randomForest)
library(e1071)
library(rpart)
library(caTools)
library(FNN) 
library(gmodels) 
library(psych)
#import .csv data
data <- read.csv("data.csv", header = TRUE)
summary(data)
str(data)

#names of attributes
attributes <- names(data)
print(attributes)

#check missing value
missing_values <- sapply(data, function(x) sum(is.na(x)))
print(missing_values)

######data preprocessing######
unique_words_Age<- unique(data$Age)
print(unique_words_Age)
Age_min <- min(unique_words_Age)
Age_max <- max(unique_words_Age)
print(paste("Range of Age:", Age_min, "-", Age_max))
data$Age <- round(data$Age)
unique_words <- unique(data$CAEC)
print(unique_words)
unique_words_FCVC <- unique(data$FCVC)
print(unique_words_FCVC)
fcvc_min <- min(unique_words_FCVC)
fcvc_max <- max(unique_words_FCVC)
print(paste("Range of FCVC:", fcvc_min, "-", fcvc_max))

#organize demical to integer
data$FCVC <- round(data$FCVC)
unique_words_FCVC <- unique(data$FCVC)
print(unique_words_FCVC)
data$FCVC <- round(data$FCVC)
unique_words_NCP <- unique(data$NCP)
print(unique_words_NCP)
NCP_min <- min(unique_words_NCP)
NCP_max <- max(unique_words_NCP)
print(paste("Range of NCP:", NCP_min, "-", NCP_max))

#organize demical to integer
data$NCP <- round(data$NCP)
unique_words_NCP <- unique(data$NCP)
print(unique_words_NCP)
data$NCP <- round(data$NCP)
unique_words_CH2O <- unique(data$CH2O)
print(unique_words_CH2O)
CH2O_min <- min(unique_words_CH2O)
CH2O_max <- max(unique_words_CH2O)
print(paste("Range of CH2O:", CH2O_min, "-", CH2O_max))

#organize demical to integer
data$CH2O <- round(data$CH2O)
unique_words_CH2O <- unique(data$CH2O)
print(unique_words_CH2O)
data$CH2O <- round(data$CH2O)
unique_words_FAF <- unique(data$FAF)
print(unique_words_FAF)
FAF_min <- min(unique_words_FAF)
FAF_max <- max(unique_words_FAF)
print(paste("Range of FAF:", FAF_min, "-", FAF_max))

#organize demical to integer
data$FAF <- round(data$FAF)
unique_words_FAF <- unique(data$FAF)
print(unique_words_FAF)
data$FAF <- round(data$FAF)
unique_words_TUE <- unique(data$TUE)
print(unique_words_TUE)
TUE_min <- min(unique_words_TUE)
TUE_max <- max(unique_words_TUE)
print(paste("Range of TUE:", TUE_min, "-", TUE_max))

#organize demical to integer
data$TUE <- round(data$TUE)
unique_words_TUE <- unique(data$TUE)
print(unique_words_TUE)
data$TUE <- round(data$TUE)
unique_words_CALC <- unique(data$CALC)
print(unique_words_CALC)
unique_words_MTRANS <- unique(data$MTRANS)
print(unique_words_MTRANS)
unique_words_NObeyesdad <- unique(data$NObeyesdad)
print(unique_words_NObeyesdad)

############Pre-processing############
####aggregations####
##weight/height--BMI##
data$BMI <- round(data$Weight / (data$Height)^2,2)
head(data)
##MTRANS into binary based on the active level, let AutoM and MotorB = 1,
##walking and Cycling = 2
map_transportation <- function(mode) {
  active_modes <- c("Walking", "Cycling")
  if (mode %in% active_modes) {
    return("1")
  } else {
    return("2")
  }
}
data$Transportation <- sapply(data$MTRANS, map_transportation)
data$Transportation <- as.factor(data$Transportation)
data <- data[,-16]

##define age group
# Function to map age to age group
map_age_to_group <- function(Age) {
  if (Age >= 14 & Age <= 20) {
    return("Teenager")
  } else if (Age >= 21 & Age <= 27) {
    return("Young Adult")
  } else if (Age >= 28 & Age <= 34) {
    return("Adult")
  } else if (Age >= 35 & Age <= 40) {
    return("Early Middle-aged Adult")
  } else if (Age >= 41 & Age <= 47) {
    return("Late Middle-aged Adult (1)")
  } else if (Age >= 48 & Age <= 50) {
    return("Late Middle-aged Adult (2)")
  } else if (Age >= 51 & Age <= 60) {
    return("Early Senior")
    
  } else {
    return("Senior")
  }
}
data$Age_Group <- sapply(data$Age, map_age_to_group)
unique(data$Age_Group)

#Scale the numerical data
data$Height <- as.numeric(scale(data$Height))
data$Weight <- as.numeric(scale(data$Weight))
data$Age <- as.numeric(scale(data$Age))

data$NCP<-as.numeric(scale(data$NCP))
data$CH2O<-as.numeric(scale(data$CH2O))
data$FAF<-as.numeric(scale(data$FAF))
data$TUE<-as.numeric(scale(data$TUE))
data$BMI<-as.numeric(scale(data$BMI))
data$FCVC<-as.numeric(scale(data$FCVC))

#check corplot between num variables
dt<-data[,-c(1,5,6,9,10,12,15,16,18,19)]
M<-cor(dt)
head(round(M,2))
library(corrplot)
corrplot(M, method="circle")

'----------1.Explore data-------------'
data <- data[ -c(3,4,19) ] 
'Remove weight, height since we integrated a index variable BMI related to them,
you can also inspect it from corrplot.'
#Convert to factors
str(data) 
data$Gender <- as.factor(data$Gender)
data$family_history_with_overweight <- as.factor(data$family_history_with_overweight)  
data$FAVC <- as.factor(data$FAVC)
data$CAEC<- as.factor(data$CAEC)
data$SMOKE <- as.factor(data$SMOKE)
data$SCC <- as.factor(data$SCC)
data$CALC <- as.factor(data$CALC)
data$NObeyesdad <-as.factor(data$NObeyesdad)
str(data) # Good to go!

#scale numerical data
data[, c("Age", "FCVC", "NCP", "CH2O", "FAF", "TUE","BMI")] <- scale(data[, c("Age", "FCVC",
                                                                "NCP", "CH2O", "FAF", "TUE","BMI")])
head(data)


'--- First, we scale all numerical variables as. above,
We then dummy code variables that have just two levels and are coded 1, 0---'
data$family_history_with_overweight<- ifelse(data$family_history_with_overweight == "yes", 1, 0)
data$FAVC<- ifelse(data$FAVC == "yes", 1, 0)
data$SMOKE <- ifelse(data$SMOKE == "yes", 1, 0)
data$SCC <- ifelse(data$SCC == "yes", 1, 0)

data$Gender <- dummy.code(data$Gender)
data$Transportation <- dummy.code(data$Transportation)

'we dummy code variables that have three or more levels'
CAEC1 <- as.data.frame(dummy.code(data$CAEC))
CALC1 <- as.data.frame(dummy.code(data$CALC))
data_new<- cbind(data, CAEC1, CALC1)
data_new <- subset(data_new, select=-c(CAEC, CALC))
head(data_new)
'So, data_new is the dataset ready for modelling...!!!'

#deal with Y
data_new$NObeyesdad <-as.numeric(data$NObeyesdad) #we need to turn Y into numerical to build model -knn rules
data_new <-data_new %>% relocate(NObeyesdad, .after = last_col())



'---------- 2.KNN Modeling -------------'
set.seed(1234) # set the seed to make the partition reproducible
# 75% of the sample size
smp_size <- floor(0.75 * nrow(data_new))
train_ind <- sample(seq_len(nrow(data_new)), size = smp_size)
# creating test and training sets that contain all of the predictors
data_train <- data_new[train_ind, ]
data_train <- data_train[,-22]
data_test <- data_new[-train_ind, ]
data_test <- data_test[,-22]
# put outcome in its own object
mjob_outcome <- data_new %>% select(NObeyesdad)
train_labels <- mjob_outcome[train_ind, ]
test_labels <- mjob_outcome[-train_ind, ]

'---- KNN k=39 and k=9 (optimal)---------------------------------'
# by rules, choose k = sqrt of number of rows - sqrt(1583) ,and  make it odd only.
library(class)
predictions <- knn(train = data_train, test = data_test, cl = train_labels, k=39)
predictions
plot(predictions)

m_pred_caret <- train(data_train, train_labels, method = "knn", preProcess = c("center","scale"))
m_pred_caret
plot(m_pred_caret) # output shows accuracy peak at k=9 from the OUTPUT.
m_pred_caret$bestTune # also shows accuracy peak at k=9 from the OUTPUT.


'----Scatter plot (k=9)----'
pred_sca<-knn(train = data_train, test = data_test, cl = train_labels, 9)
result <- cbind(data_test, pred_sca)
combinetest <- cbind(data_test, test_labels)

result%>%
  ggplot(aes(x=Age, y=BMI, color=pred_sca))+
  geom_point(size=3)


'----Check model accuracy (k=9), outcome = 0.77 -----'
# Load required library
library(caret)

# Compute accuracy
accuracy <- sum(pred_sca == test_labels) / length(test_labels)
cat("Accuracy:", accuracy, "\n")

# Create a confusion matrix
conf_matrix <- confusionMatrix(data = factor(pred_sca, levels = unique(train_labels)),
                               reference = factor(test_labels, levels = unique(train_labels)))
print(conf_matrix)


'------- 3.KNN algorithms graphs (k=9)---------------'
library(class)
predictions <- knn(train = data_train,
                   test = data_test,
                   cl = train_labels,
                   k= 9)
plot_predictions <- data.frame(
    data_test$BMI, 
    #data_test$Gender,
    data_test$Age,
    #data_test$family_history_with_overweight,
    data_test$FCVC,
    data_test$NCP,#data_test$CAEC,data_test$CALC,
    predicted = predictions)

colnames(plot_predictions) <- c("BMI",
                                "Age",
                                "FCVC",
                                "NCP",
                                'predicted')

'--- knn graph 1 ---'
# Visualize the KNN algorithm results.
library(ggplot2)
# Define the range of values for NCP and Age
ncp_range <- range(c(data_train$NCP, data_test$NCP))
age_range <- range(c(data_train$Age, data_test$Age))
# Generate a grid of NCP and Age values
ncp_grid <- seq(from = ncp_range[1], to = ncp_range[2], length.out = 100)
age_grid <- seq(from = age_range[1], to = age_range[2], length.out = 100)
grid_data <- expand.grid(NCP = ncp_grid, Age = age_grid)
# Make predictions on the grid data
grid_predictions <- knn(train = data_train[, c("NCP", "Age")],
                        test = grid_data,
                        cl = train_labels,
                        k = 9)

# Combine grid data with predictions
grid_predictions <- cbind(grid_data, predicted = grid_predictions)

# Plot the grid with predicted classes
ggplot(grid_predictions, aes(NCP, Age, fill = factor(predicted))) +
  geom_tile(color = "white") +
  scale_fill_manual(values = rainbow(7)) +  # Use a rainbow color scale for 7 classes
  labs(title = "KNN Predictions with Grid",
       fill = "Predicted Class") +
  theme(plot.title = element_text(hjust = 0.5))


'----knn graph 2----'
  # Visualize the KNN algorithm results.
library(ggplot2)
# Define the range of values for NCP and Age
bmi_range <- range(c(data_train$BMI, data_test$BMI))
age_range <- range(c(data_train$Age, data_test$Age))
# Generate a grid of NCP and Age values
bmi_grid <- seq(from = bmi_range[1], to = bmi_range[2], length.out = 100)
age_grid <- seq(from = age_range[1], to = age_range[2], length.out = 100)
grid_data <- expand.grid(BMI = bmi_grid, Age = age_grid)
# Make predictions on the grid data
grid_predictions <- knn(train = data_train[, c("BMI", "Age")],
                        test = grid_data,
                        cl = train_labels,
                        k = 9)
# Combine grid data with predictions
grid_predictions <- cbind(grid_data, predicted = grid_predictions)

# Plot the grid with predicted classes
ggplot(grid_predictions, aes(BMI, Age, fill = factor(predicted))) +
  geom_tile(color = "white") +
  scale_fill_manual(values = rainbow(7)) +  # Use a rainbow color scale for 7 classes
  labs(title = "KNN Predictions with Grid",
       fill = "Predicted Class") +
  theme(plot.title = element_text(hjust = 0.5))

'----knn graph 3----'
library(ggplot2)

# Define the range of values for FCVC and NCP
fcvc_range <- range(c(data_train$FCVC, data_test$FCVC))
ncp_range <- range(c(data_train$NCP, data_test$NCP))

# Generate a grid of FCVC and NCP values
fcvc_grid <- seq(from = fcvc_range[1], to = fcvc_range[2], length.out = 100)
ncp_grid <- seq(from = ncp_range[1], to = ncp_range[2], length.out = 100)
grid_data <- expand.grid(FCVC = fcvc_grid, NCP = ncp_grid)  # Corrected variable names

# Make predictions on the grid data
grid_predictions <- knn(train = data_train[, c("FCVC", "NCP")],
                        test = grid_data,
                        cl = train_labels,
                        k = 9)

# Combine grid data with predictions
grid_predictions <- cbind(grid_data, predicted = grid_predictions)

# Plot the grid with predicted classes
ggplot(grid_predictions, aes(FCVC, NCP, fill = factor(predicted))) +
  geom_tile(color = "white") +
  scale_fill_manual(values = rainbow(7)) +  # Use a rainbow color scale for 7 classes
  labs(title = "KNN Predictions with Grid",
       fill = "Predicted Class") +
  theme(plot.title = element_text(hjust = 0.5))