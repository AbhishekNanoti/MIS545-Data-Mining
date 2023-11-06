# FNU GEETIKA, ABHISHEK SANJAY NANOTI, GURLEEN KOHLI, LIKITHA TRIPURANENI, 
# AMANDEEP KAUR
# MIS 545 01
# MIS545HotelCancellation.R
# This code demonstrates the summary of HotelCancellation.csv, creates training 
# and testing tibbles, generates the logistic regression, knn, naive bayes, 
# decision tree and neural networks model, predicts labels, confusion matrix 
# and predictive accuracy

# Install tidyverse, rpart.plot, dplyr packages -----------------------------
# install.packages("tidyverse")
# install.packages("rpart.plot")
# install.packages("dplyr")
# install.packages("dummies", repos = NULL, type="source")
# install.packages("corrplot")
# install.packages("olsrr")
# install.packages("smotefamily")
# install.packages("neuralnet)
# install.packages("e1071")

# Load tidyverse, rpart, rpart.plot, dplyr packages -------------------------
library(tidyverse)
library(rpart)
library(rpart.plot)
library(dplyr)
library(corrplot)
library(olsrr)
library(smotefamily)
library(dummies)
library(scales)
library(neuralnet)
library(class)
library(e1071)

# Set the current working directory ----------------------------------
setwd("C:/Geetika/MIS 545/Project")

# Read the HotelCancellation.csv file into a tibble ----------------
hotelCancellation <- read_csv(file= "HotelCancellation.csv",
                      col_types = "flnnnfnff",
                      col_names = TRUE)

# Display hotelCancellation tibble -------------------------------------------
print(hotelCancellation)

# Display the structure of hotelCancellation ---------------------------------
print(str(hotelCancellation))

# Summarize the hotelCancellation tibble -------------------------------------
print(summary(hotelCancellation))

# Duplicating the hotelCancellation to hotelCancellationCorrelation tibble ---
hotelCancellationCorrelation <- hotelCancellation

# Changing the columns to numeric
hotelCancellationCorrelation$hotel <- 
  as.numeric(hotelCancellationCorrelation$hotel)
hotelCancellationCorrelation$marketSegment <- 
  as.numeric(hotelCancellationCorrelation$marketSegment)
hotelCancellationCorrelation$bookingChanges <- 
  as.numeric(hotelCancellationCorrelation$bookingChanges)
hotelCancellationCorrelation$customerType <- 
  as.numeric(hotelCancellationCorrelation$customerType)
hotelCancellationCorrelation$isCanceled <-
  as.numeric(hotelCancellationCorrelation$isCanceled)
hotelCancellationCorrelation$arrivalMonth <-
  as.numeric(hotelCancellationCorrelation$arrivalMonth)
hotelCancellationCorrelation$arrivalDay <- 
  as.numeric(hotelCancellationCorrelation$arrivalDay)
hotelCancellationCorrelation$customerType <- 
  as.numeric(hotelCancellationCorrelation$customerType)

# Print the structure of hotelCancellationCorrelation
print(str(hotelCancellationCorrelation))

# Display the correlation
round(cor(hotelCancellationCorrelation),2)

# Correlation Plot
corrplot(cor(hotelCancellationCorrelation),
         method="number",
         type="lower",
         )

# Create displayAllHistograms function ---------------------------------------
displayAllHistograms <- function(tibbleDataset) {
  tibbleDataset %>%
    keep(is.numeric) %>%
    gather() %>%
    ggplot() + geom_histogram(mapping = aes(x=value,fill=key),
                              color="black",
                              bins = 30
    )+
    facet_wrap(~key,scales="free")+
    theme_minimal()
}

# Display all histograms for hotelCancellation -------------------------------
displayAllHistograms(hotelCancellation)

# Maximum cancellations ------------------------------------------------------
print(hotelCancellation %>%
        filter(isCanceled == 1) %>%
        group_by(hotel,marketSegment) %>%
        summarize(Performance = n()) %>%
        arrange(desc(Performance), n=Inf) %>%
        slice(1:3))

# Calculate the percentage of cancellations for months and see which months 
# have the maximum percentage of cancellations based on booking ---------------
print(hotelCancellation %>%
        filter(isCanceled == 1) %>%
        group_by(arrivalMonth) %>%
        summarize(Performance = n()) %>%
        arrange(desc(Performance), n=Inf) %>%
        slice(1:12))

# Customers cancellations with respect to lead time --------------------------
print(hotelCancellation %>%
        filter( isCanceled ==1 & leadTime <7) %>%
        group_by(hotel,customerType) %>%
        summarize(Performance = n()) %>%
        arrange(desc(Performance), n=Inf))

# Convert into data frame  ----------------------------------------------------
hotelCancellationDataframe <- data.frame(hotelCancellation,
                                         check.names = FALSE)

# Dummy code customerType ----------------------------------------------------
hotelCancellationDummy <- as_tibble(dummy.data.frame
                                    (data = hotelCancellationDataframe,
                                      names= "customerType"))

# Convert into data frame  ----------------------------------------------------
hotelCancellationDataframe <- data.frame(hotelCancellationDummy,
                                         check.names = FALSE)

# Dummy Coding hotel ---------------------------------------------------------
hotelCancellationDummy <- as_tibble(dummy.data.frame
                                    (data = hotelCancellationDataframe,
                                      names= "hotel"))

# Convert into data frame  ----------------------------------------------------
hotelCancellationDataframe <- data.frame(hotelCancellationDummy,
                                         check.names = FALSE)

# Dummy Coding marketSegment --------------------------------------------------
hotelCancellationDummy <- as_tibble(dummy.data.frame
                                    (data = hotelCancellationDataframe,
                                      names= "marketSegment"))

# Correcting the name of the columns -----------------------------------------
hotelCancellationDummy <- hotelCancellationDummy %>%
  mutate(hotelResort = `hotelResort Hotel`,
         hotelCity = `hotelCity Hotel`,
         marketSegmentOnlineTA = `marketSegmentOnline TA`,
         marketSegmentOfflineTATO = `marketSegmentOffline TA/TO`,
         customerTypeParty = `customerTypeTransient-Party`
  ) %>%
  select(-c(`hotelResort Hotel`, `hotelCity Hotel`, `marketSegmentOnline TA`,
            `marketSegmentOffline TA/TO`, `customerTypeTransient-Party`))

# Summarize the hotelCancellation tibble -------------------------------------
print(summary(hotelCancellation))

# Split data into training and testing -------------------------------------
# The set.seed() function helps us to ensure that we get the same result
# Every time we run a random sampling process ---------------------------------
set.seed(517)

# Create a vector of 75% randomly sampled rows from the original dataset -----
sampleSet <- sample(nrow(hotelCancellationDummy),
                    round(nrow(hotelCancellationDummy)*0.75),
                    replace = FALSE)

#Put the records from the 75% sample into Training dataset -------------------
hotelCancellationLogTraining <- hotelCancellationDummy[sampleSet, ]

# Put all the remaining (25%) into Testing dataset --------------------------
hotelCancellationLogTesting <- hotelCancellationDummy[-sampleSet, ]

# Do we have Class Imbalance in the training Dataset? ----------------------
summary(hotelCancellationLogTraining$isCanceled)

# Store the magnitude of Class Imbalance into a variable -------------------
classImbalanceMagnitude <- 560 / 162

# Deal with Class Imbalance in the Training Dataset ------------------------
hotelCancellationLogTrainingSmoted <-
  tibble(SMOTE(X = data.frame(hotelCancellationLogTraining),
               target = hotelCancellationLogTraining$isCanceled,
               dup_size = 3)$data)

# Remove the new added Class variable ---------------------------------------
hotelCancellationLogTrainingSmoted <-
  hotelCancellationLogTrainingSmoted %>%
  select(-class)

# Check the Class Imbalance on the Smoted Dataset ---------------------------
summary(hotelCancellationLogTrainingSmoted$isCanceled)

# Generate the Logistic Regression Model -----------------------------------
hotelCancellationLogModel <- glm(data = hotelCancellationLogTraining,
                                 family = binomial,
                                 formula = isCanceled ~ .)

# Display the Output of Logistic Regression Model ---------------------------
# Remember the Beta coefficients are log-odds --------------------------------
summary(hotelCancellationLogModel)

# Log odds for all the independent variables ---------------------------------
exp(coef(hotelCancellationLogModel)["hotelResort"])
exp(coef(hotelCancellationLogModel)["hotelCity"])
exp(coef(hotelCancellationLogModel)["leadTime"])
exp(coef(hotelCancellationLogModel)["arrivalDateMonth"])
exp(coef(hotelCancellationLogModel)["arrivalDateDayOfMonth"])
exp(coef(hotelCancellationLogModel)["marketSegmentDirect"])
exp(coef(hotelCancellationLogModel)["marketSegmentOnlineTA"])
exp(coef(hotelCancellationLogModel)["marketSegmentGroups"])
exp(coef(hotelCancellationLogModel)["marketSegmentOfflineTATO"])
exp(coef(hotelCancellationLogModel)["marketSegmentCorporate"])
exp(coef(hotelCancellationLogModel)["marketSegmentAviation"])
exp(coef(hotelCancellationLogModel)["bookingChanges"])
exp(coef(hotelCancellationLogModel)["customerTypeTransient"])
exp(coef(hotelCancellationLogModel)["customerTypeParty"])
exp(coef(hotelCancellationLogModel)["customerTypeGroup"])

# Use the model to predict the output in the testing dataset -----------------
hotelCancellationLogPrediction <- predict(hotelCancellationLogModel,
                                          hotelCancellationLogTesting,
                                          type = "response")

# Display hotelCancellationLogPrediction on the console ----------------------
print(hotelCancellationLogPrediction)

# Summary of hotelCancellationLogPrediction ----------------------------------
summary(hotelCancellationLogPrediction)

# Treat anything below or equal to 0.5 as 0 and greater than 0.5 as 1 --------
hotelCancellationLogPrediction <-
  ifelse(hotelCancellationLogPrediction >=0.5,1,0)

# Create a Confusion Matrix -------------------------------------------------
hotelCancellationLogConfusionMatrix <-
  table(hotelCancellationLogTesting$isCanceled,
        hotelCancellationLogPrediction)

# Print the Confusion Matrix on Console -------------------------------------
print(hotelCancellationLogConfusionMatrix)

# Calculate the False Positive Rate -----------------------------------------
falsePositiveRateLog <- 
  hotelCancellationLogConfusionMatrix[1,2]/
  (hotelCancellationLogConfusionMatrix[1,2]
   + hotelCancellationLogConfusionMatrix[1,1])

# Print False Positive Rate ------------------------------------------------
print(falsePositiveRateLog)

# Calculate the False Negative Rate ----------------------------------------
falseNegativeRateLog <- 
  hotelCancellationLogConfusionMatrix[2,1]/
  (hotelCancellationLogConfusionMatrix[2,2]
   + hotelCancellationLogConfusionMatrix[2,1])

# Print False Negative Rate ------------------------------------------------
print(falseNegativeRateLog)

# Calculate the prediction accuracy by dividing the number of true
# positives and true negatives by the total amount of predictions in the
# testing dataset ----------------------------------------------------------
predictiveAccuracyLog <- sum(diag(hotelCancellationLogConfusionMatrix))/
  nrow(hotelCancellationLogTesting)

# Display the prediction accuracy on the console------------------------------
print(predictiveAccuracyLog)

# Separating the tibble into two - hotelCancellationKNN with 7 variables (hotel,
# leadTime, arrivalMonth, arrivalDay, marketSegment, bookingChanges,
# customerType) and hotelCancellationLabelsKNN with 1 variables (isCanceled) --
hotelCancellationKNNLabels <- hotelCancellationDummy %>% select(isCanceled)
hotelCancellationKNN <- hotelCancellationDummy %>% select(-isCanceled)

# Create a vector of 75% randomly sampled rows from hotelCancellationKNN------
sampleSet <- sample(nrow(hotelCancellationKNN),
                    round(nrow(hotelCancellationKNN) * 0.75),
                    replace = FALSE)

# Put the records from 75% sample into hotelCancellationKNNTraining
# (7 variables) and hotelCancellationKNNTrainingLabels (1 variable)-----------
hotelCancellationKNNTraining <- hotelCancellationKNN[sampleSet, ]
hotelCancellationKNNTrainingLabels <- hotelCancellationKNNLabels[sampleSet, ]

# Put the records from 25% sample into hotelCancellationKNNTesting (7 variables)
# and hotelCancellationKNNTestingLabels (1 variable)--------------------------
hotelCancellationKNNTesting <- hotelCancellationKNN[-sampleSet, ]
hotelCancellationKNNTestingLabels <- hotelCancellationKNNLabels[-sampleSet, ]

# Generating the k-nearest neighbors model using hotelCancellationKNNTraining as
# the train argument, hotelCancellationKNNTesting as the test argument,
# hotelCancellationKNNTrainingLabels$isCanceled as the cl argument, and 7 as the
# value for the k argument----------------------------------------------------
hotelCancellationKNNPrediction <-
  knn(train = hotelCancellationKNNTraining,
      test = hotelCancellationKNNTesting,
      cl = hotelCancellationKNNTrainingLabels$isCanceled,
      k = 27)

# Displaying the predictions from the testing dataset on the console----------
print(hotelCancellationKNNPrediction)

# Display summary of the predictions from the testing dataset-----------------
print(summary(hotelCancellationKNNPrediction))

# Create confusion matrix-----------------------------------------------------
hotelCancellationKNNConfusionMatrix <-
  table(hotelCancellationKNNTestingLabels$isCanceled,
        hotelCancellationKNNPrediction)

# Display confusion matrix on the console-------------------------------------
print(hotelCancellationKNNConfusionMatrix)

# Calculate the False Positive Rate -----------------------------------------
falsePositiveRateKNN <- 
  hotelCancellationKNNConfusionMatrix[1,2]/
  (hotelCancellationKNNConfusionMatrix[1,2]
   + hotelCancellationKNNConfusionMatrix[1,1])

# Print False Positive Rate ------------------------------------------------
print(falsePositiveRateKNN)

# Calculate the False Negative Rate ----------------------------------------
falseNegativeRateKNN <- 
  hotelCancellationKNNConfusionMatrix[2,1]/
  (hotelCancellationKNNConfusionMatrix[2,2]
   + hotelCancellationKNNConfusionMatrix[2,1])

# Print False Negative Rate ------------------------------------------------
print(falseNegativeRateKNN)

# Calculate the model predictive accuracy and storing it into a variable
# called predictiveAccuracyKNN------------------------------------------------
predictiveAccuracyKNN <- sum(diag(hotelCancellationKNNConfusionMatrix)) /
  nrow(hotelCancellationKNNTesting)

# Display the prediction accuracy on the console------------------------------
print(predictiveAccuracyKNN)

# Create a matrix of k-values with their predictive accuracy and storing the
# matrix into an object called kValueMatrix-----------------------------------
kValueMatrix = matrix(data = NA,
                      nrow = 0,
                      ncol = 2)

# Assign column names of "k value" and "Predictive accuracy" to the kValueMatrix
colnames(kValueMatrix) = c("k value", "Predictive accuracy")

# Loop through odd values of k from 1 up 499 in the training dataset. With each
# pass through the loop, storing the k-value along with its predictive accuracy
for(kValue in 1:499){
  if(kValue %% 2 != 0){
    # Generate the model
    hotelCancellationKNNPrediction <-
      knn(train = hotelCancellationKNNTraining,
          test = hotelCancellationKNNTesting,
          cl = hotelCancellationKNNTrainingLabels$isCanceled,
          k = kValue)
    # Generate the confusion matrix
    hotelCancellationKNNConfusionMatrix <-
      table(hotelCancellationKNNTestingLabels$isCanceled,
            hotelCancellationKNNPrediction)
    # Generate the predictive accuracy
    predictiveAccuracyKNN <- sum(diag(hotelCancellationKNNConfusionMatrix)) /
      nrow(hotelCancellationKNNTesting)
    # Adding a new row to the kValueMatrix-
    kValueMatrix <- rbind(kValueMatrix, c(kValue, predictiveAccuracyKNN))
  }
}

# Display the kValueMatrix on the console to determine the best k-value-------
print(kValueMatrix)

# Dichotimize the lead_time attribute ----------------------------------------
hotelCancellationDicot = hotelCancellation %>%
  mutate(advanceBooking = ifelse(leadTime > 60,1,0),
         last2weeks = ifelse(arrivalDay > 14, 1, 0),
         quarter = case_when(
           arrivalMonth < 4 & arrivalMonth > 0 ~ 1,
           arrivalMonth < 7 & arrivalMonth > 3 ~ 2,
           arrivalMonth < 10 & arrivalMonth > 6 ~ 3,
           arrivalMonth < 13 & arrivalMonth > 9 ~ 4
           )
         )

# Remove columns ------------------------------------------------------------
hotelCancellationDicot <- hotelCancellationDicot %>%
  select(-c(leadTime, 
            arrivalMonth, 
            arrivalDay))

# Create a vector of 75% randomly sampled rows from hotelCancellationDicot
sampleSet <- sample(nrow(hotelCancellationDicot),
                    round(nrow(hotelCancellationDicot) * 0.75),
                    replace = FALSE)

# Put the records from 75% sample into hotelCancellationNaiveModelTraining ----
hotelCancellationNaiveModelTraining <- hotelCancellationDicot[sampleSet, ]

# Put the records from 25% sample into hotelCancellationNaiveModelTesting ----
hotelCancellationNaiveModelTesting <- hotelCancellationDicot[-sampleSet, ]

# Generate Naive Bayes model ------------------------------------------------
hotelCancellationNaiveBayesModel <-  
  naiveBayes(formula = isCanceled ~.,
             data = hotelCancellationNaiveModelTraining,
             laplace = 1)

# Display the Naive Bayes Model -----------------------------------------
print(hotelCancellationNaiveBayesModel)

# Predict classes for each record in the testing dataset -------------
hotelCancellationNaivePrediction <- predict(hotelCancellationNaiveBayesModel,
                                            hotelCancellationNaiveModelTesting,
                                            type = "class")

# Display predictions from hotelCancellationNaivePrediction ----------------
print(hotelCancellationNaivePrediction)

# Create confusion matrix --------------------------------------------
hotelCancellationNaiveConfusionMatrix <- 
  table(hotelCancellationNaiveModelTesting$isCanceled,
        hotelCancellationNaivePrediction)

# Display confusion matrix -------------------------------------------
print(hotelCancellationNaiveConfusionMatrix)

# Calculate the False Positive Rate -----------------------------------------
falsePositiveRateNaive <- 
  hotelCancellationNaiveConfusionMatrix[1,2]/
  (hotelCancellationNaiveConfusionMatrix[1,2]
   + hotelCancellationNaiveConfusionMatrix[1,1])

# Print False Positive Rate ------------------------------------------------
print(falsePositiveRateNaive)

# Calculate the False Negative Rate ----------------------------------------
falseNegativeRateNaive <- 
  hotelCancellationNaiveConfusionMatrix[2,1]/
  (hotelCancellationNaiveConfusionMatrix[2,2]
   + hotelCancellationNaiveConfusionMatrix[2,1])

# Print False Negative Rate ------------------------------------------------
print(falseNegativeRateNaive)

# Calculate the prediction accuracy ----------------------------------
predictiveAccuracyNaive <- sum(diag(hotelCancellationNaiveConfusionMatrix)) /
  nrow(hotelCancellationNaiveModelTesting)

# Display the prediction accuracy ------------------------------------
print(predictiveAccuracyNaive)

# Put the records from 75% sample into hotelCancellationDicotTraining ---------
hotelCancellationDicotTraining <- hotelCancellationDicot[sampleSet, ]

# Put the records from 25% sample into hotelCancellationDicotTesting ---------
hotelCancellationDicotTesting <- hotelCancellationDicot[-sampleSet, ]

# Generate Decision Tree model for 0.01 complexity parameter----------
hotelCancellationDicotDecisionModel<- rpart(formula = isCanceled ~ .,
                                        method = "class",
                                        cp = 0.01,
                                        data = hotelCancellationDicotTraining)

# Display the descision tree -----------------------------------------
rpart.plot(hotelCancellationDicotDecisionModel)

# Predict classes for each record in the testing dataset -------------
hotelCancellationDicotPrediction <- predict(hotelCancellationDicotDecisionModel,
                                       hotelCancellationDicotTesting,
                               type = 'class')

# Display predictions from hotelCancellationPrediction -----------------------
print(hotelCancellationDicotPrediction)

# Create confusion matrix --------------------------------------------
hotelCancellationDicotConfusionMatrix <- 
  table(hotelCancellationDicotTesting$isCanceled,
        hotelCancellationDicotPrediction)

# Display confusion matrix -------------------------------------------
print(hotelCancellationDicotConfusionMatrix)

# Calculate the False Positive Rate -----------------------------------------
falsePositiveRateDicot <- 
  hotelCancellationDicotConfusionMatrix[1,2]/
  (hotelCancellationDicotConfusionMatrix[1,2]
   + hotelCancellationDicotConfusionMatrix[1,1])

# Print False Positive Rate ------------------------------------------------
print(falsePositiveRateDicot)

# Calculate the False Negative Rate ----------------------------------------
falseNegativeRateDicot <- 
  hotelCancellationDicotConfusionMatrix[2,1]/
  (hotelCancellationDicotConfusionMatrix[2,2]
   + hotelCancellationDicotConfusionMatrix[2,1])

# Print False Negative Rate ------------------------------------------------
print(falseNegativeRateDicot)

# Calculate the prediction accuracy ----------------------------------
predictiveAccuracyDicot <- sum(diag(hotelCancellationDicotConfusionMatrix)) /
  nrow(hotelCancellationDicotTesting)

# Display the prediction accuracy ------------------------------------
print(predictiveAccuracyDicot)

# Generate Decision Tree model for 0.0007 complexity parameter---------
hotelCancellationDicotDecisionModel <- rpart(formula = isCanceled ~ .,
                                method = "class",
                                cp = 0.0007,
                                data = hotelCancellationDicotTraining)

# Display the descision tree -----------------------------------------
rpart.plot(hotelCancellationDicotDecisionModel)

# Predict classes for each record in the testing dataset -------------
hotelCancellationDicotPrediction <- predict(hotelCancellationDicotDecisionModel,
                                       hotelCancellationDicotTesting,
                                       type = 'class')

# Display predictions from hotelCancellationPrediction -----------------------
print(hotelCancellationDicotPrediction)

# Create confusion matrix --------------------------------------------
hotelCancellationDicotConfusionMatrix <- table(
                                      hotelCancellationDicotTesting$isCanceled,
                                      hotelCancellationDicotPrediction)

# Display confusion matrix -------------------------------------------
print(hotelCancellationDicotConfusionMatrix)

# Calculate the prediction accuracy ----------------------------------
predictiveAccuracyDicot <- sum(diag(hotelCancellationDicotConfusionMatrix)) /
  nrow(hotelCancellationDicotTesting)

# Display the prediction accuracy ------------------------------------
print(predictiveAccuracyDicot)

# Scaling the lead_time variable----------------------------------------------
hotelCancellationNN <- hotelCancellationDummy %>%
  mutate(lead_time_scaled = (leadTime - min(leadTime))/
           (max(leadTime)- min(leadTime)))

# Scaling the arrival_date_day_of_month varibale------------------------------
hotelCancellationNN <- hotelCancellationNN %>%
  mutate(arrivalDay_scaled =
           (arrivalDay - min(arrivalDay))/
           (max(arrivalDay)- min(arrivalDay)))

# Scaling the arrivalMonth variable-------------------------------------------
hotelCancellationNN <- hotelCancellationNN %>%
  mutate(arrivalMonth_scaled =
           (arrivalMonth - min(arrivalMonth))/
           (max(arrivalMonth)- min(arrivalMonth)))

# Scaling the booking_changes variable------------------------------------------
hotelCancellationNN <- hotelCancellationNN %>%
  mutate(bookingChanges_scaled =
           (bookingChanges - min(bookingChanges))/
           (max(bookingChanges)- min(bookingChanges)))

# Remove columns ------------------------------------------------------------
hotelCancellationNN <- hotelCancellationNN %>%
  select(-c(leadTime, 
            arrivalMonth, 
            arrivalDay,
            bookingChanges))

# Randomly splitting the dataset into hotelCancellationTraining (75% of records)
# and hotelCancellationTesting (25% of records)
sampleSet <- sample(nrow(hotelCancellationNN),
                    round(nrow(hotelCancellationNN) * 0.75),
                    replace = FALSE)
hotelCancellationNNTraining <- hotelCancellationNN[sampleSet, ]
hotelCancellationNNTesting <- hotelCancellationNN[-sampleSet, ]

# Generating the neural network model to predict is_canceled
# (dependent variable) using independent variables------------------------------
hotelCancellationNN <- neuralnet(
  formula = isCanceled ~ .,
  data = hotelCancellationNNTraining,
  hidden = 3,
  act.fct = "logistic",
  linear.output = FALSE
)

# Displaying the neural network numeric results---------------------------------
print(hotelCancellationNN$result.matrix)

# Visualizing the neural network------------------------------------------------
plot(hotelCancellationNN)

# Using hotelCancellationNeuralNet to generate probabilities on the
# hotelCancellationTesting data set---------------------------------------------
hotelCancellationNNProbability <- compute(hotelCancellationNN,
                                        hotelCancellationNNTesting)

# Displaying the probabilities from the testing dataset on the console----------
print(hotelCancellationNNProbability$net.result)

# Converting probability predictions into 0/1 predictions and store this into
# hotelCancellationPrediction---------------------------------------------------
hotelCancellationNNPrediction <-
  ifelse(hotelCancellationNNProbability$net.result > 0.5, 1, 0 )

# Displaying the 0/1 predictions on the console---------------------------------
print(hotelCancellationNNPrediction)

# Evaluating the model by forming a confusion matrix----------------------------
hotelCancellationNNConfusionMatrix <- 
  table(hotelCancellationNNTesting$isCanceled,
        hotelCancellationNNPrediction)

# Displaying the confusion matrix on the console------------------------------
print(hotelCancellationNNConfusionMatrix)

# Calculating the model predictive accuracy-----------------------------------
predictiveAccuracyNN <- sum(diag(hotelCancellationNNConfusionMatrix)) /
  nrow(hotelCancellationNNTesting)

# Displaying the predictive accuracy on the console---------------------------
print(predictiveAccuracyNN)
