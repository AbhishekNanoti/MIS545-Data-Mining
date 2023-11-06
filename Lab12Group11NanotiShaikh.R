#  and Abhishek Nanoti
# MIS 545 Section 001
# Lab12Group11NanotiShaikh.R
# Using K means clustering algorithm to determine the relation between 
# Corruption index and Nmber of days to open business accoring to different 
# countries

# Pre-execution  -----------------------------------------------------
# Install the packages
# install.packages("tidyverse")
# install.packages("neuralnet")

# Load the packages
library(tidyverse)
library(neuralnet)

# Set the working directory
setwd(paste0("C:/Users/ual-laptop/Desktop/MIS-545"))

# Load the value from fishingCharter.csv
fishingCharter <- read_csv(file = "fishingCharter.csv",
                      col_types = "lnn",
                      col_names = TRUE)

# Display on console
print(fishingCharter)

# Structure of tibble
str(fishingCharter)

# Summary of tibble
summary(fishingCharter)

# Scale the AnnualIncome feature from 0 to 1
fishingCharter <- fishingCharter %>%
  mutate(AnnualIncomeScaled = (AnnualIncome - min(AnnualIncome))/
  (max(AnnualIncome) - min(AnnualIncome)))

# Scale the CatchRate feature from 0 to 1
fishingCharter <- fishingCharter %>%
  mutate(CatchRateScaled = (CatchRate - min(CatchRate))/
           (max(CatchRate) - min(CatchRate)))

# Set seed to 591
set.seed(591)

# Create a vector of 75% randomly sampled rows from the original tibble
sampleSet <- sample(nrow(fishingCharter),
                    round(nrow(fishingCharter) * 0.75),
                    replace = FALSE)

# Split the tibble into training  dataset
fishingCharterTrain <- fishingCharter[sampleSet, ]

# Split the tibble into testing  dataset
fishingCharterTest <- fishingCharter[-sampleSet, ]

# Generate the Naive Bayes model
fishingCharterNeuralNet <- neuralnet(
  formula = CharteredBoat ~ CatchRateScaled + AnnualIncomeScaled,
  data = fishingCharterTrain,
  hidden = 3,
  act.fct = "logistic",
  linear.output = FALSE)

# Display the Neural Network Results
print(fishingCharterNeuralNet$result.matrix)

# Visualise the Neural Network
plot(fishingCharterNeuralNet)

# Use the newural network to generate probabilities
# on the testig dataset
fishingCharterProbability <- compute(fishingCharterNeuralNet,
                                     fishingCharterTest)

# Display the probabilities of testing dataset
print(fishingCharterProbability$net.result)

# Convert probability predictions into 0/1 predictions
fishingCharterPrediction <- 
  ifelse(fishingCharterProbability$net.result > 0.5,1,0)

# Display the predictions of testing dataset
print(fishingCharterPrediction)

# Evaluate the model by forming a confusion matrix
fishingCharterConfusionMatrix <- table(fishingCharterTest$CharteredBoat,
                                     fishingCharterPrediction)
# Display the confusion matrix
print(fishingCharterConfusionMatrix)

# Calculate the model predictive accuracy
predictiveAccuracy <- sum(diag(fishingCharterConfusionMatrix)) /
  nrow(fishingCharterTest)

# Display the predictive accuracy
print(predictiveAccuracy)

