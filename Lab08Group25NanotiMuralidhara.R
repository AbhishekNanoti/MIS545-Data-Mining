# Bhargavi Muralidhara and Abhishek Nanoti
# MIS 545 Section 001
# Lab08Group25NanotiMuralidhara.R
# Using K nearest neighbor algorithm to predict size of sedan
# with respect to reliability, price, roadtest

# Pre-execution  -----------------------------------------------------
# Install the packages
# install.packages("tidyverse")
# install.packages("e1071")

# Load the packages
library(tidyverse)
library(e1071)

# Set the working directory
setwd(paste0("C:/Users/ual-laptop/Desktop/MIS-545"))

# Load the value from dwellingType.csv
dwellingType <- read_csv(file = "DwellingType.csv",
                         col_types = "filll",
                         col_names = TRUE)

# Display on console
print(dwellingType)

# Structure of tibble
str(dwellingType)

# Summary of tibble
summary(dwellingType)

# Set seed to 154
set.seed(154)

# Create a vector of 75% randomly sampled rows from the original tibble
sampleSet <- sample(nrow(dwellingType),
                    round(nrow(dwellingType) * 0.75),
                    replace = FALSE)

# Split the tibble into training and testing
dwellingTypeTrain <- dwellingType[sampleSet, ]
dwellingTypeTest <- dwellingType[-sampleSet, ]

# Generate the Naive Bayes model
dwellingTypeNBModel <- naiveBayes(formula = DwellingType ~ .,
                                  data = dwellingTypeTrain,
                                  laplace = 1)

# Probability for every record in Test data
dwellingTypeProb <- predict(dwellingTypeNBModel,
                            dwellingTypeTest,
                            type = "raw")

# Display dwellingTypeProb on the console
print(dwellingTypeProb)

# Predict classes for each record in the test tibble
dwellingTypePrediction <- predict(dwellingTypeNBModel,
                                  dwellingTypeTest,
                                  type = "class")

# Display dwellingTypePrediction on the console
print(dwellingTypePrediction)

# Evaluate the model by forming a confusion matrix
dwellingTypeConfusionMatrix <- table(dwellingTypeTest$DwellingType,
                                     dwellingTypePrediction)

# Display the confusion matrix
print(dwellingTypeConfusionMatrix)

# Calculate the model predictive accuracy
predictiveAccuracy <- sum(diag(dwellingTypeConfusionMatrix)) /
  nrow(dwellingTypeTest)

# Display the predictive accuracy
print(predictiveAccuracy)