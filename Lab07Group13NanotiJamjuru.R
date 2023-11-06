# Harsha Jamjuru and Abhishek Nanoti
# MIS 545 Section 001
# Lab07Group13JamjuruNanoti.R
# Using K nearest neighbor algorithm to predict size of sedan
# with respect to reliability, price, roadtest

# Pre-execution  ---------------------------------------------------------------
# Install the packages 
# install.packages("tidyverse")
# install.packages("class")

# Load the packages
library(tidyverse)
library(class)

# Set the working directory
setwd(paste0("C:/Users/ual-laptop/Desktop/MIS-545"))

# Load the value from sedanSize.csv
sedanSize <- read_csv(file = "SedanSize.csv",
                      col_types = "cfnii",
                      col_names = TRUE
)

# Display on console
print(sedanSize)

# Structure of tibble
str(sedanSize)

# Summary of tibble
summary(sedanSize)

# Remove the MakeModel Attribute
sedanSize <- sedanSize %>% select(-MakeModel)

# Separating the tibble in Lables tibble and values tibble
sedanSizeLabel <- sedanSize %>% select(SedanSize)
sedanSize <- sedanSize %>% select(-SedanSize)

# DisplayHistograms function
displayAllHistograms <- function(tibbleDataSet) {
  tibbleDataSet %>%
    keep(is.numeric) %>%
    gather() %>%
    ggplot() + geom_histogram(mapping = aes(x=value,fill=key),
                              color = "black") +
    facet_wrap(~ key, scales="free") +
    theme_minimal()
}

# Passing the sedanSize tibble in dislayHistograms function
displayAllHistograms(sedanSize)

# set seed to 517
set.seed(517)

# Create a new vector which has 75% sampled rows
sampleSet <- sample(nrow(sedanSize),
                    round(nrow(sedanSize)*.75),
                    replace = FALSE)

sedanSizeTraining <- sedanSize[sampleSet,]
sedanSizeLabelTraining <- sedanSizeLabel[sampleSet,]

sedanSizeTesting <- sedanSize[-sampleSet,]
sedanSizeLabelTesting <- sedanSizeLabel[-sampleSet,]

# Using the KNN model for prediction of sedan size
sedanSizePrediction <- knn(train = sedanSizeTraining,
                           test = sedanSizeTesting,
                           cl = sedanSizeLabelTraining$SedanSize,
                           k = 7)

# Display the prediction of knn model on console
print(sedanSizePrediction)

# Display the summary of prediction of knn model on console
summary(sedanSizePrediction)

# Creating a confusion matrix
sedanSizeConfusionMatrix <- table(sedanSizeLabelTesting$SedanSize,
                                  sedanSizePrediction)

# Display the confusion matrix
print(sedanSizeConfusionMatrix)

# The predictive accuracy of our model
sedanSizeAccuracy <- sum(diag(sedanSizeConfusionMatrix)) /
  nrow(sedanSizeTesting)

# Display the model prediction accuracy
print(sedanSizeAccuracy)

# New matrix of k values with the predictive accuracy
kValueMatrix <- matrix(data = NA,
                       nrow = 0,
                       ncol = 2)

# Setting the column names as "k value" and "Predictive Accuracy"
colnames(kValueMatrix) <- c("k value","Predictive Accuracy")

# Storing the k value and the predictive accuracy in the training dataset
for (kValue in 1:nrow(sedanSizeTraining)) {
  if (kValue %% 2 != 0) {
    # Using the KNN model for prediction of sedan size
    sedanSizePrediction <- knn(train = sedanSizeTraining,
                               test = sedanSizeTesting,
                               cl = sedanSizeLabelTraining$SedanSize,
                               k = kValue)

    # Creating a confusion matrix
    sedanSizeConfusionMatrix <- table(sedanSizeLabelTesting$SedanSize,
                                      sedanSizePrediction)

    # The predictive accuracy of our model
    sedanSizeAccuracy <- sum(diag(sedanSizeConfusionMatrix)) /
      nrow(sedanSizeTesting)

    # Adding a new row to the matrix
    kValueMatrix = rbind(kValueMatrix, c(kValue,sedanSizeAccuracy))
  }  
}

# Display the k value matrix on the console
print(kValueMatrix)