# Edward Ku and Abhishek Sanjay Nanoti
# MIS 545 Section 001-02
# Lab06Group25KuNanoti.R
# This code is meant to provide in-depth analysis on the ZooVisitSpending.csv
# dataset provided in MIS545 to extract meaningful information and 
# visualizations by understanding correlation values and creating linear 
# regression models.

# Pre-execution  --------------------------------------------------------
# Install the tidyverse, corrplot, and olsrr packages  
#install.packages("corrplot")
#install.packages("olsrr")
#install.packages("tidyverse")
#install.packages("smotefamily")

# Load the tidyverse, corrplot, and olsrr libraries
library(tidyverse)
library(corrplot)
library(olsrr)
library(smotefamily)

# Set the working directory to your Lab06 folder
setwd(paste0("C:/Users/ual-laptop/Desktop/MIS-545"))

# Read MobilePhoneSubscribers.csv into a tibble called cancelledMobileService 
cancelledMobileService1 <- read_csv(file = "MobilePhoneSubscribers.csv",
                        col_types = "lillnininn",
                        col_names = TRUE )

# Analyze the dataset  --------------------------------------------------
# Display cancelledMobileService in the console
print(cancelledMobileService1)

# Display the structure of cancelledMobileService in the console
str(cancelledMobileService1)

# Display the summary of cancelledMobileService in the console
summary(cancelledMobileService1)

# Display a correlation matrix of cancelledMobileService rounded to two decimal places
round(cor(cancelledMobileService1),2)

# Display a correlation plot using the "number" method and limit output to the 
# bottom left
corrplot(cor(cancelledMobileService1),
         method = "number",
         type = "lower")

cancelledMobileService <- cancelledMobileService1 %>%
  select(-c(DataUsage,DataPlan))

#Split data into training and testing
#The set.seed() function helps us to ensure that we get the same result
#Every time we run a random sampling process
set.seed(203)

#Create a vector of 75% randomly sampled rows from the original dataset
sampleSet <- sample(nrow(cancelledMobileService),
                    round(nrow(cancelledMobileService)*0.75),
                    replace = FALSE)

#Put the records from the 75% sample into Training dataset
cancelledMobileServiceTraining <- cancelledMobileService[sampleSet, ] 

#Put all the remaining (25%) into Testing dataset
cancelledMobileServiceTesting <- cancelledMobileService[-sampleSet, ] 

#Do we have Class Imbalance in the training Dataset?
summary(cancelledMobileServiceTraining$CancelledService)

#Store the magnitude of Class Imbalance into a variable
classImbalanceMagnitude <- 1259 / 354

#Deal with Class Imbalance in the Training Dataset
cancelledMobileServiceTrainingSmoted <- 
  tibble(SMOTE(X = data.frame(cancelledMobileServiceTraining),
               target = cancelledMobileServiceTraining$CancelledService,
               dup_size = 3)$data)

#Convert CancelledService,RecentRenewal,DataPlan back into logical Datatype
cancelledMobileServiceTrainingSmoted <- 
  cancelledMobileServiceTrainingSmoted %>%
  mutate(CancelledService = as.logical(CancelledService),
         RecentRenewal = as.logical(RecentRenewal))

#Remove the new added Class variable
cancelledMobileServiceTrainingSmoted <- 
  cancelledMobileServiceTrainingSmoted %>%
  select(-class)

#Check the Class Imbalance on the Smoted Dataset
summary(cancelledMobileServiceTrainingSmoted)

#Generate the Logistic Regression Model
cancelledMobileServiceModel <- glm(data = cancelledMobileServiceTrainingSmoted,
                                   family = binomial,
                                   formula = CancelledService ~ .)

#Display the Output of Logistic Regression Model.
#Remember the Beta coefficients are log-odds
summary(cancelledMobileServiceModel)


exp(coef(cancelledMobileServiceModel)["AccountWeeks"])
exp(coef(cancelledMobileServiceModel)["RecentRenewalTRUE"])
exp(coef(cancelledMobileServiceModel)["CustServCalls"])
exp(coef(cancelledMobileServiceModel)["AvgCallMinsPerMonth"])
exp(coef(cancelledMobileServiceModel)["AvgCallsPerMonth"])
exp(coef(cancelledMobileServiceModel)["MonthlyBill"])
exp(coef(cancelledMobileServiceModel)["OverageFee"])

#Use the model to predict the output in the testing dataset
cancelledMobileServicePrediction <- predict(cancelledMobileServiceModel,
                                            cancelledMobileServiceTesting,
                                            type = "response")

#Display cancelledMobileServicePrediction on the console
print(cancelledMobileServicePrediction)

summary

#Treat anything below or equal to 0.5 as 0 and greater than 0.5 as 1
cancelledMobileServicePrediction <- 
  ifelse(cancelledMobileServicePrediction >=0.5,1,0)

#Create a Confusion Matrix
cancelledMobileServiceConfusionMatrix <- 
  table(cancelledMobileServiceTesting$CancelledService,
        cancelledMobileServicePrediction)

#Print the Confusion Matrix on Console
print(cancelledMobileServiceConfusionMatrix)

#Calculate the False Positive Rate
cancelledMobileServiceConfusionMatrix[1,2]/
  (cancelledMobileServiceConfusionMatrix[1,2]
   +cancelledMobileServiceConfusionMatrix[1,1])
        
#Calculate the False Negative Rate
cancelledMobileServiceConfusionMatrix[2,1]/
  (cancelledMobileServiceConfusionMatrix[2,2]
   +cancelledMobileServiceConfusionMatrix[2,1])

#Calculate the prediction accuracy by dividing the number of true
#positives and true negatives by the total amount of predictions in the
#testing dataset
sum(diag(cancelledMobileServiceConfusionMatrix))/
  nrow(cancelledMobileServiceTesting)
