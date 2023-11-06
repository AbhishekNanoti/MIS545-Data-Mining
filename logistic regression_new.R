# 

# Install the required packages
install.packages("dummies", repos = NULL, type="source")
install.packages("tidyverse")
install.packages("corrplot")
install.packages("olsrr")
install.packages("smotefamily")

# Load the tidyverse, corrplot, and olsrr libraries
library(tidyverse)
library(corrplot)
library(olsrr)
library(smotefamily)
library(dummies)

# Set the working directory to your Lab06 folder
setwd(paste0("C:/Users/ual-laptop/Desktop/MIS-545"))

# Read HotelCancellation.csv into a tibble called hotelCancellation 
hotelCancellation <- read_csv(file = "hotelCancellation.csv",
                              col_types = "flnnnfnfff",
                              col_names = TRUE )

# Determine the value of outlier minimum  ---------------------------------
outlierMin <- quantile(hotelCancellation$booking_changes, .25) -
  (IQR(hotelCancellation$booking_changes) * 1.5)

# Determine the value of outlier maximum  ---------------------------------
outlierMax <- quantile(hotelCancellation$booking_changes, .75) +
  (IQR(hotelCancellation$booking_changes) * 1.5)

# Determine the outliers in the hotelCancellation tibble for booking_changes ---
bookingChangesOutliers <- hotelCancellation %>%
  filter(booking_changes < outlierMin | booking_changes > outlierMax)



boxplotbookingChanges <- ggplot(data = bookingChangesOutliers,
                                aes(x = booking_changes))

# Geometry layer
boxplotbookingChanges + geom_boxplot(color = "blue",
                                     fill = "lightblue")

# Remove outliers
hotelCancellation <- hotelCancellation %>%
  filter(booking_changes > outlierMin | booking_changes < outlierMax)

# removing the non required columns agent
hotelCancellation = hotelCancellation %>%
  select(-c(agent))

# Convert into data frame
hotelCancellationdataframe <- data.frame(hotelCancellation,check.names = FALSE)

# 
hotelCancellation <- as_tibble(dummy.data.frame
                               (data = hotelCancellationdataframe,
                                 names= "customerType"))

hotelCancellationdataframe <- data.frame(hotelCancellation,check.names = FALSE)

hotelCancellation <- as_tibble(dummy.data.frame
                               (data = hotelCancellationdataframe,
                                 names= "hotel"))

hotelCancellationdataframe <- data.frame(hotelCancellation,check.names = FALSE)

hotelCancellation <- as_tibble(dummy.data.frame
                               (data = hotelCancellationdataframe,
                                 names= "marketSegment"))


summary(hotelCancellation)

#hotelCancellation <- hotelCancellation %>%
#  mutate(hotelResort = hotelResort.Hotel,
#         hotelCity = hotelCity.Hotel,
#         marketOnlineTA = `marketSegmentOnline TA`,
#         marketOfflineTATO = `marketSegmentOffline TA/TO`,
#         customerParty = customerTypeTransient.Party
#  ) %>%
#  select(-c(hotelResort.Hotel,hotelCity.Hotel,`marketSegmentOnline TA`,
#            `marketSegmentOffline TA/TO`,customerTypeTransient.Party))





# Display a correlation matrix of hotelCancellation 
# rounded to two decimal places
round(cor(hotelCancellation),2)

# Display a correlation plot using the "number" method and limit output to the 
# bottom left
corrplot(cor(hotelCancellation),
         method = "number",
         type = "lower",
         number.cex = 0.5,
         tl.cex = 0.5)

# Split data into training and testing
# The set.seed() function helps us to ensure that we get the same result
# Every time we run a random sampling process
set.seed(203)

# Create a vector of 75% randomly sampled rows from the original dataset
sampleSet <- sample(nrow(hotelCancellation),
                    round(nrow(hotelCancellation)*0.75),
                    replace = FALSE)

#Put the records from the 75% sample into Training dataset
hotelCancellationTraining <- hotelCancellation[sampleSet, ] 

# Put all the remaining (25%) into Testing dataset
hotelCancellationTesting <- hotelCancellation[-sampleSet, ] 

# Do we have Class Imbalance in the training Dataset?
summary(hotelCancellationTraining$isCanceled)

# Store the magnitude of Class Imbalance into a variable
classImbalanceMagnitude <- 568 / 154

# Deal with Class Imbalance in the Training Dataset
hotelCancellationTrainingSmoted <- 
  tibble(SMOTE(X = data.frame(hotelCancellationTraining),
               target = hotelCancellationTraining$isCanceled,
               dup_size = 3)$data)

# Remove the new added Class variable
hotelCancellationTrainingSmoted <- 
  hotelCancellationTrainingSmoted %>%
  select(-class)

hotelCancellationTrainingSmoted$isCanceled <- 
  as.logical(hotelCancellationTrainingSmoted$isCanceled)

# Check the Class Imbalance on the Smoted Dataset
summary(hotelCancellationTrainingSmoted$isCanceled)

# Generate the Logistic Regression Model
hotelCancellationModel <- glm(data = hotelCancellationTraining,
                              family = binomial,
                              formula = isCanceled ~ .)

# Display the Output of Logistic Regression Model.
# Remember the Beta coefficients are log-odds
summary(hotelCancellationModel)


exp(coef(hotelCancellationModel)["hotelResort"])
exp(coef(hotelCancellationModel)["hotelCity"])
exp(coef(hotelCancellationModel)["leadTime"])
exp(coef(hotelCancellationModel)["arrivalDateMonth"])
exp(coef(hotelCancellationModel)["arrivalDateDayOfMonth"])
exp(coef(hotelCancellationModel)["marketSegmentDirect"])
exp(coef(hotelCancellationModel)["marketOnlineTA"])
exp(coef(hotelCancellationModel)["marketSegmentGroups"])
exp(coef(hotelCancellationModel)["marketOfflineTATO"])
exp(coef(hotelCancellationModel)["marketSegmentCorporate"])
exp(coef(hotelCancellationModel)["marketSegmentAviation"])
exp(coef(hotelCancellationModel)["bookingChanges"])
exp(coef(hotelCancellationModel)["customerTransient"])
exp(coef(hotelCancellationModel)["customerParty"])
exp(coef(hotelCancellationModel)["customerTypeGroup"])

# Use the model to predict the output in the testing dataset
hotelCancellationPrediction <- predict(hotelCancellationModel,
                                       hotelCancellationTesting,
                                       type = "response")

# Display hotelCancellationPrediction on the console
print(hotelCancellationPrediction)

# Summary of hotelCancellationPrediction
summary(hotelCancellationPrediction)

# Treat anything below or equal to 0.5 as 0 and greater than 0.5 as 1
hotelCancellationPrediction <- 
  ifelse(hotelCancellationPrediction >=0.5,1,0)

# Create a Confusion Matrix
hotelCancellationConfusionMatrix <- 
  table(hotelCancellationTesting$isCanceled,
        hotelCancellationPrediction)

# Print the Confusion Matrix on Console
print(hotelCancellationConfusionMatrix)

# Calculate the False Positive Rate
hotelCancellationConfusionMatrix[1,2]/
  (hotelCancellationConfusionMatrix[1,2]
   +hotelCancellationConfusionMatrix[1,1])

# Calculate the False Negative Rate
hotelCancellationConfusionMatrix[2,1]/
  (hotelCancellationConfusionMatrix[2,2]
   +hotelCancellationConfusionMatrix[2,1])

# Calculate the prediction accuracy by dividing the number of true
# positives and true negatives by the total amount of predictions in the
# testing dataset
predictiveAccuracyLog <- sum(diag(hotelCancellationConfusionMatrix))/
  nrow(hotelCancellationTesting)

# Print predictive accuracy
print(predictiveAccuracyLog)
