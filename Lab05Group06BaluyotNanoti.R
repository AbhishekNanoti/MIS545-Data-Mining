# Michael Baluyot and Abhishek Sanjay Nanoti
# MIS 545 Section 001-02
# Lab05Group06BaluyotNanoti.R
# This code is meant to provide in-depth analysis on the ZooVisitSpending.csv
# dataset provided in MIS545 to extract meaningful information and 
# visualizations by understanding correlation values and creating linear 
# regression models.

# Pre-execution  --------------------------------------------------------
# Install the tidyverse, corrplot, and olsrr packages  
#install.packages("corrplot")
#install.packages("olsrr")
# install.packages("tidyverse")
#install.packages("smotefamily")

# Load the tidyverse, corrplot, and olsrr libraries
library(tidyverse)
library(corrplot)
library(olsrr)
#library(smotefamily)

# Set the working directory to your Lab05 folder
setwd(paste0("C:/Users/ual-laptop/Desktop/MIS-545"))

# Read ZooVisitSpending.csv into a tibble called zooSpending 
zooSpending <- read_csv(file = "ZooVisitSpending.csv",
                        col_types = "niil",
                        col_names = TRUE )

# Analyze the dataset  --------------------------------------------------
# Display zooSpending in the console
print(zooSpending)

# Display the structure of zooSpending in the console
str(zooSpending)

# Display the summary of zooSpending in the console
summary(zooSpending)

# Recreate the displayAllHistograms() function as shown in the video 
# demonstration.
displayAllHistograms <- function(tibbleDataset) {
  tibbleDataset %>%
    keep(is.numeric) %>%
    gather() %>%
    ggplot() + geom_histogram(mapping = aes(x=value,fill=key),
                              color = "black") +
    facet_wrap (~ key, scales = "free") +
    theme_minimal ()
}

# Call the displayAllHistograms() function, passing in zooSpending as an 
# Argument
displayAllHistograms(zooSpending)

# Display a correlation matrix of zooSpending rounded to two decimal places
round(cor(zooSpending),2)

# Display a correlation plot using the "number" method and limit output to the 
# bottom left
corrplot(cor(zooSpending),
         method = "number",
         type = "lower")

# Generate the linear regression model and save it in an object called 
zooSpendingModel <- lm (data = zooSpending,
                        formula = VisitSpending ~ .)

# Display the beta coefficients for the model on the console
print(zooSpendingModel)

# Display the linear regression model results using the summary() function
summary(zooSpendingModel)

# Test for multicollinearity using the ols_vif_tol() function
ols_vif_tol(zooSpendingModel)
