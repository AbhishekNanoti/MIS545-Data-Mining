# Abhishek Sanjay Nanoti & Jonathan Chen
# MIS 545 Section 1
# Lab10Group04NanotiChen.R
# Lab 10 requires you to use R to import a dataset of Instacart grocery 
# transactions and generate association rules among items in a transaction. 
# Instacart is an online grocery ordering and delivery platform that works 
# with many grocery retailers. 

#Install tidyverse package
# install.packages("tidyverse")
# install.packages("arules")

# Load the tidyverse package using library()
library(tidyverse)
library(arules)

# set working directory to lab folder on my laptop
setwd(paste0("C:/Users/ual-laptop/Desktop/MIS-545"))

# Reading the InstacartTransactions.csv file 
# in InstacartTransactions tibble
instacartTransactions <- read.transactions(file = 'InstacartTransactions.csv',
                                           format = "single",
                                           header= TRUE, 
                                           sep=",",
                                           cols= c("OrderID","ItemID"))

# Display a summary of instacartTransactions
summary(instacartTransactions)

# Display the first three transactions from instacartTransactions on the console
inspect(instacartTransactions[1:3])

# Examine the frequency of a single item: 24852 (bananas).
itemFrequency(instacartTransactions[,"24852"])

# Convert the frequency values in instacartTransactions into a tibble called 
# instacartTransactionsFrequency
instacartTransactionsFrequency <- 
  tibble (Item= names(itemFrequency(instacartTransactions)),
          Frequency=itemFrequency(instacartTransactions))

# Display the item frequencies in instacartTransactionsFrequency on the console
print(instacartTransactionsFrequency)

# Display the 10 most frequently purchased items on the console
instacartTransactionsFrequency %>%
  arrange(desc(Frequency)) %>%
  slice(1:10)

# Generate the association rules model in an object called 
# instacartTransactionRules using the following parameters:
# Support of 0.005
# Confidence of 0.2
# Minimum length of 2
instacartTransactionsRules <- 
  apriori(instacartTransactions, 
          parameter = list(
            support= 0.005,
            confidence= 0.2,
            minlen= 2))

# Display a summary of instacartTransactionRules
summary(instacartTransactionsRules)

# Display the first 10 association rules
inspect(instacartTransactionsRules[1:10])

# Sort the association rules by lift and view the top 10
instacartTransactionsRules %>%
  sort(by="lift") %>%
  head(n=10) %>%
  inspect()
