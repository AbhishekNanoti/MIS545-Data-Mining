# Malcolm Shepherd and Abhishek Nanoti
# MIS 545 Section 001
# Lab11Group09NanotiShepherd.R
# Using K means clustering algorithm to determine the relation between 
# Corruption index and Nmber of days to open business accoring to different 
# countries

# Pre-execution  -----------------------------------------------------
# Install the packages
# install.packages("tidyverse")
# install.packages("factoextra")

# Load the packages
library(tidyverse)
library(factoextra)
library(stats)
library(cluster)
library(gridExtra)

# Set the working directory
setwd(paste0("C:/Users/ual-laptop/Desktop/MIS-545"))

# Load the value from dwellingType.csv
countries <- read_csv(file = "CountryData.csv",
                         col_types = "cnnnnini",
                         col_names = TRUE)

# Display on console
print(countries)

# Structure of tibble
str(countries)

# Summary of tibble
summary(countries)

# Convert the column containing the country name to the row title of the tibble 
# this is a requirement for later visualizing the clusters
countries <- countries %>% column_to_rownames(var= "Country")

# Remove countries from the tibble with missing data in any feature
countries <- countries %>% drop_na()

# Summary of tibble
summary(countries)

# scale CorruptionIndex, DaysToOpenBusiness   so they have equal impact on the 
# clustering calculations
countriesScaled <- countries %>% 
    select(CorruptionIndex, DaysToOpenBusiness) %>% scale()

# Set seed to 679
set.seed(679)

# Generate K means Clusters
countries4Clusters <- kmeans(x = countriesScaled,
                             centers = 4,
                             nstart = 25)

# Display Cluster Sizes
countries4Clusters$size

# Display Cluster centers (z scores)
countries4Clusters$centers

# Visualize the Clusters
fviz_cluster(object = countries4Clusters,
             data = countriesScaled,
             repel = FALSE)

# Optimizing the value for k
# Elbow Method
fviz_nbclust(x = countriesScaled,
             FUNcluster = kmeans,
             method = "wss")

# Average Silhouette Method
fviz_nbclust(x = countriesScaled,
             FUNcluster = kmeans,
             method = "silhouette")

# Gap Statistic Method
fviz_nbclust(x = countriesScaled,
             FUNcluster = kmeans,
             method = "gap_stat")

# Regenerate the analysis using optimal number of Clusters
# Generate K means Clusters
countries3Clusters <- kmeans(x = countriesScaled,
                             centers = 3,
                             nstart = 25)

# Display Cluster Sizes
countries3Clusters$size

# Display Cluster centers (z scores)
countries3Clusters$centers

# Visualize the Clusters
fviz_cluster(object = countries3Clusters,
             data = countriesScaled,
             repel = FALSE)

# similarities and differences among the clusters using the remaining 
# features in the dataset
countries %>%
  mutate(cluster = countries3Clusters$cluster) %>%
  select(cluster,
         GiniCoefficient,
         GDPPerCapita,
         EduPercGovSpend,
         EduPercGDP,
         CompulsoryEducationYears) %>%
  group_by(cluster) %>%
  summarise_all("mean")




