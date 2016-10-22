#
# Data Wrangling Exercise 2: Dealing with missing values
# Springboard Data Science course
# 22.10.2016 - HC
# 1st version - final
#
# =====================================================
#
# set working directory
# getwd()
# setwd("/.../Springboard/Exercises/Data Wrangling 2")
#

# 0: Read csv file to load data in RStudio
titanic_df <- read.csv("titanic_original.csv", header = TRUE, sep = ";", dec = ",")

# 1: Port of embarkation
# Find the missing values in embarked column and replace them with S
titanic_df$embarked <- gsub(pattern = "^$", replacement = "S", titanic_df$embarked)
titanic_df$embarked <- gsub(pattern = " ", replacement = "S", titanic_df$embarked)

# 2: Age
# Calculate the mean of the Age column and use that value to populate the missing values in Age column
titanic_df$age <- ifelse(is.na(titanic_df$age), mean(titanic_df$age, na.rm = TRUE), titanic_df$age)

# 3: Lifeboat
# Find the missing values in boat column and replace them with None
titanic_df$boat <- gsub(pattern = "^$", replacement = "None", titanic_df$boat)
titanic_df$boat <- gsub(pattern = " ", replacement = "None", titanic_df$boat)

# 4: Cabin
# Create a new column has_cabin_number which has 1 if there is a cabin number, and 0 otherwise.
titanic_df$has_cabin_number <- ifelse(titanic_df$cabin != "", 1, 0)

# 6. Write the cleaned up data as a CSV file called refine_clean.csv
write.csv(titanic_df, file = "titanic_clean.csv")