#
# Data Wrangling Exercise 1: Basic Data Manipulation
# Springboard Data Science course
# 08.10.2016 - HC
# 2nd version - b (final version)
#
# =====================================================
#
# tidyr library for separate()
# install.packages("tidyr")
# library(tidyr)

# set working directory
# getwd()
# setwd("/Users/HC/Documnets/Springboard/Exercises/Data Wrangling 1")
#

# 0: Read csv file to load data in RStudio
refine_df <- read.csv("refine_original.csv", header = TRUE, sep = ";")

# 1: Clean up brand names (lowercase and misspellings on 'company' column)
refine_df$company <- tolower(refine_df$company)
refine_df$company <- gsub(pattern = "ak zo", replacement = "akzo", refine_df$company)
refine_df$company <- gsub(pattern = "akz0", replacement = "akzo", refine_df$company)
refine_df$company <- gsub(pattern = "phillips", replacement = "philips", refine_df$company)
refine_df$company <- gsub(pattern = "fillips", replacement = "philips", refine_df$company)
refine_df$company <- gsub(pattern = "phlips", replacement = "philips", refine_df$company)
refine_df$company <- gsub(pattern = "phllips", replacement = "philips", refine_df$company)
refine_df$company <- gsub(pattern = "phillps", replacement = "philips", refine_df$company)
refine_df$company <- gsub(pattern = "unilver", replacement = "unilever", refine_df$company)

# 2: Separate product code and number into separate columns
refine_df <- separate(refine_df, Product.code...number, c("product_code", "product_number"), sep = "-")

# 3: Add product categories
# ... Copy product_code column to a new columns (at the end, 8th column)
refine_df <- cbind(refine_df, refine_df[,2])
# ... Order the columns
refine_df <- refine_df[c(1, 2, 8, 3, 4, 5, 6, 7)]
# ... Name the new column, 3rd (product category)
names(refine_df)[3] <- "product_category"
# ... Replace product code by product categoyy
refine_df$product_category <- gsub(pattern = "p", replacement = "Smartphone", refine_df$product_category)
refine_df$product_category <- gsub(pattern = "v", replacement = "TV", refine_df$product_category)
refine_df$product_category <- gsub(pattern = "x", replacement = "Laptop", refine_df$product_category)
refine_df$product_category <- gsub(pattern = "q", replacement = "Tablet", refine_df$product_category)

# 4: Add full address for geocoding
refine_df$full_address <- paste(refine_df$address, refine_df$city, refine_df$country, sep = ",")

# 5: Create and update dummy columns for company and product category
refine_df$company_philips <- ifelse(refine_df[,1] == "philips", refine_df$company_philips <- 1, refine_df$company_philips <- 0)
refine_df$company_akzo <- ifelse(refine_df[,1] == "akzo", refine_df$company_akzo <- 1, refine_df$company_akzo <- 0)
refine_df$company_van_houten <- ifelse(refine_df[,1] == "van houten", refine_df$company_van_houten <- 1, refine_df$company_van_houten <- 0)
refine_df$company_unilever <- ifelse(refine_df[,1] == "unilever", refine_df$company_unilever <- 1, refine_df$company_unilever <- 0)
refine_df$product_smartphone <- ifelse(refine_df[,3] == "Smartphone", refine_df$product_smartphone <- 1, refine_df$product_smartphone <- 0)
refine_df$product_tv <- ifelse(refine_df[,3] == "TV", refine_df$product_tv <- 1, refine_df$product_tv <- 0)
refine_df$product_laptop <- ifelse(refine_df[,3] == "Laptop", refine_df$product_laptop <- 1, refine_df$product_laptop <- 0)
refine_df$product_tablet <- ifelse (refine_df[,3] == "Tablet", refine_df$product_tablet <- 1, refine_df$product_tablet <- 0)

# 6. Write the cleaned up data as a CSV file called refine_clean.csv
write.csv(refine_df, file = "refine_clean.csv")
