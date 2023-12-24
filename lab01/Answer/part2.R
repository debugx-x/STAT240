## Lab 1 - Vaibhav saini - 301386847
# Q2
# load the library xlsx to read the excel file
library("xlsx")

# read the data from the file datasets.xlsx into a variable dataset
dataset <- read.xlsx("./datasets.xlsx", sheet = 0, header = TRUE, encoding = "UTF-8")

#read the data from the file breast-cancer-wisconsin.data into a variable data
data <- read.table("./5/breast-cancer-wisconsin.data", header = FALSE, sep = ",", na.strings = "?")

# find mean of the 3rd column of data
tableMean <- mean(data[, 3], na.rm = TRUE)

# find median of the 3rd column of data
tableMedian <- median(data[, 3], na.rm = TRUE)

# find standard deviation of the 3rd column of data
tableSD <- sd(data[, 3], na.rm = TRUE)

# find min of the 3rd column of data
tableMin <- min(data[, 3], na.rm = TRUE)

# find max of the 3rd column of data
tableMax <- max(data[, 3], na.rm = TRUE)

# print the mean, median, standard deviation, min and max of the 3rd column of data upto 3 decimal places
sprintf("Mean: %.3f, Median: %.3f, Standard Deviation: %.3f, Min: %.3f, Max: %.3f", tableMean, tableMedian, tableSD, tableMin, tableMax)