# Question 1

#my.lm that takes a vector x and a vector y and returns the slope a and y-intercept b of the line fpxq “ ax ` b that minimizes the sum of squared errors between fpxiq and yi
my.lm <- function(x, y) {
  b <- sum((x - mean(x)) * (y - mean(y))) / sum((x - mean(x)) ^ 2)
  a <- mean(y) - b * mean(x)
  return(list(a = a, b = b))
}

# create vector x and y of length 3
x <- c(3, 0, 1)
y <- c(8, 6, 8)

# call my.lm
res <- my.lm(x, y)

# print slope and y-intercept decimal values
sprintf("Slope: %.2f", res$a)
sprintf("Y-intercept: %.2f", res$b)

# Question 2

# import libraries
library(RSQLite)
library(dplyr)
library(lubridate)

# create database connection
db <- dbConnect(SQLite(), dbname = "xcoretail.sqlite")

# read sales table
data_sales <- dbReadTable(db, "sales")

# read prices table
data_prices <- dbReadTable(db, "prices")

# convert InvoiceDate to date format
data_sales$InvoiceDate <- as.Date(data_sales$InvoiceDate, format = "%Y-%m-%d")

# filter data_sales between 2011-01-01 and 2011-08-31
data_sales <- data_sales %>% filter(InvoiceDate >= "2011-01-01" & InvoiceDate <= "2011-08-31")

# join data_sales and data_prices on StockCode
data <- left_join(data_sales, data_prices, by = "StockCode")

# clean up data with description "" and UnitPrice 0
data <- data %>% filter(Description != "" & UnitPrice != 0)

# calculate total price
data$total_price <- data$Quantity * data$UnitPrice

# calculate total price per day
data_day <- data %>% group_by(InvoiceDate) %>% summarise(total_price = sum(total_price))

# calculate total price per month
data_month <- data %>% group_by(month = format(InvoiceDate, "%Y-%m")) %>% summarise(total_price = sum(total_price))

# convert yyyy-mm to mm
data_month$month <- as.numeric(substr(data_month$month, 6, 7))

# plot total price per month
plot(data_month$month, data_month$total_price, xlab = "Month", ylab = "Total Revenue (USD)", main = "Total Revenue per Month")

# Bonus Question
my.prodpredict <- function(code, month) {

  # generate a list of months 3 months before the month of interest using the lubridate package
  months <- c(substr(as.character(ym(month) - months(3)), start = 1, stop = 7), substr(as.character(ym(month) - months(2)), start = 1, stop = 7), substr(as.character(ym(month) - months(1)), start = 1, stop = 7))
  
  # create a vector of length 3 to store the total price of the product in the 3 months before the month of interest
  counts <- rep(NA, 3)
  
  # loop through the months
  for (i in 1:3) {
    
    # create a query to select the total price of the product in the month of interest
    query <- paste0("SELECT SUM(Quantity) FROM sales WHERE StockCode = '", code, "' AND InvoiceDate LIKE '", months[i], "%'")
    
    # send the query to the database
    request <- dbSendQuery(db, query)
    
    # fetch the result
    counts[i] <- strtoi(dbFetch(request))
    
    # clear the result
    dbClearResult(request)
  }
  
  # get the slope and y-intercept of the line of best fit
  res <- my.lm(c(1, 2, 3), counts)
  
  # calculate the total units product in the month of interest
  counts[4] <- res$a + res$b * 4
  
  # plot the total units product in the 3 months before the month of interest
  plot(c(1, 2, 3), counts[-4], main = paste0("Prediction for Product ", code, ": ", month), ylab = "Units", xlab = "Month", pch = 19, xlim = c(0, 5), ylim = c(counts[1], counts[4]))
  
  # plot the total units product in the month of interest
  points(4, counts[4], pch = 3)
  
  # plot the line for the Linear regression
  abline(a = res$a, b = res$b)
  
  return(counts[4])

}

# test predictions for example 

# Specify Stock code
scode <- '71053'

# Specify Month to be predicted for
smonth <- '2011-05'

res <- my.prodpredict(scode,smonth)

sprintf("Total units of Stock %s for the month %s that should be purchased are : %.2f", scode, smonth, res)
