# Lab 05
## Vaibhav Saini - 301386847

# Question 1

# import canada function from a r script
source("./canada.R")

data <- read.csv("./ebird30.csv", header = TRUE)

data_names <- read.csv("./names.csv", header=TRUE)

result <- data.frame()

## loop through the data and check if exactly one region has bird species 
for (i in 1:nrow(data)) {
  bird <- data [i, 2:ncol(data)]
  count <- 0
  for (j in 1:length(bird)) {
    if (bird[j] > 0) {
      count <- count + 1
    }
  }
  if (count == 1) {
    result <- rbind(result, data[i,])
  }
}

sprintf("Total numbers of bird species that have been spotted in only one region of Canada: %d",nrow(result))

canada('Total numbers of bird species', result)

# get sum of all columns 2:end (except the first column)
data$sum <- apply(data[,2:ncol(data)], 1, sum)

# find row with max sum
max_row <- which(data$sum == max(data$sum))

# get data for that row
max_data <- data[max_row,]

# get name of the bird with max sum
max_name <- data_names[which(data_names$Code == max_data$Code),]

#print names
sprintf("The common and scientific names for the bird with highest count %d are: %s and %s respectively",max_data$sum,max_name$Common.Name,max_name$Scientific.Name )

# get min sum not equal to 0
min_sum <- min(data[data$sum != 0,]$sum)

# filter out rows with sum equal to min sum
data_min <- data[data$sum == min_sum,]

# merge min data with names using Code
data_min <- merge(data_min, data_names, by = "Code")

# sort data by common name in ascending order
data_min <- data_min[order(data_min$Common.Name),]

sprintf("Total number of bird species tied for non-zero min count are: %d", nrow(data_min))

# print top 10 common names with min sum
sprintf("Top 10 common names with min sum are:")
print(data_min$Common.Name[1:10])

## Bonus Question

# import libraries
library(RSQLite)
library(dplyr)
library(lubridate)

# create database connection
db = dbConnect(SQLite(), dbname = "xcoretail2.sqlite")

# read sales table
data_sales = dbReadTable(db, "sales")

# convert InvoiceDate to date format
data_sales$InvoiceDate <- as.Date(data_sales$InvoiceDate, format = "%Y-%m-%d")

# read prices table
data_prices = dbReadTable(db, "prices")

# Write SOP(code, month) function to perform standard operating procedure
my.SOP <- function(code, month) {
  # generate previous month using the lubridate package
  prev_month <- substr(as.character(ym(month) - months(1)), start = 1, stop = 7)
  
  # create a vector to store the quantity of the product
  count <- NA
  
  # create a query to select the total price of the product in the month of interest
  query <- paste0("SELECT SUM(Quantity) FROM sales WHERE StockCode = '", code, "' AND InvoiceDate LIKE '", prev_month, "%'")
  
  # send the query to the database
  request <- dbSendQuery(db, query)
  
  # fetch the result
  count <- strtoi(dbFetch(request))
  
  # clear the result
  dbClearResult(request)
  
  # return demand
  return (count)
}

# define my.lm function for regression
#my.lm that takes a vector x and a vector y and returns the slope a and y-intercept b of the line fpxq “ ax ` b that minimizes the sum of squared errors between fpxiq and yi
my.lm <- function(x, y) {
  b <- sum((x-mean(x)) * (y-mean(y))) / sum((x-mean(x))^2)
  a <- mean(y) - b * mean(x)
  return(list(a = a, b = b))
}

# define LIN function from lab 04
my.LIN <- function(code, month) {
  
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
  #plot(c(1, 2, 3), counts[-4], main = paste0("Prediction for Product ", code, ": ", month), ylab = "Units", xlab = "Month", pch = 19, xlim = c(0, 5), ylim = c(counts[1], counts[4]))
  
  # plot the total units product in the month of interest
  #points(4, counts[4], pch = 3)
  
  # plot the line for the Linear regression
  #abline(a = res$a, b = res$b)
  
  return(counts[4])
}

## Get 100 most popular products
# get stockCode and quantity from sales table
data_pop = data_sales %>% select(StockCode, Quantity)

# group by stockCode and sum quantity
data_pop = data_pop %>% group_by(StockCode) %>% summarise(Quantity = sum(Quantity))

# get top 100 popular items
data_pop = data_pop %>% arrange(desc(Quantity)) %>% head(100)

#get a list of dates from the sales table
data_dates = data_sales %>% select(InvoiceDate) %>% distinct() %>% mutate(date = InvoiceDate) %>% select(date) %>% arrange(date)

# format dates to yyyy-mm
data_dates$date = format(data_dates$date, "%Y-%m")

# get unique dates
data_dates = data_dates %>% distinct()

# remove top 3 entries from dates (Valid Months)
data_dates = data_dates[-c(1:3),]

# create vector elin and esop to store difference 
elin = c()
esop = c()

# loop through each stockCode in data_pop and get predicted sales for each month
for (i in 1:nrow(data_pop)) {
  # get stockCode
  StockCode = data_pop$StockCode[i]
  
  # loop thru dates
  for (tMonth in data_dates) {
    
    # get stockCode
    StockCode = data_pop$StockCode[1]
    
    tMonth <- data_dates[1]
    
    # get LIN prediction for that month
    pred_lin <- my.LIN(StockCode, tMonth)
    
    # get SOP prediction for that month
    pred_sop <- my.SOP(StockCode, tMonth)
    
    # create filtration date in yyyy-mm-dd 
    filter_date <- as.Date(paste(tMonth, "-01", sep=""), format = "%Y-%m-%d")
    
    # get true quantity sold of the stockCode and month
    true_data <- data_sales[data_sales$StockCode == StockCode & data_sales$InvoiceDate >= filter_date & data_sales$InvoiceDate < filter_date + months(1),]
    
    # get sum of quantity
    true <- sum(true_data$Quantity)
    
    # get absolute difference between LIN prediction and true
    diff_lin <- abs(pred_lin - true)
    
    # check is difference is NA
    if (is.na(diff_lin)) {
      diff_lin <- 0
    }
    
    # get difference between LIN prediction and true
    diff_sop <- abs(pred_sop - true)
    
    # check is difference is NA
    if (is.na(diff_sop)) {
      diff_sop <- 0
    }
    
    # append differences
    elin = c(elin, diff_lin)
    esop = c(esop, diff_sop)
  }
}
