#import libraries
library(flexclust)
library(ggplot2)
library(dplyr)
library(tidyr)


# Question 1

## Part a

# write dist2 function without flexclust
# function takes in 2 dataframes with N and K rows
my.dist2 <- function(df1, df2) {
  n <- nrow(df1)
  k <- nrow(df2)

  x <- matrix(rnorm(n * k), n, k)
  d <- matrix(0, n, n)
  for (i in 1:n) {
    for (j in 1:n) {
      d[i, j] <- sum((x[i, ] - x[j, ])^2)
    }
  }
  return(d)
}

## Part b

# write my.kmeans function
my.kmeans <- function(x, k, dist) {
  n <- nrow(x)
  d <- matrix(0, n, n)
  for (i in 1:n) {
    for (j in 1:n) {
      d[i, j] <- sum((x[i, ] - x[j, ])^2)
    }
  }
  cl <- kmeans(x, k, dist = 0)
  return(cl)
}

# write kmeans function
# the function takes data df, max number of iterations iter, distance threshold tres
my.kmeans <- function(df, iter, tres) {
  # initialize cluster centers
  c1 <- df[1, ]
  c2 <- df[2, ]
  # initialize cluster assignment
  cl <- rep(1, nrow(df))
  # initialize distance matrix
  d <- matrix(0, nrow(df), nrow(df))
  # initialize counter
  i <- 0
  # initialize distance
  dist <- 1
  # while loop
  while (i < iter && dist > tres) {
    # calculate distance matrix using dist2 function
    d <- my.dist2(df, df)
    # assign cluster
    for (i in 1:nrow(df)) {
      if (d[i, 1] < d[i, 2]) {
        cl[i] <- 1
      } else {
        cl[i] <- 2
      }
    }
    # calculate new cluster centers
    c1 <- colMeans(df[cl == 1, ])
    c2 <- colMeans(df[cl == 2, ])
    # calculate distance between old and new cluster centers
    dist <- sum((c1 - c2)^2)
    # increase counter
    i <- i + 1
  }
  return(cl)
}

# run kmeans function
cl <- my.kmeans(df, 100, 0.0001)

# plot
plot(df, col = cl)

## Part c

k <- 7 # number of clusters
k <- k + 1 # add 1 to k

# import Calm code data for 8 clusters
df <- read.csv("./labData/data.csv", header = TRUE)

# run my.kmeans function
my.cl <- my.kmeans(df, 100, 0.0001)

# run my.kmeans function
cl <- kmeans(df, 100, 0.0001)