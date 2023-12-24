# Q1

# create function my.median that takes a vector as input and returns the median of the vector
my.median <- function(x) {
  # sort the vector
  x <- sort(x)
  # find the length of the vector
  n <- length(x)

  # if the length is even, return the average of the two middle values
  if (n %% 2 == 0) {
    return((x[n / 2] + x[n / 2 + 1]) / 2)
  } else {
    # if the length is odd, return the middle value
    return(x[(n + 1) / 2])
  }
}

data <- read.table(file = "AS-N100.tsv", sep = "\t", header = TRUE)

a <- (301386847 %% 27) + 1 # generate a using SFU ID

# get symbol data from the data frame using the a-th row
symbol <- sort(unique(data$ticker))[a]

# print the symbol
print(paste("The symbol is", symbol))

# use my.median to calculate the median of the open price of the symbol
median <- my.median(data[data$ticker == symbol,]$open)

# print the result
print(paste("The median of the open price of", symbol, "is", median))

# Q2

# load libraries
library("rjson")
library("imager")

libraries <- fromJSON(file = "./libraries.json")
b <- (301386847 %% 21) + 1 # generate b using SFU ID
# load image of Vancouver map from Figure03.png
image <- load.image("./Figure03.png")

# get the name of the b-th library
name <- libraries[["features"]][[b]][["properties"]]$maptip

# get the co-ordinates of the b-th library
coords <- libraries[["features"]][[b]][["geometry"]]$coordinates

# print the result
print(paste("The", b, "th library is", name, "at", coords[2], "and", coords[1]))

# adjust co-ordinates to fit the image size and position of the map
# the co-ordinates are in the form of top left corner of the map (49.410705, -124.217671)
# and the bottom right corner of the map (47.929083, -121.994887)
# the image is 8094 x 8094 pixels
coords[2] <- (coords[2] - 49.410705) / (47.929083 - 49.410705) * 8094
coords[1] <- (coords[1] - (-124.217671)) / ((-121.994887) - (-124.217671)) * 8094

# print the result
print(paste("The", b, "th library is", name, "at abjusted", coords[2], "and", coords[1], "in the image."))

# display Figure03.png
plot(image, axes = FALSE, xlab = "", ylab = "")

# mark an ‘X’ symbol on the GPS coordinates of the b-th library branch
text(coords[1], coords[2], "X", col = "red")
# and to print the name and address of the library branch directly below the ‘X’
text(coords[1], coords[2] + 150, name, col = "red")