# import libraries
library(stringr)

# Problem 1

## Part a
regX_a <- "^[1-8]*9[1-8]*9[1-8]*9[1-8]*$";

str_match(03999, regX_a);
str_match(3999999, regX_a);


## Part b
regX_b <- '<TAG\\s+FLAG\\s*=\\s*\"[^\\\"]*\">';


## Part c
extract.flag <- function(tag, flag, input_string) {
  # Define the regular expression
  regex <- paste0("<", tag, "\\s+", flag, "\\s*=\\s*\"([^\\\"]*)\">")
  
  # Use str_extract to extract the matching string
  result <- str_extract(input_string, regex)
  
  # Check if a match was found
  if (!is.na(result)) {
    # Return the matching string
    return(str_match(result, regex)[,2])
  } else {
    # Return NA if no match was found
    return(NA)
  }
}


## Part d
#  Write a regular expression that matches a positive integer without leading 0s, such that at least one digit is repeated at least once or more
regX_d = "([1-9][0-9]*)(\\1)+";

# Bonus Question 
extract.indel <- function(dna_substrs) {
  # Find the shortest substring length
  min_length <- min(nchar(dna_substrs))
  
  # Iterate over possible indel lengths
  for (i in min_length:1) {
    # Get the indel candidate
    indel_candidate <- substr(dna_substrs[1], 1, i)
    
    # Check if the indel candidate explains all substrings
    if (all(grepl(paste0("^", indel_candidate, "+$"), dna_substrs))) {
      # Count the number of repeats of the indel for each substring
      repeats <- sapply(dna_substrs, function(x) nchar(x) / nchar(indel_candidate))
      
      # Return the indel and the repeats
      return(list(indel = indel_candidate, repeats = repeats))
    }
  }
  
  # Return NA if no indel was found
  return(list(indel = NA, repeats = rep(NA, length(dna_substrs))))
}
