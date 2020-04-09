# ===========================================================================
# Title      : analysis.R 
# Objective  : Detect different shapes over the stock prices.
# Created by : Katerina Stojanova
# Created on : 09.04.20
# ===========================================================================
#
# Detect a U-shape in the patterns
#
detectUShape <- function (company_data, startDate, endDate) {
  #
  # Set the variables to detect the pattern
  #
  index  <- index(company_data);
  stocks <- company_data[startDate < index & index < endDate];
  result <- NULL;
  length <- nrow(stocks);
  
  if (length >= 3) {
    #
    # Find the minimum index
    #
    minIdx <- 1 + which.min(stocks[2 : (length - 1), 1]);
    #
    # Partition the period as pre and post-crisis
    #
    before <- stocks[1 : (minIdx - 1)];
    after  <- stocks[(minIdx + 1) : length];
    #
    # Find the maxes in the two parititions
    #
    maxIdxBefore <- which.max(before[, 1]);
    maxIdxAfter  <- minIdx + which.max(after[, 1]);
    #
    # Extract the values from the xts row
    #
    maxBefore <- as.numeric(stocks[maxIdxBefore, 1]);
    min       <- as.numeric(stocks[minIdx, 1]);
    maxAfter  <- as.numeric(stocks[maxIdxAfter, 1]);
    #
    # Sanity checks
    #
    if (maxBefore > min && min < maxAfter) {
      result <- list (
        maxBefore = stocks[maxIdxBefore,],
        min       = stocks[minIdx, ],
        maxAfter  = stocks[maxIdxAfter, ]
      );  
    }
  } 
  return(result);
}

detectLShape <- function (company_data, startDate, endDate) {
  #
  # Set the variables to detect the pattern
  #
  index  <- index(company_data);
  stocks <- company_data[startDate < index & index < endDate];
  result <- NULL;
  length <- nrow(stocks);
  
  if (length >= 2) {
    #
    # Find the minimum index
    #
    minIdx <- 1 + which.min(stocks[2 : length, 1]);
    #
    # Find the maximum, before the minum
    #
    before <- stocks[1 : (minIdx - 1)];
    maxIdx <- which.max(before[, 1]);
    #
    # Extract the values from the xts row
    #
    maxPrice <- as.numeric(stocks[maxIdx, 1]);
    minPrice <- as.numeric(stocks[minIdx, 1]);
    #
    # Sanity checks
    #
    if (maxIdx < minIdx && maxPrice > minPrice) {
      result <- list (
        max = stocks[maxIdx, ],
        min = stocks[minIdx, ]
      );  
    }
  } 
  return(result);
}