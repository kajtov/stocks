# ===========================================================================
# Title      : utils.R 
# Objective  : Utilities functions.
# Created by : Katerina Stojanova
# Created on : 09.04.20
# ===========================================================================
#
# Enable or disable logging
#
loggingEnabled <- TRUE;
printLog <- function (msg) {
  if (loggingEnabled) {
    print(msg);
  }
}
#
# Check whether a variable is non-null
#
is.nonNull <- function(x) {
  !is.null(x)
}
#
# Cleanup after the rbind call
#
reparseData <- function(initial_df) {
  
  df               <- data.frame(initial_df);
  df$symbol        <- as.character(df$symbol);
  
  df$beforeDate    <- as.character(df$beforeDate);
  df$crisisDate    <- as.character(df$crisisDate);
  df$afterDate     <- as.character(df$afterDate);
  df$latestMaxDate <- as.character(df$latestMaxDate);
  df$latestMinDate <- as.character(df$latestMinDate);
  
  # df$beforeDate    <- as.character(df$beforeDate, "%Y-%m-%d");
  # df$crisisDate    <- as.Date(df$crisisDate);
  # df$afterDate     <- as.Date(df$afterDate);
  # df$latestMaxDate <- as.Date(df$latestMaxDate);
  # df$latestMinDate <- as.Date(df$latestMinDate);
  
  df$before        <- as.numeric(df$before);
  df$crisis        <- as.numeric(df$crisis);
  df$after         <- as.numeric(df$after);
  df$latestMax     <- as.numeric(df$latestMax);
  df$latestMin     <- as.numeric(df$latestMin);
  
  df$stockFitness  <- as.numeric(df$stockFitness);
  
  return(df);
}