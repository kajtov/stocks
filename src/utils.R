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
  df           <- data.frame(initial_df);
  df$symbol    <- df$symbol;
  df$before    <- as.numeric(df$before);
  df$crisis    <- as.numeric(df$crisis);
  df$after     <- as.numeric(df$after);
  df$latestMax <- as.numeric(df$latestMax);
  df$latestMin <- as.numeric(df$latestMin);
  return(df);
}