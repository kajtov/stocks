# ===========================================================================
# Simple stock analysis.
# ===========================================================================
#
# install.packages("Quandl");
# install.packages("ggplot2");
# install.packages("ggrepel");
# install.packages("extrafont");
# install.packages("quantmod");
# install.packages("readtext");
library(readtext);
library(quantmod);
library(Quandl);
library(ggplot2);
library(scales);
library(extrafont);
#
# Clear all objects (including the hidden ones), and free up memory
#
rm(list = ls(all.names = TRUE));
gc();
#
# A quick hack: Setup the abosolute path of wherhere this file script
# is located. Also, inclue an API key, obtained from Quandl (to freshly
# download stock data).
#
setwd(file.path(path.expand("~"), "workspace", "stocks"));
Quandl.api_key(toString(readtext("quandl-api-key.txt")));
#
# Load the other sources
#
source(file.path(getwd(), "src", "plotting.R"));
source(file.path(getwd(), "src", "utils.R"));
#
# The represenation of historical data used accross this project
#
HistoricalData <- setClass("HistoricalData", slots=list(symbol="character", name="character", data="data.frame"))

getHistoricalData <- function (symbol) {
    #
    # Setup the location for our data. All historical data will be stored
    # in the same directory of this script in the ./timeseries relative path
    #
    historicalData <- NULL;
    pathTimeSeries <- file.path(getwd(), "timeseries")
    pathSymbolFile <- file.path(pathTimeSeries, paste(symbol, ".Rda", sep=""))
    #
    # Make sure that the folder where we store data exists
    #
    if (!dir.exists(pathTimeSeries)) {
        printLog(paste("Creating data directory at:", pathTimeSeries));
        dir.create(pathTimeSeries);
    }
    if (file.exists(pathSymbolFile)) {
        #
        # If the file exists, then load the historical data from file
        #
        printLog(paste("Loading symbol", symbol, "from disk:", pathSymbolFile));
        timeseriesData <- readRDS(file = pathSymbolFile)
        historicalData <- HistoricalData(symbol = symbol, name = "", data = timeseriesData);
    } else {
        #
        # If not, make an attempt to load the data from Quandl
        #
        print(paste("Loading symbol", symbol, "from Quandl"));
        currentTime <- Sys.time();
        tryCatch ({
            timeseriesData <- Quandl(paste("WIKI/", symbol, sep = ""));
            saveRDS(timeseriesData, file = pathSymbolFile);
            historicalData <- HistoricalData(name = symbol, data = timeseriesData);
        }, error = printLog, warning = printLog, finally = {
            timeTaken <- Sys.time() - currentTime;
            printLog(paste("Loading complete, took", timeTaken, "seconds"));
        });
    }
    return(historicalData);
} 

getMarketSymbols <- function(marketName) {
    pathSymbols <- file.path(getwd(), "symbols", paste(marketName, ".csv", sep=""));
    symbols <- read.csv(pathSymbols);
    return(symbols);
}

#
# Obtain the data for a given market name
#
downloadData <- function(marketName) {
    # 
    # Setup the location for the market data
    #
    marketData     <- NULL;  
    pathTimeSeries <- file.path(getwd(), "market")
    pathMarketFile <- file.path(pathTimeSeries, paste(marketName, ".Rda", sep=""))
    #
    # Make sure that the folder where we store data exists
    #
    if (!dir.exists(pathTimeSeries)) {
        printLog(paste("Creating data directory at:", pathTimeSeries));
        dir.create(pathTimeSeries);
    }
    if (file.exists(pathMarketFile)) {
        #
        # If the market data file exists, then load it from the file
        #
        printLog(paste("Loading market data for", marketName, "from disk:", pathMarketFile));
        marketData <- readRDS(file = pathMarketFile)
    } else {
        #
        # If not, load the data pre company
        #
        symbols <- getMarketSymbols(marketName);
        size    <- nrow(symbols);
        #
        # Obtain the historical data for each company
        #
        result <- lapply(1:size, function(i) {
            symbol <- toString(symbols$Symbol[i]);
            hd <- getHistoricalData(symbol);
            return(hd);
        });
        #
        # Filter the ones that are available only
        #
        result <- result[lapply(result, is.null) == 0]
        saveRDS(result, file = pathMarketFile)
        return(result);
    }
}


companies      <- downloadData("nasdaq")

reccessionAnalysis <- function (company) {
  #
  # Let's set some assumptions
  #
  startDate <- "2007-06-01";
  endDate   <- "2010-06-01";
  #
  # Get the data for each company, order it by date, and focus on the
  # recession crirys in 2008
  #
  company_data <- company@data[order(company@data$Date), ];
  df <- data.frame(company_data);
  df <- subset(df, Date > startDate & Date < endDate);
  #
  # Order the data of stocks by date in ascending order (just in case)
  #
  n  <- nrow(df)
  if (n < 3) return(NULL);
  #
  # Iterate backward and find the right maximum
  #
  rmax_idx_array    <- vector(mode="numeric", length=n)
  rmax_idx_array[n] <- n;
  for(i in (n-1):1) {
    rmax_idx <- rmax_idx_array[i + 1];
    rmax <- as.numeric(df[rmax_idx, ]$Open);
    curr <- as.numeric(df[i, ]$Open);
    if (rmax < curr) {
      rmax_idx_array[i] <- i;
    } else {
      rmax_idx_array[i] <- rmax_idx;
    }
  }
  #
  # Iterate forward and look for the left max and local minima
  #
  lmax_idx <- 1;
  lmin_idx <- 1;
  preCrisisHigh   <- NULL;
  crisisLow       <- NULL;
  postCrisisHigh  <- NULL;
  
  for (i in 2:(n-1)) {
    #
    # Find the right index
    #
    rmax_idx <- rmax_idx_array[i + 1];
    #
    # Now compute the rest of the values
    #
    lmax <- as.numeric(df[lmax_idx, ]$Open);
    lmin <- as.numeric(df[lmin_idx, ]$Open);
    rmax <- as.numeric(df[lmin_idx, ]$Open);
    curr <- as.numeric(df[i, ]$Open);
    #
    # We have reached, yet another local minima
    #
    if (curr < lmin) {
      preCrisisHigh  <- df[lmax_idx,];
      crisisLow      <- df[i, ]
      postCrisisHigh <- df[rmax_idx,];
      # difference <- curr / lmin * 100;
    }
    
    if (curr < lmin) lmin_idx <- i;
    if (curr > lmax) lmax_idx <- i;
  }
  
  # print(preCrisisHigh);
  # print(crisisLow);
  # print(postCrisisHigh);
  
  result <- list (
    symbol          = company@symbol,
    
    preCrisisDate   = preCrisisHigh$Date,
    crisisDate      = crisisLow$Date,
    postCrisisDate  = postCrisisHigh$Date,
    
    preCrisis       = as.numeric(preCrisisHigh$Open),
    crisis          = as.numeric(crisisLow$Open),
    postCrisis      = as.numeric(postCrisisHigh$Open),
    
    difference      = as.numeric(preCrisisHigh$Open) - as.numeric(crisisLow$Open),
    relative        = (1 - as.numeric(crisisLow$Open) / as.numeric(preCrisisHigh$Open)) * 100,
    recovery        = preCrisisHigh$Open <= postCrisisHigh$Open,
    
    latest          = as.numeric(company_data[nrow(company_data), ]$Open)
    
  );

  printLog(paste(
    company@symbol, 
    "Relative: ", round(result$relative, 2), 
    "Recovery: ", result$recovery, 
    "postCrisis: ", result$postCrisis,
    "latest: ", result$latest
  ));
  
  return(result);
}



analysis_list  <- lapply(companies, reccessionAnalysis)
analysis_filtered <- analysis_list[lapply(analysis_list, is.null) == 0]
analysis_df <- do.call(rbind, analysis_filtered)

analysis_df_corrected <- data.frame(analysis_df)
analysis_df_corrected$relative <- as.numeric(analysis_df_corrected$relative)
analysis_df_corrected <- subset(analysis_df_corrected, recovery == TRUE);
analysis_df_corrected <- analysis_df_corrected[order(-analysis_df_corrected$relative), ]


# analysis_df_corrected <- head(analysis_df_corrected, 100)
analysis_df_corrected$preCrisisDate <- as.Date(as.numeric(analysis_df_corrected$preCrisisDate))
analysis_df_corrected$crisisDate <- as.Date(as.numeric(analysis_df_corrected$crisisDate))
analysis_df_corrected$postCrisisDate <- as.Date(as.numeric(analysis_df_corrected$postCrisisDate))

analysis_df_corrected$postCrisis <- as.numeric(analysis_df_corrected$postCrisis)
analysis_df_corrected$latest <- as.numeric(analysis_df_corrected$latest)

analysis_df_corrected # <- subset(analysis_df_corrected, postCrisis < latest);

max(getHistoricalData("AAPL")@data$Open);

# getHistoricalData("AAOI");
# analyze(apple);


# df$Date <- as.Date(df$Date, format="%Y-%m-%d")
#subset(df, Date > "2014-12-03" & Date < "2014-12-05")
# df$Date <= "2018-03-20"
# plotHistoricalData(apple)












