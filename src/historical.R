# ===========================================================================
# Title      : historical_data.R 
# Objective  : Auxilary functions to help us download and cache ticker data.
# Created by : Katerina Stojanova
# Created on : 09.04.20
# ===========================================================================
#
if (!require("pacman")) install.packages("pacman")
pacman::p_load(quantmod)
#
# The represenation of a company used accross this project
#
Company <- setClass("Company", slots=list(symbol="character", name="character", data="xts"))
#
# getHistoricalData will download the historical data for a given ticker symbol. 
# Once downloaded, it will be stored on diskl.
#
getHistoricalData <- function (symbol, name = "", useDelay = FALSE) {
    #
    # Setup the location for our data. All historical data will be stored
    # in the same directory of this script in the ./timeseries relative path
    #
    company        <- NULL;
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
        prices  <- readRDS(file = pathSymbolFile)
        company <- Company(symbol = symbol, name = name, data = prices);
    } else {
        #
        # If not, make an attempt to load the data from quantmod
        #
        print(paste("Loading symbol", symbol, "from quantmod"));
        startTime <- Sys.time();
        tryCatch ({
            prices <- getSymbols(symbol, warnings = FALSE, auto.assign = FALSE);
            saveRDS(prices, file = pathSymbolFile);
            company <- Company(symbol = symbol, name = name, data = prices);
        }, error = printLog, warning = printLog, finally = {
            timeTaken <- Sys.time() - startTime;
            printLog(paste("Loading complete, took", timeTaken, "seconds"));
        });
        #
        # A hack to introduce a virtual delay, for data downloading
        #
        if (useDelay) {
            startTime   <- as.numeric(startTime)
            currentTime <- as.numeric(Sys.time());
            while (currentTime - startTime < 0.5) {
                currentTime <- as.numeric(Sys.time());
            }
        }
    }
    
    return(company);
} 
#
# Read the market symbols for a given CSV indicated with the marketName
#
getMarketSymbols <- function(marketName) {
    pathSymbols <- file.path(getwd(), "symbols", paste(marketName, ".csv", sep=""));
    symbols <- read.csv(pathSymbols);
    return(symbols);
}

#
# Obtain the data for a given market name
#
downloadData <- function(marketName, useDelay = FALSE) {
    # 
    # Setup the location for the market data
    #
    pathTimeSeries <- file.path(getwd(), "market")
    pathMarketFile <- file.path(pathTimeSeries, paste(marketName, ".Rda", sep=""))
    result         <- NULL;  
    symbols        <- getMarketSymbols(marketName);
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
        result <- readRDS(file = pathMarketFile)
    } else {
        #
        # If not, obtain the historical data for each company
        #
        marketData <- lapply(1:nrow(symbols), function(i) {
            symbol  <- toString(symbols$Symbol[i]);
            name    <- toString(symbols$Company.Name[i])
            company <- getHistoricalData(symbol, name, useDelay);
            return (company);
        });
        #
        # Filter the ones that are available only
        #
        result <- result[lapply(result, is.null) == 0]
        saveRDS(result, file = pathMarketFile)
        
    }
    
    print(sprintf("%10d companies are listed on the %s market", nrow(symbols), marketName));
    print(sprintf("%10d companies are are available on quantmod", length(result)));
    return(result);
}
#
# Create a plot chart from the given data
#
plotHistoricalData <- function(company) {
    plot <- barChart(company@data)
    return(plot);
}