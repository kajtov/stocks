# ===========================================================================
# Title      : main.R 
# Objective  : Analyze the stock prices of crisis resilient companies
# Created by : Katerina Stojanova
# Created on : 09.04.20
# ===========================================================================
#
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  xts, readtext, quantmod, tidyquant
);
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
#
# Load the other sources
#
source(file.path(getwd(), "src", "historical.R"));
source(file.path(getwd(), "src", "analysis.R"));
source(file.path(getwd(), "src", "utils.R"));
#
# Perform per-company analysis
#
reccessionAnalysisPerCompany <- function (company) {
  result       <- NULL;
  company_data <- company@data;
  #
  # Detect a U-shape of pattern during the 2008 crisis and an
  # L-shape pattern during the pandemics
  #
  shapeU <- detectUShape(company_data, "2007-06-01", "2010-06-01");
  shapeL <- detectLShape(company_data, "2020-01-01", "2020-04-01");

  if (is.nonNull(shapeU) && is.nonNull(shapeL)) {
    #
    # If both shapes are detected, output the results
    #
    result <- list (
      symbol       = company@symbol,
      #
      # Filter the informations for the 2008 crisis
      #
      beforeDate   = toString(index(shapeU$maxBefore)),
      crisisDate   = toString(index(shapeU$min)),
      afterDate    = toString(index(shapeU$maxAfter)),
      before       = as.numeric(shapeU$maxBefore[,1]),
      crisis       = as.numeric(shapeU$min[,1]),
      after        = as.numeric(shapeU$maxAfter[,1]),
      #
      # Filter the informations for the pandemic crisis
      #
      latestMaxDate = toString(index(shapeL$max)),
      latestMinDate = toString(index(shapeL$min)),
      latestMax     = as.numeric(shapeL$max[,1]),
      latestMin     = as.numeric(shapeL$min[,1])
    );
  } 
  return(result);
}

recessionAnalysis <- function (companies) {
  #
  # Run the analysis for each company, and obtain the one that went through reccession
  #
  reccessionCompaniesList <- lapply(companies, reccessionAnalysisPerCompany);
  reccessionCompaniesList <- reccessionCompaniesList[lapply(reccessionCompaniesList, is.null) == 0];
  reccessionCompanies     <- reparseData(do.call(rbind, reccessionCompaniesList));
  #
  # Filter the ones that survived the crysis in 2008
  #
  resilientCompanies <- subset(reccessionCompanies, before < after);
  #
  # Filter the ones that are doing fine today
  #
  goodCompanies <- subset(resilientCompanies, after < latestMax);
  #
  # Print some statistics over the obtained data.
  #
  print(sprintf("%10d companies fit the pattern",              nrow(reccessionCompanies)));
  print(sprintf("%10d companies were resilient to the crisis", nrow(resilientCompanies)));
  print(sprintf("%10d companies are still doing fine",         nrow(goodCompanies)));
  #
  # Return the good companies
  #
  return(goodCompanies);
}
#
# Order by the biggest relative drop in the observed L-shape
#
orderByLShape <- function (df) {
  df$LShape <- (1 - df$latestMin / df$latestMax) * 100;
  df <- df[order(-df$LShape), ]
  return(df);
}
#
# Order by the biggest relative drop in the observed U-shape
#
orderByUShape <- function (df) {
  df$UShape <- (1 - df$crisis / df$after) * 100;
  df <- df[order(-df$UShape), ]
  return(df);
}


companies <- downloadData("nasdaq")
analysis  <- recessionAnalysis(companies);

orderByLShape(analysis);


