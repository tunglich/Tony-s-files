#Robot advisor project

library(zoo)
library(xts)
library(dplyr)
library(quantmod)
library(PerformanceAnalytics)
library(readr)
library(lubridate)
library(readxl)
library(ggplot2)

#function: convert data type of char to double but keep Date as char, then tranform to xts object

csv2xts <- function(x){
  Date_temp <- x$Date
  x$Date <- NULL
  x <- sapply(x, as.numeric)
  x <- as.data.frame(x)
  Date_temp <- as.Date(Date_temp)
  rownames(x) <- Date_temp
  x <- as.xts(x)
  return(x)
    
  }

#loading data from Nomura funds

Nomura_Return_All <- read_excel("C:/Users/tungl/OneDrive/文件/GitHub/Nomura-TW-fund/Nomura_Return_All-1.xlsx"
                                , sheet = 1, col_names = TRUE)
Nomura_Shape <- read_excel("C:/Users/tungl/OneDrive/文件/GitHub/Nomura-TW-fund/Nomura_Sharp-1.xlsx"
                                , sheet = 1, col_names = TRUE)
index_all <- read_excel("C:/Users/tungl/OneDrive/文件/GitHub/Nomura-TW-fund/index-1.xlsx"
                                , sheet = 1, col_names = TRUE)

#call fuction to convert to xts object

Nomura_Return_All <- csv2xts(Nomura_Return_All)
Nomura_Shape <- csv2xts(Nomura_Shape)
index_all <- csv2xts(index_all)
 
#covert % return to numeric return

Nomura_Return_All <- Nomura_Return_All / 100
index_all <- index_all / 100

#function: calculate cumulative return

cumReturn <- function(x, start_date = "2006-12-29", end_date = "2017-10-31") {
    period <- paste0(start_date, "/", end_date)
    result <- Return.cumulative(x[period, ], geometric = TRUE)
    return(result)
}

#function: calculate annulized return

annReturn <- function(x, start_date = "2006-12-29", end_date = "2017-10-31" ) {
    period <- paste0(start_date, "/", end_date)
    result <- Return.annualized(x[period,], geometric = TRUE)
    return(result)
}


