# Robot advisor project

# import libraries

library(zoo)
library(xts)
library(dplyr)
library(quantmod)
library(PerformanceAnalytics)
library(readr)
library(lubridate)
library(readxl)
library(ggplot2)

# Set Chinese font family

windowsFonts(BL = windowsFont('微軟正黑體'))


# function: convert data type of char to double but keep Date as char, then tranform to xts object

data2xts <- function(x){
  Date_temp <- x$Date
  x$Date <- NULL
  x <- sapply(x, as.numeric)
  x <- as.data.frame(x)
  Date_temp <- as.Date(Date_temp)
  rownames(x) <- Date_temp
  x <- as.xts(x)
  return(x)
    
  }

# loading data from Nomura funds

Nomura_Return_All <- read_excel("C:/Users/tungl/OneDrive/文件/GitHub/Nomura-TW-fund/Nomura_Return_All-1.xlsx"
                                , sheet = 1, col_names = TRUE)
Nomura_Shape <- read_excel("C:/Users/tungl/OneDrive/文件/GitHub/Nomura-TW-fund/Nomura_Sharp-1.xlsx"
                                , sheet = 1, col_names = TRUE)
index_all <- read_excel("C:/Users/tungl/OneDrive/文件/GitHub/Nomura-TW-fund/index-1.xlsx"
                                , sheet = 1, col_names = TRUE)

# call fuction to convert to xts object

Nomura_Return_All <- data2xts(Nomura_Return_All)
Nomura_Shape <- data2xts(Nomura_Shape)
index_all <- data2xts(index_all)
 
# covert % return to numeric return

Nomura_Return_All <- Nomura_Return_All / 100
index_all <- index_all / 100

# function: calculate cumulative return

cumReturn <- function(x, start_date = "2006-12-29", end_date = "2017-10-31") {
    period <- paste0(start_date, "/", end_date)
    result <- Return.cumulative(x[period, ], geometric = TRUE)
    return(result)
}

# function: calculate annulized return

annReturn <- function(x, start_date = "2006-12-29", end_date = "2017-10-31" ) {
    period <- paste0(start_date, "/", end_date)
    result <- Return.annualized(x[period,], geometric = TRUE)
    return(result)
}

# function: calculate periodical return
# default frequency: "monthly"
# alternative frequency: "weekly", quarterly", "yearly"

periodReturn <- function(x, frequency = "monthly"){
    period = paste0("apply.",frequency,"(x, cumReturn)")
    result <- eval(parse(text = period))
    return(result)
}

# function: calculate up-to-date periodical return
# default period: 3 months
# unit of time: month
# example: past-6-month return >> histPerform(x, monthly_period = 6)

histPerform <- function(x, monthly_period = 3) {
    date <- as.POSIXlt(as.Date("2017-10-31"))
    date$mon <- date$mon - monthly_period
    result <-  cumReturn(x, start_date = date, end_date = end_date)
    return(result)
    
}

# function: calculate the portfolio returns of a fund of funds
# default rebalance_on = NA, or "years", "quarters", "months", "weeks", "days"
# portfolio return is calculated within the same period of time
# weights: a vector. ex: c(0.5, 0.5)
# verbose parameters: returns, contribution, EOP.Weight, BOP.Weight
#                     BOP.Value, EOP.Value (End of Period, Beginning of Period)

fofReturn <- function(x, weights, rebalance_on = NA){
    result <- Return.portfolio(x, weights = weights, rebalance_on  = rebalance_on,
                               verbose = TRUE)
    return(result)
}
