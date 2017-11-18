library(zoo)
library(xts)
library(dplyr)
library(quantmod)
library(PerformanceAnalytics)
library(readr)

#function: convert data type of char to double but keep Date as char, then tranform to xts object
csv2xts <- function(x){
  Date_temp <- x$Date
  x$Date <- NULL
  x <- sapply(x, as.numeric)
  x <- as.data.frame(x)
  Date_temp <- as.Date(Date_temp)
  rownames(x) <- Date_temp
  x <- as.xts(x)
  na.locf(x)
  return(x)
  
}

#loading database

Nomura_Return_All <- read_csv("C:/Users/tungl/OneDrive/文件/GitHub/Nomura-TW-fund/Nomura_Return_All.csv")
Nomura_Shape <- read_csv("C:/Users/tungl/OneDrive/文件/GitHub/Nomura-TW-fund/Nomura_Sharp.csv")
index_all <- read_csv("C:/Users/tungl/OneDrive/文件/GitHub/Nomura-TW-fund/index.csv")


#covert to xts objects
Nomura_Return_All <- csv2xts(Nomura_Return_All)
Nomura_Shape <- csv2xts(Nomura_Shape)
index_all <- csv2xts(index_all)

#covert numeric return to % return
Nomura_Return_All <- Nomura_Return_All / 100
index_all <- index_all / 100







