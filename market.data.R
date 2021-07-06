#--------------------------------------------------------------------#
#---- Policy Agendas and Financial Markets -  Stock Market Data  ----#
#--------------------------------------------------------------------#


#### Packages and Working Directory ####

if(!require(rstudioapi)) install.packages("rstudioapi")
if(!require(quantmod)) install.packages("quantmod")
if(!require(TSstudio)) install.packages("TSstudio")
if(!require(lubridate)) install.packages("lubridate")


library(rstudioapi)
library(quantmod)
library(TSstudio)
library(lubridate)

source("return.and.volatility.R")          # script containing fuctions to calculate various return and volatility measures


this.files.path <- getActiveDocumentContext()$path
setwd(dirname(this.files.path))

#### Market Index Data #### 

min.year <- 1996
max.year <- 2006

# MSCI World Index Data - Excluding USA

msci.ex <- read.csv("Data/msci.world.ex.usa.csv", sep = ",")         # Loading MSCI World Index data from local file
colnames(msci.ex) <- c("date", "msci.world.ex")                     
msci.ex$date <- as.Date(msci.ex$date, "%b-%d-%Y")                 
msci.ex$year <- as.numeric(substr(as.character(msci.ex$date), 1,4))
msci.ex <- subset(msci.ex, year >= min.year & year <= max.year)

msci.ex$msci.return <- daily_return(msci.ex$msci.world.ex)

# SP500 Index Data
 
sp500 <- as.data.frame(getSymbols("^GSPC", src = "yahoo", from = "1996-01-01", to = "2006-12-31", auto.assign = FALSE)) #Getting S&P500 data
sp500$date <- as.Date(rownames(sp500))

vix <- as.data.frame(getSymbols("^VIX", src = "yahoo", from = "1996-01-01", to = "2006-12-31", auto.assign = FALSE))   # Getting VIX data
vix$date <- as.Date(rownames(vix))

sp500 <- cbind(sp500[,c("date", "GSPC.Close", "GSPC.Volume")], vix[,c("VIX.Close")])   # Combining sp500 and vix into a single data frame
colnames(sp500)[2:4] <- c("sp500", "sp500.volume", "vix")                                
sp500$sp500.return <- daily_return(sp500$sp500)                                        # Daily log return of the S&P 500 Market Index


us.stock.data <- merge(msci.ex, sp500, by = "date")                         # Combining msci, sp500, and vix data into a single dataframe
us.stock.data$month <- month(us.stock.data$date)                            # Extracting month from date


#### Treasury Bond Rates Data ####

tbond.data <- read.csv("Data/tbond.rates.csv", sep = ",")
tbond.data$date <- as.Date(tbond.data$date,  "%m/%d/%Y")
colnames(tbond.data)[2] <- "tbond"
tbond.data$tbond.d <- NA 
tbond.data$tbond.d[2:nrow(tbond.data)] <- diff(tbond.data$tbond, 1)

#### Media Attention and Stock Market ####
month.year.index <- which(colnames(us.stock.data) == "month" | colnames(us.stock.data) == "year")

masm.data <- merge(nyt.daily, us.stock.data[,-c(month.year.index)], by = "date", all = TRUE)     # masm stands for "Media Attention and Stock Markets"
masm.data <- merge(masm.data, tbond.data, by = "date", all = FALSE)
masm.data <- na.omit(masm.data)

#### Save the data ####

write.csv(masm.data, file = "masm.data.csv")


#### Exploratory time series plots ####

# Attention diversity
ts_plot(masm.data[1:100, c("date", "sp500.return")])  # first 100 days
ts_plot(masm.data[, c("date", "sp500.return")])       # full sample

# Attention instability
ts_plot(masm.data[1:100, c("date", "vix")])  # first 100 days
ts_plot(masm.data[, c("date", "vix")])      # full sample
