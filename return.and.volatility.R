#-------------------------------------------------------#
#---- Daily return and Monthly Volatility Functions ----#
#-------------------------------------------------------#

#### Daily return ####

daily_return <- function(prices){
  prices_t <- prices[2:length(prices)]
  prices_t_1 <- prices[1:(length(prices)-1)]
  log_return <-  c(NA,log(prices_t/prices_t_1))
  return(log_return)
}

#### Monthly volatility (firms or portfolios) ####

# Works with daily return data from WRDS and daily portfolio data from Fama-French 
# Make sure the return column of the data is named "return"
# For firm level data set firm = TRUE

m_volatility <- function(data, firm = TRUE, first_year, last_year){ if(firm == TRUE){
  date_index <- which(colnames(data) == "datadate" | colnames(data) == "ref.date")
  if( any(colnames(data) == "month") & any(colnames(data) == "year")){
    data <- data
  } else {
  data$month = as.numeric(format(data[,date_index], "%m")) 
  data$year = as.numeric(format(data[,date_index], "%Y"))
  }
  
} else { return_column <-  which(colnames(data) != "month" & colnames(data) != "year")
         colnames(data)[return_column] <- "return"
         data$return <- as.numeric(as.character(data$return))
  
}   
  monthly_volatility <- data.frame(year = rep(c(first_year:last_year), each = 12), 
                                   month = rep(seq(1:12), times = last_year - first_year +1))
  
  for(i in 1:nrow(monthly_volatility)){
    subset_data <- subset(data, month == monthly_volatility$month[i] & year == monthly_volatility$year[i])
    monthly_volatility$volatility[i] <- sqrt(252)*100*sd(subset_data[,"return"], na.rm = TRUE)  
  }
  

if(firm == FALSE){
  monthly_volatility$volatility <- monthly_volatility$volatility/100
}
  
if(firm == TRUE){
  return(monthly_volatility)
  } else {
    return(monthly_volatility$volatility)
}
}


#### Market volatility #### 

#### Getting SP500 index data from Yahoo ####
if(!require(BatchGetSymbols)) install.packages("BatchGetSymbols")
library(BatchGetSymbols)

#Set dates
first.date <- Sys.Date() - (40*365)
last.date <- Sys.Date()
freq.data <- 'daily'

#Set tickers
tickers <- c('^GSPC')
tickers_2 <- c('^CRSPTM1')
l.out <- BatchGetSymbols(tickers = tickers, first.date = first.date,last.date = last.date, freq.data = freq.data, 
                         cache.folder = file.path(tempdir(), 'BGS_Cache') ) 

#Create the dataframe

spx_daily <- na.omit(l.out$df.tickers)

spx_daily$month <- as.numeric(format(spx_daily$ref.date, "%m"))
spx_daily$year <- as.numeric(format(spx_daily$ref.date, "%Y"))

#Daily returns
spx_daily$return <- daily_return(spx_daily$price.close)
#Monthly volatility 

spx_monthly <- m_volatility(spx_daily, firm =  FALSE, first_year = 1985, last_year = 2014)

#monthly return

m_return <- function(daily_data, monthly_data){
  
  date_index <- which(colnames(daily_data) == "datadate" | colnames(daily_data) == "ref.date")
  daily_data$month = as.numeric(format(daily_data[,date_index], "%m")) 
  daily_data$year = as.numeric(format(daily_data[,date_index], "%Y"))
  
  for(i in 2:nrow(monthly_data)){
    subset_data_1 <- subset(daily_data, month == monthly_data$month[i-1] & year == monthly_data$year[i-1])
    subset_data_2 <- subset(daily_data, month == monthly_data$month[i] & year == monthly_data$year[i])
    price_index <- which(colnames(subset_data_1) == "price.close" | colnames(subset_data_1) == "prccd")
    price_t <- tail(subset_data_2[,price_index],1)
    price_t_1 <- tail(subset_data_1[,price_index],1)
    if(length(subset_data_1[,1]) <5 | length(subset_data_2[,1]) < 5){ 
      next 
    }
    monthly_data$mrkt_return[i] <- log(price_t/price_t_1)
  }
  return_index <- which(colnames(monthly_data) == "mrkt_return")
  return(monthly_data[,return_index])
}


#Firm specific (idiosyncratic) volatility 

firm_volatility <- function(){
  model_beta <- lm(formula = firm_return ~ mrkt_return, data = monthly_volatility)
  beta <- summary(model_beta)$coefficients[2,1]
  firm_volatility <- sqrt((monthly_volatility$volatility^2) - (beta^2)*(monthly_volatility$mrkt_volatility^2))
  firm_volatility[which(firm_volatility == "NaN")] <- 0
  firm_volatility <- firm_volatility + 1
  return(firm_volatility)
}


