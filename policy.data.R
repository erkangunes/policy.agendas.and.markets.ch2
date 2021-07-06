#---------------------------------------------------------------------------------#
#---- Policy Agendas and Financial Markets -  NY Times Front Page Daily Data  ----#
#---------------------------------------------------------------------------------#


#### Packages and Working Directory ####

if(!require(rstudioapi)) install.packages("rstudioapi")
if(!require(RCurl)) install.packages("RCurl")
if(!require(TSstudio)) install.packages("TSstudio")
if(!require(lsa)) install.packages("lsa")
if(!require(matrixStats)) install.packages("matrixStats")


library(rstudioapi)
library(RCurl)
library(TSstudio)
library(lsa)
library(matrixStats)


this.files.path <- getActiveDocumentContext()$path
setwd(dirname(this.files.path))

#### Load Raw Data ####

nyt.raw <- read.csv("Data/NYT_FrontPage_Dataset_1996-2006.csv", sep = ",")

#### Daily Attention Allocation Data #### 

#Defining variables for each unique majortopic and subtopic in the raw dataset

majortopics <- paste("mt", sort(unique(nyt.raw$CAPtopic_2digit)), sep = "")  #majortopic variables
subtopics <- paste("st", sort(unique(nyt.raw$CAPtopic_4digit)), sep = "")    #subtopic variables 

# Each row represents a unique date from the raw New York Times Front Page Dataset in which
# each row contains policy issue topic information for each front page articles in the NYT

nyt.daily <- data.frame(date = unique(nyt.raw$date))        # Empty data frame with date variable
nyt.daily[, majortopics] <- 0                               # Initializing majortopic variables with 0 values
nyt.daily[, subtopics] <- 0                                 # Initializing subtopic variables with 0 values
nyt.daily$date <- as.Date(nyt.daily$date, "%m/%d/%Y") 
nyt.daily <- nyt.daily[order(nyt.daily$date),]              # Order by date

# Daily majortopic and subtopic counts from article level data.
# The loop below iterates over the articles in the raw data and
# adds one to the majortopic and subtopic variables in the nyt.daily 
# dataset which correspond to the majortopic and subtopic categories of 
# the ith article in the raw dataset. The resulting dataset will be a 
# a sparse topic count matrix

for(i in 1:nrow(nyt.raw)){
    majortopic.i <- nyt.raw$CAPtopic_2digit[i]                                              # major topic (mt) category of the ith article
    subtopic.i <- nyt.raw$CAPtopic_4digit[i]                                                # subtopic (st) category of the ith article
    majortopic.column <- which(colnames(nyt.daily) == paste("mt", majortopic.i, sep = ""))  # the column index of the majortopic category in the nyt.daily
    subtopic.column <- which(colnames(nyt.daily) == paste("st", subtopic.i, sep = ""))      # the column index of the subtopic category in the nyt.daily 
    date.i <- as.Date(nyt.raw$date[i], "%m/%d/%Y")                                          # date of the ith article
    date.row <- which(nyt.daily$date == date.i)                                             # row index of the ith article's date in the nyt.daily

    majortopic.value <- nyt.daily[date.row, majortopic.column]                              # the mt(i) count at i-1
    subtopic.value <- nyt.daily[date.row, subtopic.column]                                  # the st(i) count at i-1  
    
    
    nyt.daily[date.row, majortopic.column] <- majortopic.value + 1                          # increasing the mt(i) count by 1
    nyt.daily[date.row, subtopic.column] <- subtopic.value + 1                              # increasing the st(i) count by 1
}



#### Attention to policy and nonpolicy topics #### 

# Topic lists

first.policy.issue <- which(colnames(nyt.daily) == "mt1")                    
last.policy.issue <- which(colnames(nyt.daily) == "mt21")
policy.issues <- colnames(nyt.daily)[first.policy.issue:last.policy.issue]      # the list of policy issue topics

first.nonpolicy.issue <- last.policy.issue + 1
last.nonpolicy.issue <- which(colnames(nyt.daily) == "mt99")
nonpolicy.issues <- colnames(nyt.daily)[first.nonpolicy.issue:last.nonpolicy.issue]   # the list of nonpolicy issue topics


# Counts

nyt.daily$policy.total<- rowSums(nyt.daily[, policy.issues])                    # daily count of policy issues in the front page of the NYT
nyt.daily$nonpolicy.total<- rowSums(nyt.daily[, nonpolicy.issues])              # daily count of the nonpolicy issues in the front page of the NYT 

nyt.daily$np.p.ratio <- nyt.daily$nonpolicy.total/nyt.daily$policy.total        # the ratio of nonpolicy count to policy count



#### Attention allocation characteristiscs ####

nyt.daily$total.attention <- nyt.daily$policy.total + nyt.daily$nonpolicy.total        #daily number of articles on the front page of the NYT

attention.diffs <- c()     # empty vector to store the daily changes in attention by topic


for (i in 1:nrow(nyt.daily)) { 
    total.attention <- nyt.daily$total.attention[i]
    
    frac <- function(a){issue.frac <- a/total.attention          # a function to calculate the what fraction of attention each topic gets
    return(issue.frac)}
    
    #Daily entropy and herfindahl
    issue.fracs <- apply(nyt.daily[i,c(majortopics)], MARGIN = 2, FUN = frac)  
    nyt.daily$entropy[i] <- -(sum(log(issue.fracs^issue.fracs))/log(28))  # attention diversity in the nyt frontpage as measured by the Shannon's entropy
    
    h <- sum(issue.fracs^2)
    nyt.daily$herfindahl[i] <- (h - (1/28))/(1-(1/28))                    # attention diversity in the nyt frontpage as measured by the Herfindahl Index
    
    if(i < 2){
        next
    }
    
    #Attention instability metrics                
    attention.diff <- nyt.daily[i,c(majortopics)] - nyt.daily[i-1,c(majortopics)]    # attentions changes from day i to i+1 
     
    #attention.diffs <- c(attention.diffs, attention.diff)   #uncomment if you need to store attention diffs for later use
    
    nyt.daily$volatility[i] <- sum(abs(attention.diff))/2                                                                     # attention volatility metric
    nyt.daily$cosine[i] <- cosine(as.numeric(nyt.daily[i,c(majortopics)]), as.numeric(nyt.daily[i-1,c(majortopics)]))         # cosine dissimilarity metric 
    nyt.daily$euclid[i] <- sqrt(sum(attention.diff^2))                                                                        # euclidean distance metric
}  


#### Daily EPU Data ####

epu.daily <- read.csv("Data/epu.daily.csv", sep = ",")                                                    #epu data from local file
colnames(epu.daily)[4] <- "epu"
epu.daily$date <- as.Date(paste(epu.daily$year,"-",epu.daily$month,"-",epu.daily$day, sep = ""))

nyt.daily <- merge(nyt.daily, epu.daily, by = "date", all = FALSE)

#### Save the dataset ####

write.csv(nyt.daily, file = "nyt.daily.csv")

#### Exploratory time series plots ####

# Attention diversity
ts_plot(nyt.daily[1:100, c("date", "herfindahl", "entropy")])  # first 100 days
ts_plot(nyt.daily[, c("date", "herfindahl", "entropy")])       # full sample

# Attention instability
ts_plot(nyt.daily[1:100, c("date", "volatility", "cosine", "euclid")])  # first 100 days
ts_plot(nyt.daily[, c("date", "volatility", "cosine", "euclid")])       # full sample




