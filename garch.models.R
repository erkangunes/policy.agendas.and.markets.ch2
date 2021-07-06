#---------------------------------------------------------------#
#---- Policy Agendas and Financial Markets -  GARCH Models  ----#
#---------------------------------------------------------------#

#### Packages and Working Directory ####

if(!require(rugarch)) install.packages("rugarch")
if(!require(texreg)) install.packages("texreg")


library(rugarch)
library(texreg)

#### Model matrix for each of the three attention instability measures ####

model.matrix.1 <- model.matrix(~ 0 + volatility + epu + log(sp500.volume) + tbond.d, data = masm.data)   # attention instability measured as attention volatility
model.matrix.2 <- model.matrix(~ 0 + cosine + epu + log(sp500.volume) + tbond.d, data = masm.data)       # attention instability measured as cosine dissimilarity
model.matrix.3 <- model.matrix(~ 0 + euclid + epu + log(sp500.volume) + tbond.d, data = masm.data)       # attention instability measured as euclidean distance

model.matrice <- list(model.matrix.1, model.matrix.2, model.matrix.3)

#### Stock return data ####

return.data <- masm.data[, "sp500.return"]  


#### ARMA and Distribution Specifications ####

arma.list <- c(c(1,1), c(2,1), c(2,2))
dist.list <- c("std", "norm")

all.models <- list(list(), list(), list())  # empty list to store all model configurations



#### EGARCH estimation with different ARMA and distribution specifications ####

source("extract.rugarch.R")

for(i in 1:3){
    for(j in 1:3){
        p <- arma.list[2*j-1]
        q <- arma.list[2*j]
        
        for(k in 1:2){
            dist.type <- dist.list[k] 
            
            model.matrix.i <- scale(model.matrice[[i]])
            
            garch.spec <- ugarchspec(variance.model = list(model = "eGARCH",
                                                           garchOrder = c(1, 1), 
                                                           external.regressors = model.matrix.i
            ),
            
            mean.model = list(armaOrder = c(p,q), include.mean = TRUE,
                              external.regressors = model.matrix.i
            ),
            
            distribution.model = dist.type
            )    
            
            garch.fit <- ugarchfit(spec = garch.spec, data = return.data, solver = "hybrid", solver.control = list(tol = 1e-12))
            
            model.index <- ifelse(k==1,2*j-1,2*j)
            all.models[[i]][model.index] <- extract.rugarch(garch.fit, include.rsquared = FALSE)
        }
    }
}


#### Latex output of the above models ####

texreg(all.models[[1]][1:6], digits = 5)
texreg(all.models[[2]][1:6], digits = 5)
texreg(all.models[[3]][1:6], digits = 5)
