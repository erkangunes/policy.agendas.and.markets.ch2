
#### Function to extract estimate information from the rugarch output ####

# The original function can be found on the link below 
# https://stackoverflow.com/questions/38894044/print-pretty-tables-for-h2o-models-in-r

extract.rugarch <- function(fit, 
                            include.rsquared = TRUE, include.loglike = TRUE, include.aic = TRUE, include.bic = TRUE) {
    
    # extract coefficient table from fit:
    coefnames <- rownames(as.data.frame(fit@fit$coef))
    coefs <- fit@fit$coef
    se <- as.vector(fit@fit$robust.matcoef[, c(2)])
    pvalues <-  as.vector(fit@fit$robust.matcoef[, c(4)])       # numeric vector with p-values
    
    # create empty GOF vectors and subsequently add GOF statistics from model:
    gof <- numeric()
    gof.names <- character()
    gof.decimal <- logical()
    if (include.rsquared == TRUE) {
        r2 <-  1 - (var(fit@fit$residuals) / var(y))
        gof <- c(gof, r2)
        gof.names <- c(gof.names, "R^2")
        gof.decimal <- c(gof.decimal, TRUE)
    }
    if (include.loglike == TRUE) {
        loglike <- fit@fit$LLH
        gof <- c(gof, loglike)
        gof.names <- c(gof.names, "Log likelihood")
        gof.decimal <- c(gof.decimal, TRUE)
    }
    if (include.aic == TRUE) {
        aic <- infocriteria(fit)[c(1)]
        gof <- c(gof, aic)
        gof.names <- c(gof.names, "AIC")
        gof.decimal <- c(gof.decimal, TRUE)
    }
    
    if (include.bic == TRUE) {
        bic <- infocriteria(fit)[c(2)]
        gof <- c(gof, bic)
        gof.names <- c(gof.names, "BIC")
        gof.decimal <- c(gof.decimal, TRUE)
    }
    
    # create texreg object:
    tr <- createTexreg(
        coef.names = coefnames, 
        coef = coefs,
        se = se,
        pvalues = pvalues, 
        gof.names = gof.names, 
        gof = gof, 
        gof.decimal = gof.decimal
    )
    return(tr)
}
