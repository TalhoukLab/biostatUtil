#'An internal function that decomposes a survival formula
#'@param formula a formula object, with the response on the left of a ~ operator, and the terms on the right. The response must be a survival object as returned by the Surv function.
#'@param data a data.frame in which to interpret the variables named in the formula.
#'@return This function retuns a list with the following parameters
#'\item{resp} Surv response matrix
#'\item{mm1} Model matrix without time effects
#'\item{survpair} names of survival pairs
#'\item{predictors} names of covariates
#'\item{indcc} indicator of complete cases
#'@author Aline Talhouk (adapted from an existing function)

decomposeSurvform <-function(formula,data){
    # save the entered formula
    orig.formula <- formula
    ## construct response variable:
    SurvResp <- model.extract(model.frame(formula, data = data), "response")
    mm <- model.matrix(formula, data = data) ## Model-Matrix
    mm1 <- mm[, -1, drop=FALSE]	# w/o intercept
    terms <- terms(formula, data=data)
    fac <- attr(terms, "factors")
    predictors <- attr(terms, "term.labels")
    labels <- attr(terms, "term.labels")
    ## function that splits by special chars
    f<-function(str) {
      for(chars in c("(", ")", ":", " ", ",", "*", "^"))
        str <- unlist(strsplit(str, split=chars, fixed=TRUE))
      str
    }
    rowSplit <- sapply(rownames(fac), f)	# splitted effects
    time <- tail(rowSplit[[1]], 2)[1]	# name of stoptime
    event <- tail(rowSplit[[1]], 2)[2]	# name of stoptime
    indcc <- complete.cases(data[,c(time, event, predictors)])
    list(resp=SurvResp, 			# Surv response matrix
         mm1=mm1, 			# model matrix without time effects
         survpair=c(time,event),# names of survival pairs
         predictors=predictors,	# names of covariates
         indcc=indcc			# indicator of complete cases
    )
    }