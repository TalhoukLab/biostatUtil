#' a function to decompose a survival formula
#'
#' @param formula formula
#' @param data data
#' @importFrom stats model.extract model.frame model.matrix complete.cases
#' @importFrom utils tail
#' @export
decomposeSurvform <- function(formula, data) {
  ### decomposes complex survival formula
  orig.formula <- formula

  terms <- terms(formula, data = data)
  fac <- attr(terms, "factors")
  predictors <- attr(terms, "term.labels")

  ## construct response variable:
  SurvResp <- model.extract(model.frame(formula, data = data), "response")
  mm <- model.matrix(formula, data = data) ## Model-Matrix
  mm1 <- mm[, -1, drop = FALSE]	# w/o intercept

  terms <- terms(formula, data = data)
  fac <- attr(terms, "factors")
  labels <- attr(terms, "term.labels")

  ## splittes by special chars
  #	f <- function(str)
  #		for(chars in c("(", ")", ":", " ", ",", "*", "^"))
  #			str <- unlist(strsplit(str, split=chars, fixed=TRUE))
  f <- function(str) {
    for (chars in c("(", ")", ":", " ", ",", "*", "^"))
      str <- unlist(strsplit(str, split = chars, fixed = TRUE))
    str
  }

  rowSplit <- sapply(rownames(fac), f)	# splitted effects
  time <- tail(rowSplit[[1]], 2)[1]	# name of stoptime
  event <- tail(rowSplit[[1]], 2)[2]	# name of stoptime
  indcc <- complete.cases(data[,c(time, event, predictors)])

  list(resp = SurvResp, 			# Surv response matrix
       mm1 = mm1, 			# model matrix without time effects
       survpair = c(time, event),  # names of survival pairs
       predictors = predictors,	 # names of covariates
       indcc = indcc			# indicator of complete cases
  )
}