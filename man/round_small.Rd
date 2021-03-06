% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/round_small.R
\name{round_small}
\alias{round_small}
\title{Rounding of Small Numbers}
\usage{
round_small(x, method = c("round", "signif"), digits = 3, sci = FALSE)
}
\arguments{
\item{x}{a numeric vector or matrix}

\item{method}{use either \code{\link[=round]{round()}} or \code{\link[=signif]{signif()}} as
rounding method}

\item{digits}{integer indicating number of decimal places to round to}

\item{sci}{if \code{TRUE}, scientific notation is used}
}
\value{
If precision of number is larger than desired rounding, the default
\code{round} is used. Otherwise, we provide an upper bound instead of
coercion to 0.
}
\description{
Rounding of a number smaller than specified precision doesn't coerce to 0.
}
\details{
This function is useful when we have small p-values and don't want to show
the scientific notation, or coercion to 0. Instead we show an upper bound.
For example, if a p-value is 2e-05 and we want to round to 3 digits, the
function will return "< 0.001".
}
\examples{
# Vector inputs
round_small(2e-04)
round_small(5e-04)
round_small(6e-04)

# Matrix input
set.seed(12)
x <- matrix(rexp(25, 3), nrow = 5)
round_small(x, digits = 1)
}
\author{
Derek Chiu
}
