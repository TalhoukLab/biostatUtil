% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/grhoTests.R
\name{grhoTests}
\alias{grhoTests}
\title{Summary of Survival Curve differences using G-rho tests}
\usage{
grhoTests(formula, data, digits = 4)
}
\arguments{
\item{formula}{formula expression of the form \code{Surv(time, status) ~ predictors}}

\item{data}{data frame where variables from \code{formula} originate}

\item{digits}{number of significant digits to retain}
}
\value{
The Chi-Square statistic, degrees of freedom, and p-value are given
for both G-rho tests.
}
\description{
Runs the log-rank test and Breslow test to test for difference between two or
more survival curves.
}
\details{
The log-rank test corresponds to \code{rho = 0} and the Breslow test
corresponds to \code{rho = 1} in \code{survdiff}. This function emulates the
"Overall Comparisons" table output from SPSS.
}
\examples{
library(survival)
grhoTests(Surv(futime, fustat) ~ rx, data = ovarian)
}
\author{
Derek Chiu
}
