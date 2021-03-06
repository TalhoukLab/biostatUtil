% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/freqTable.R
\name{freqTable}
\alias{freqTable}
\title{Generate a Frequency Table}
\usage{
freqTable(
  x,
  levels = sort(unique(as.character(x))),
  missing = NULL,
  description = NULL,
  round = 1,
  plot = FALSE
)
}
\arguments{
\item{x}{vector of values to show frequencies for}

\item{levels}{(optional) vector for order of levels in \code{x}}

\item{missing}{vector of levels for which we consider missing and don't count
in the valid percentage}

\item{description}{(optional) description for each level of \code{x}. Must be
same length and order as \code{levels}}

\item{round}{number of digits to round percentages to}

\item{plot}{logical; if \code{TRUE}, a barplot is produced.}
}
\value{
A data frame with the following columns
\item{Class}{Tells you which scores are valid and which are missing. Not shown if
there are no missing values.}
\item{Score}{Different levels}
\item{Frequency}{Count for each score}
\item{Percent}{Percent of Frequency out of the grand total}
\item{Valid Percent}{Percent of Frequency out of the Valid scores. Not applicable if
there are no missing values.}
\item{Cumulative Percent}{Accumulated Percent of Frequency out of the Valid Scores}
\item{Description}{If \code{description} is given, a description for each level}
}
\description{
A frequency table emulating the SPSS FREQ output is generated.
}
\examples{
# Create vector of randomly reordered alphabet with various frequencies
# for each letter
set.seed(123)
n <- sample(10, length(letters), replace = TRUE)
x <- sample(rep.int(letters, times = n))
freqTable(x, plot = TRUE)

# Treat vowels as missing
freqTable(x, missing = c("a", "e", "i", "o", "u"), round = 2)
}
\seealso{
\code{\link[=freq]{freq()}}
}
\author{
Derek Chiu
}
