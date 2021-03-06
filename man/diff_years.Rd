% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/date_compute.R
\name{diff_years}
\alias{diff_years}
\title{Date difference in years}
\usage{
diff_years(start, stop)
}
\arguments{
\item{start}{starting date of time interval}

\item{stop}{ending date of time interval}
}
\value{
A numeric value representing the date difference in years
}
\description{
Calculate date difference in years, taking into account leap years.
}
\details{
Since the number of days in a year changes depending on whether said year
is a leap year, dividing a period of time by estimates such as 365.25,
365.241, 365.24442 will be slightly biased. The correct way to calculate a
length of period in years is to take into account which years are leap years
and divide by 365 or 366 as needed.
}
\examples{
start <- as.Date("2009-03-08")
stop <- as.Date("2009-08-09")
diff_years(start, stop)
}
