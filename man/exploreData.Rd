% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/exploreData.R
\name{exploreData}
\alias{exploreData}
\title{Graphical Exploration of a Dataset}
\usage{
exploreData(datmat)
}
\arguments{
\item{datmat}{the data frame (only categorical and numerical variables will be analysed)}
}
\description{
Function to provide a graphical exploration of a dataset will print results to file.
}
\examples{
mtcars$vs <- as.factor(mtcars$vs)
mtcars$am <- as.factor(mtcars$am)
exploreData(mtcars)
file.remove("DataSummary.pdf")
}
\author{
Aline Talhouk
}
