% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/name_cuts.R
\name{name_cuts}
\alias{name_cuts}
\title{Name cutpoint variables}
\usage{
name_cuts(x, cuts)
}
\arguments{
\item{x}{vector to split by certain cutpoints}

\item{cuts}{where to cut \code{x}}
}
\value{
A character string representing the cutpoint variable name.
}
\description{
Create a name for cutpoint variables based on location of cut and number of
groups.
}
\details{
The naming system is based on how \code{\link[Hmisc:cut2]{Hmisc::cut2()}} cuts variables. Used in
\code{\link[=build_cuts]{build_cuts()}} for naming the new cutpoint variables.
}
\examples{
set.seed(1108)
x <- sample(0:4, size = 1000, replace = TRUE)
name_cuts(x, c(1, 4))
name_cuts(x, c(2, 4))
}
\seealso{
\code{\link[Hmisc:cut2]{Hmisc::cut2()}}, \code{\link[=build_cuts]{build_cuts()}}
}
\author{
Derek Chiu
}
