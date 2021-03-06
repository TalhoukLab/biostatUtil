% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ms_process.R
\name{ms_process}
\alias{ms_process}
\title{Process mass spectrometry data}
\usage{
ms_process(
  psm,
  protein,
  treatment,
  samples = NULL,
  sample.id = NULL,
  path = NULL,
  ...
)
}
\arguments{
\item{psm}{PSM file}

\item{protein}{Protein file}

\item{treatment}{character vector of treatment groups}

\item{samples}{character vector of sample names to keep}

\item{sample.id}{character vector for sample IDs. Order of samples must match
that in the psm raw data.}

\item{path}{file path to save return element \code{pep}}

\item{...}{additional arguments to \code{ms_condition}}
}
\value{
A list with the following elements
\item{pep}{processed data frame to be used by \code{ms_summarize}}
\item{raw}{raw data values}
\item{l2}{log2 raw data values}
\item{vsn}{vsn raw data values}
}
\description{
Process mass spectrometry data for filtering out contaminant samples,
manipulating variables, removing duplicates, and more.
}
\seealso{
Other Mass Spectrometry functions: 
\code{\link{ms_condition}()},
\code{\link{ms_plot}},
\code{\link{ms_summarize}()},
\code{\link{ms_top}()}
}
\author{
Derek Chiu
}
\concept{Mass Spectrometry functions}
