% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ms_summarize.R
\name{ms_summarize}
\alias{ms_summarize}
\title{Summarize mass spectrometry data}
\usage{
ms_summarize(
  x,
  g,
  level = c("Gene", "Peptide"),
  col.names = NULL,
  info.vars = NULL,
  path = NULL
)
}
\arguments{
\item{x}{data frame with expression values}

\item{g}{vector of factor levels for samples}

\item{level}{analysis is on the "Gene" level or "Peptide" level}

\item{col.names}{vector of column names for output data frame}

\item{info.vars}{vector of column names containing metadata information.
These variables are collapsed if not unique.}

\item{path}{file path to save result object}
}
\value{
A data frame of statistics from analyzing mass spec data. Includes
t-values, Wald p-values, effect sizes, fold change, absolute fold change.
}
\description{
Summarize mass spectrometry data for expression differences between sample
groups. Apply Benjamini-Hochberg p-value adjustment.
}
\details{
Gene-level analysis is performed on the "Gene" variable. Peptide-level
analysis is performed on distinct combinations of the Accession, Gene,
Descriptions, Sequence, and Modifications variables. This combined variable
is renamed to "AGDSM".
}
\seealso{
Other Mass Spectrometry functions: 
\code{\link{ms_condition}()},
\code{\link{ms_plot}},
\code{\link{ms_process}()},
\code{\link{ms_top}()}
}
\author{
Derek Chiu
}
\concept{Mass Spectrometry functions}
