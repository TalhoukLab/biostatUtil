% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/remove_dup.R
\name{remove_dup}
\alias{remove_dup}
\title{Remove duplicates}
\usage{
remove_dup(x, cols)
}
\arguments{
\item{x}{data frame}

\item{cols}{character vector of column names from \code{x} to remove
duplicates}
}
\value{
A data frame with potentially fewer rows than \code{x} after
duplicated entries have been removed and repeated information has been
collapsed.
}
\description{
Remove duplicates for specified columns of a data frame
}
\details{
In Mass Spec data, there are occasionally duplicated entries that need to be
removed before further analysis. Duplication is indicated by the
\code{Quan.Info} and \code{PSM.Ambiguity} columns. \code{remove_dup} removes
duplicates for certain columns, then collapses repeated information into a
single row.

This function is intended to be used after a call to
\code{\link[dplyr:group_by]{dplyr::group_by()}} such that the removal of duplicates is
performed within each group of unique protein IDs (e.g.
\code{Reporter.Quan.Result.ID}).
}
\author{
Derek Chiu
}
