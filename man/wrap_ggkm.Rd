% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wrap_ggkm.R
\name{wrap_ggkm}
\alias{wrap_ggkm}
\title{Wrap list of \code{ggkm} plots into a single patchwork}
\usage{
wrap_ggkm(x, ...)
}
\arguments{
\item{x}{list of \code{ggkm()} figures}

\item{...}{additional annotation parameters passed to
\code{\link[patchwork:plot_annotation]{patchwork::plot_annotation()}}}
}
\description{
Kaplan-Meier plots generated for multiple survival outcomes can be wrapped
into a single figure.
}
