% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/threshold.R
\name{H_above_threshold}
\alias{H_above_threshold}
\title{H Above Threshold}
\usage{
H_above_threshold(dat.raw, settings)
}
\arguments{
\item{dat.raw}{data frame with column \emph{H}}

\item{settings}{list as returned by \code{\link{configure}} with list element
\emph{Hthresholds}}
}
\description{
Vector of TRUE/FALSE with TRUE at positions where H is above the threshold
}
