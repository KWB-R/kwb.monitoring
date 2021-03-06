% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_sample_information.R
\name{addSampleTimesToPlot}
\alias{addSampleTimesToPlot}
\title{Add Sample Times to Plot}
\usage{
addSampleTimesToPlot(sampleTimes, ymax,
  timeFormat = default_day_time_format(), cex.text = 0.7,
  showArrows = FALSE, showTimes = FALSE, showBottleNumbers = FALSE,
  legendPosition = "bottom")
}
\arguments{
\item{sampleTimes}{data frame with columns \code{sampleTime}, \code{bottle},
\code{result}}

\item{ymax}{maximum y value of plot. Used to determine arrow lengths and
positions}

\item{timeFormat}{default: "\%d.\%m \%H:\%M"}

\item{cex.text}{default: 0.7}

\item{showArrows}{default: FALSE}

\item{showTimes}{default: FALSE}

\item{showBottleNumbers}{default: FALSE}

\item{legendPosition}{character string specifying the legend position,
e.g. "bottom", "[top|bottom]left", "top", "[top|bottom]right"}
}
\description{
Add Sample Times to Plot
}
