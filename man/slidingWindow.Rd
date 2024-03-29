% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/analysisHelpers.R
\name{slidingWindow}
\alias{slidingWindow}
\title{Evenly space time data with a specified function or expression and a sliding window}
\usage{
slidingWindow(
  time_data,
  values_data = purrr::rep_along(time_data, TRUE),
  n = 4,
  agg_fn = sum,
  stride = 1
)
}
\arguments{
\item{time_data}{An array of numeric points}

\item{values_data}{a numeric or boolearn array for a value at each time in \code{time_data}.}

\item{n}{a numeric value or time duration/period}

\item{agg_fn}{a function to apply to each set of \code{values_data} within ±\code{n} time from \code{time_data}}

\item{stride}{the spacing between the resultant time points}
}
\value{
data frame of 2 columns: Time - evenly spaced time points; and Values - the result of applying \code{agg_fn} at each of the time points in Time to \code{values_data} that are within \code{±n} of the time point.
Note that this can be readily converted to a tsibble::tsibble object for time series analyses.
}
\description{
Returns a data frame with a Time and a Value column.
The Time column has evenly \code{stride} spaced points from \code{min(time_data)} to \code{max(time_data)}.
The Value column contains the result of applying \code{agg_fn} at each of the time points in Time to \code{values_data} that are within \code{±n} of the time point.
}
\examples{
library(ggplot2)
library(lubridate)

time_data = lubridate::now() + dminutes(rnorm(100))
windowed_data = slidingWindow(time_data, n = dminutes(2), stride = dseconds(5))

ggplot(windowed_data) + geom_point(aes(x = Time, y = Value))
}
