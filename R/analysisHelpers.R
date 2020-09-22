
#' Evenly space time data with a specified function or expression and a sliding window
#'
#' Returns a data frame with a Time and a Value column.
#' The Time column has evenly \code{stride} spaced points from \code{min(time_data)} to \code{max(time_data)}.
#' The Value column contains the result of applying \code{agg_fn} at each of the time points in Time to \code{values_data} that are within \code{±n} of the time point.
#'
#' @param time_data An array of numeric points
#' @param values_data a numeric or boolearn array for a value at each time in \code{time_data}.
#' @param n a numeric value or time duration/period
#' @param agg_fn a function to apply to each set of \code{values_data} within ±\code{n} time from \code{time_data}
#' @param stride the spacing between the resultant time points
#' @return data frame of 2 columns: Time - evenly spaced time points; and Values - the result of applying \code{agg_fn} at each of the time points in Time to \code{values_data} that are within \code{±n} of the time point.
#' Note that this can be readily converted to a tsibble::tsibble object for time series analyses.
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(lubridate)
#'
#' time_data = lubridate::now() + dminutes(rnorm(100))
#' windowed_data = slidingWindow(time_data, n = dminutes(2), stride = dseconds(5))
#'
#' ggplot(windowed_data) + geom_point(aes(x = Time, y = Value))
slidingWindow = function(time_data, values_data = purrr::rep_along(time_data, TRUE),
                            n = 4, agg_fn = sum, stride = 1) {
  un_sequenced_time = time_data

  is_diff_time_input = lubridate::is.difftime(time_data)
  if(is_diff_time_input) {
    base = lubridate::parse_date_time('01/01/1970', 'mdy')
    units = attr(time_data, 'units')
    un_sequenced_time = lubridate::as.duration(time_data) + base
  }

  time_pts_seq = seq(from = min(un_sequenced_time), to = max(un_sequenced_time),
                 by = stride)

  vals = sapply(
    time_pts_seq,
    function(t) values_data %>%
      magrittr::extract(abs(un_sequenced_time - t) <= n) %>%
      agg_fn)

  if(is_diff_time_input)
    time_pts_seq = as.numeric(time_pts_seq - base, units)

  data.frame(Time = time_pts_seq, Value = vals)
}


cusum = function(arr) arr %>% {. - mean(.)} %>% cumsum %>% {append(0, .)}
cusumDiff = function(arr) arr %>% cusum %>% {max(.) - min(.)}


bootstrapCusum = function(arr, n = 1000){
  #paper samples without replacement
  n_arr = length(arr)
  bootstraps = sapply(1:n,function(i) arr %>%
                        sample(n_arr) %>%
                        cusumDiff
                      )
  X = sum(cusumDiff(arr) > bootstraps)
  return(100 * X/n)
}
