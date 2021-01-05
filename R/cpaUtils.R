
calcCpa = function(data, values_col = 'CpaInput',
                   cpa_params = generateCpaDefaults())
  data %>%
  arrange(!!index(data)) %>%
  {.[[values_col]]} %>%
  {cpt.mean(., method = cpa_params$method,
           penalty = cpa_params$penalty,
           Q = cpa_params$Q,
           pen.value = cpa_params$pen.value)}


generateCpaDefaults = function()
  list(Q = 4, input_col = 'CpaInput', method = 'BinSeg', penalty = 'BIC',
       pen.value = .05, window_width = 1)

cpaToHorizontalSegment = function(cpa_df) cpa_df %>%
  group_modify(~ data.frame(xend = .x$cpts,
                            yend = .x$vals,
                            y = .x$vals,
                            x = lag(.x$cpts, default = 0)))

cpaToVerticalSegment = function(cpa_df) cpa_df %>%
  group_modify(~ data.frame(
    xintercept = (
      .x$cpts %>% lag %>% na.omit %>% as.numeric
    ))
  )


#' This simple function takes the output of a changepoint algorithm,
#' with a time array, and creates a data frame of 2 columns:
#' 1) cpts (change points, in time, the final time point before the change)
#' 2) vals (the calculated average \alue of the data before the change point)
#' @export
cpaResultsToDataFrame = function(cpt_res, time_data) {
  data.frame(cpts = time_data[cpt_res@cpts],
             vals = cpt_res@param.est$mean)
}


cpaPipelineCode = function(data_sym, time_column_sym, values_column_sym,
                           output_sym = sym("cpa_markers"), facet_column_sym = NULL,
                           ...) {
  rhs = data_sym
  is_facet = !is_null_or_empty(facet_column_sym)
  if(is_facet)
    rhs = expr(!!rhs %>% group_by(!!facet_column_sym))

  cpa_params = list2(...)

  rhs = expr(!!rhs %>%
               filter(n() > 1) %>%
               group_modify(~ arrange(.x, !!time_column_sym)) %>%
               group_modify(~
                              cpt.mean(.x[[!!(expr_text(values_column_sym))]],
                                       !!!cpa_params) %>%
                              cpaResultsToDataFrame(
                                .x[[!!(expr_text(time_column_sym))]])
               )
             )

  expr(!!output_sym <- !!rhs)
}


#' Preprocess (Regularize spacing and smooth) Data before CPA
#'
#' READ-TV uses changepoint algorithms in the \code{changepoint} package which expect (1) regularly spaced data in time, and (2) each x-axis time point should have a unique value on the y-axis.
#' \code{preprocessForCpa} regularizes the time spacing and performs an interpolation function at each of the new time points.
#'
#' \code{slidingWindow} is used by \code{preprocessForCpa} for interpolation.
#'
#' @param data a dataframe with a index column of time points
#' @param window_width the numeric or \code{lubridate::duration} width of the sliding window
#' @param index_col a string indicating which column of \code{data} is the time column
#' @param values_col a string indicating which column of \code{data} has values. If null or empty, then a mock column of only 1's is used.
#' @param output_col a string for the name of the interpolated values. Defaults to 'CpaInput'.
#' @param facet_col a string that specifies which column, if any, to treat as a faceting variable. The time-series interpolation will be executed seperately for each value of this column.
#' @param stride a numeric or \code{lubridate::duration} for the spacing between sliding window steps.
#' @param agg_fn_expr an interpolation expression calculated at each sliding window location.
#'
#'
#' @return data frame with columns for index and output. Where index will be regularly spaced and output will consist of interpolated values.
#'
#'
#' @examples
#' raw_data = data.frame(Time = rnorm(100, mean = 10))
#'
#' #uncomment to view plot
#' #plot(raw_data$Time, rep.int(1, nrow(raw_data)))
#'
#' pre_proc_data = preprocessForCpa(raw_data,
#'   window_width = 1,
#'   index_col = 'Time',
#'   stride = .1)
#'
#' #uncomment to view plot
#' #with(pre_proc_data, plot(Time, CpaInput))
#'
#' @export
preprocessForCpa = function(data, window_width,
                            index_col = 'RelativeTime',
                            values_col = NULL,
                            output_col = 'CpaInput',
                            facet_col = NULL,
                            stride = 1,
                            agg_fn_expr = sum(.values)) {
  if(is.null(values_col)) {
    data$IsEvent = 1
    values_col = 'IsEvent'
  }

  agg_fn = new_function(exprs(.values = ), enexpr(agg_fn_expr))
  mutate_fn = function(data) data %>%
    {slidingWindow(.[[index_col]], .[[values_col]],
                     n = window_width, agg_fn = agg_fn, stride = stride)} %>%
    select(!!sym(output_col) := Value, !!sym(index_col) := Time)

  output_select_fn = function(data) data %>%
    select(!!sym(index_col), !!sym(output_col)) %>%
    distinct

  as_tsibble_fn = function(data) {
    if(is.duration(stride))
      interval = stride %>%
        as.numeric('seconds') %>%
        {new_interval(second = .)}
    else interval = new_interval(unit = stride)

    data %>%
      build_tsibble(index = !!sym(index_col), interval = interval)
  }

  is_facet = !is_null_or_empty(facet_col)
  if(is_facet) {
    #facet_col = dplyr::groups(data)[[1]] #only supports one facet column for now
    data %<>%
      group_by(!!sym(facet_col)) %>%
      group_modify(~ mutate_fn(.x)) %>%
      group_modify(~ output_select_fn(.x)) %>%
      group_modify(~ as_tsibble(.x, index = index_col))
  }
  else
    data %<>%
      mutate_fn %>%
      output_select_fn %>%
      as_tsibble_fn

  data
}


quickPlotCpa = function(data, window_width,
                        index_col = 'RelativeTime',
                        cpa_params = generateCpaDefaults()) {
  smoothed_data = data %>%
    preprocessForCpa(window_width = window_width,
                     index_col = index_col)

  p = autoplot(smoothed_data, CpaInput)

  geom_input = smoothed_data %>%
    calcCpa(cpa_params = cpa_params) %>%
     {cpaPtsAndValues(smoothed_data, .)} %>%
    cpaToHorizontalSegment

  p + geom_segment(data = geom_input,
                   aes(x = x, xend = xend, y = y, yend = yend))
}


cpaPenalties = function() c("None", "SIC", "BIC", "MBIC", "AIC",
                            "Hannan-Quinn", "Asymptotic", "CROPS") #add support for Manual


cpaMethods = function() c("AMOC", "PELT", "SegNeigh", "BinSeg")


cpaPtsAndValues = function(data, cpt_out, time_col = 'RelativeTime') {
  cpts = data %>%
    arrange(!!sym(time_col)) %>%
    {.[[time_col]][cpt_out@cpts]}
  data.frame(cpts = cpts, vals = cpt_out@param.est$mean)
}

