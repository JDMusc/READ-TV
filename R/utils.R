isSelected = function(choice) any(choice != c("All"))
selectableChoices = function(choices) c("All" = "All", choices)

#https://stackoverflow.com/questions/8197559/emulate-ggplot2-default-color-palette
eventTypeColors <- function(types, do_sort = TRUE) {
  if(do_sort)
    types = sort(types)

  n_types = length(types)
  hues = seq(15, 375, length = n_types + 1)
  colors = hcl(h = hues, l = 65, c = 100)[1:n_types]
  ret = list()
  for(i in 1:n_types){
    ty = types[i]
    col = colors[i]
    ret[[ty]] = col
  }
  return(ret)
}


printWithCountGen <- function(msg) {
  count = reactiveVal(0)

  printWithCount <- function(){
    if(config.debug) {
      isolate(count(count() + 1))

      print(paste(msg, count()))
    }
  }

  return(printWithCount)
}


doesEvalCompile = function(ex, data) {
  log_utils('doesEvalCompile')

  try(
    eval_tidy(ex, data),
    silent = TRUE
  ) %>%
  class %>%
  not_equals('try-error') %>%
  all #can be more than one class
}

getElementSafe = function(item_name, obj, default = NULL) {
  if(item_name %in% names(obj)) obj[[item_name]]
  else default
}

`%not in%` = function(item, collection) !(item %in% collection)

expressionsToString = function(..., width = 50, do_style = TRUE) {
  unstyled = list(...) %>%
    {rlang::flatten(.)} %>%
    purrr::discard(~ is_empty(.x)) %>%
    purrr::map(~ ifelse(rlang::is_string(.x),
                        .x,
                        expr_text(.x, width = width)
                        )
               ) %>%
    purrr::reduce(~ paste(.x, .y, sep = '\n'))

  if(do_style)
    unstyled %>% style_text %>% paste(collapse = '\n')
  else
    unstyled
}


runExpressions = function(exs, mask, location,
			  env = rlang::caller_env()) {
  log_utils('runExpressions')
  f = stringr::str_interp
  for(i in seq_along(exs)) {
    nm = names(exs)[[i]]
    mask[[nm]] = eval_tidy_verbose(exs[[i]],
                                   data = mask,
                                   location = f('${location}: ${i}'),
                                   env = env)
  }
  mask
}


runExpressionsLast = function(exs, mask, location) {
  log_utils('runExpressionsLast')
  exs %>%
    runExpressions(mask, location) %>%
    tail(n = 1) %>%
    magrittr::extract2(1)
}

is_null_or_empty = function(e)
  is_null(e) | is_empty(e) | rlang::is_na(e) | is_empty_str(e)

is_empty_str = function(e) {
  if(is_character(e)) e == ""
  else is_null(e)
}

is_str_set = function(e) !(is_empty_str(e))

not_equals = function(e1, e2)
  !(magrittr::equals(e1, e2))

equals_null_safe = function(e1, e2) {
  e1_null = is_null_or_empty(e1)
  e2_null = is_null_or_empty(e2)

  if(e1_null != e2_null)
    FALSE
  else if(e1_null & e2_null)
    TRUE
  else
    magrittr::equals(e1, e2)
}

`%==%` = equals_null_safe

not_equals_null_safe = function(e1, e2)
  !(e1 %==% e2)

`%!=%` = not_equals_null_safe

str_un_title = function(string) string %>%
  str_sub(1, 1) %>%
  str_to_lower %>%
  paste0(str_sub(string, 2))


name_expressions = function(xs) xs %>%
  purrr::map(expr_text) %>%
  purrr::map(~ stringr::str_remove_all(.x, '`')) %>%
  {set_names(xs, nm = .)}


set_expr_names = function(xs, nms)
  nms %>% purrr::map(expr_text) %>% {set_names(xs, .)}

#' Use Non-standard evaluation with functions that require a list with string, integer, double, or bool values
#'
#' This is used with \code{launchReadtv} (\code{plotOpts} argument).
#' \code{launchReadtv} expects a list with non-expression values.
#' This type of list can be generated with \code{tvOpts}
#'
#' For any values not set, \code{launchReadtv} will use the defaults found in \code{generatePlotDefaults()}.
#' \code{generatePlotDefaults()} will also show which list names can be set, other names will be ignored.
#'
#' @return list with string, integer, double, or bool values
#'
#' @examples
#'
#' library(dplyr)
#' library(magrittr)
#'
#' readtv::japan_eq_3_11 %>%
#'   with(tvOpts(x = time, y = mag, facetOn = place))
#'
#' app = readtv::japan_eq_3_11 %>%
#'   mutate(Time = time, Case = 1, Event.Type = place) %>%
#'   launchReadtv(plotOpts = tvOpts(
#'     x = time, y = mag, color = place, facetOn = place,
#'     isFacetPaginated = TRUE, facetRowsPerPage = 3, facetPage = 2))
#'
#' #shiny::runApp(app)
#'
#' @export
tvOpts = function(...)
  rlang::enexprs(...) %>%
  purrr::map_if(
    ~ !(rlang::is_integer(.x) | rlang::is_double(.x) | rlang::is_bool(.x)),
    rlang::as_string
  )


eval_tidy_verbose = function(expr, data = NULL, env = rlang::caller_env(), location = '',
                             stop_on_error = TRUE) {
  log_utils(stringr::str_interp('will evaluate ${location}'))
  logger::log_info(expr_text(expr), namespace = logger_ns)
  ret = try(
    eval_tidy(expr, data, env),
    silent = TRUE
  )

  did_compile = ret %>%
    class %>%
    not_equals('try-error') %>%
    all #can be more than one class

  f = stringr::str_interp

  if(!did_compile) {
    msg = f('\n error trying to evaluate: ${expr_text(expr)}\n ${ret} \n location: ${location}')
    if(stop_on_error)
      stop(msg)
    else
      warning(msg)
  }

  ret
}


validEventTypeCol = function(df, col)
  is.character(df[[col]]) | is.factor(df[[col]])


validCaseCol = function(df, col)
  n_distinct(df[[col]])/nrow(df) < .2


wrapInGroupBy = function(no_env_quo, df_sym = sym('df'), group_col = sym('Case')) {
  ex = rlang::get_expr(no_env_quo)
  expr(!!df_sym %>%
         group_by(!!group_col) %>%
         group_modify(~!!ex) %>%
         ungroup
         )
}


logger_ns = 'logger.readtv'
logger_format = '{pid}/{namespace} {time} {level}: {msg}'

log_info_module_gen = function(module) {
  function(msg) {
   logger::log_info(f('${module}, ${msg}'), namespace = logger_ns)
  }
}

log_utils = log_info_module_gen('utils')

req_log_gen = function(log_fn) {
  f = stringr::str_interp
  function(msg, req_quo) {
    e_text = rlang::expr_text(req_quo)
    log_fn(f('${msg}, pre-req, ${e_text}'))

    eval_tidy(expr(req(!!req_quo)))

    log_fn(f('${msg}, post-req'))
  }
}

`%?%` = function(pred, if_t) list(pred, if_t)
`%:%` = function(q_out, if_f) if(q_out[[1]]) q_out[[2]] else if_f
