#' readtv
#'
#' todo: file in.
#'
#' @importFrom changepoint cpt.mean
#' @importFrom dplyr arrange distinct filter first group_by
#' @importFrom dplyr group_modify if_else lag mutate
#' @importFrom dplyr n n_distinct pull rename select slice
#' @importFrom dplyr summarise ungroup
#' @importFrom ggforce n_pages
#' @import ggplot2
#' @importFrom grDevices hcl
#' @importFrom logger log_trace
#' @importFrom lubridate dpicoseconds dnanoseconds dmicroseconds dmilliseconds
#' @importFrom lubridate dseconds dminutes dhours ddays dweeks dmonths dyears
#' @importFrom lubridate is.difftime is.duration is.timepoint
#' @importFrom magrittr %>% %<>% extract extract2
#' @importFrom purrr discard is_numeric keep map
#' @importFrom readr read_csv read_rds read_tsv write_csv write_rds write_tsv
#' @importFrom rlang := eval_tidy expr expr_text exprs is_call
#' @importFrom rlang is_character is_double is_empty is_integer is_logical
#' @importFrom rlang is_null list2 new_function parse_expr set_names sym quo
#' @import shiny
#' @importFrom sortable add_rank_list bucket_list
#' @importFrom stringr str_detect str_sub str_to_lower
#' @importFrom styler style_text
#' @importFrom tidyr drop_na
#' @importFrom tools file_ext
#' @importFrom tsibble as_tsibble build_tsibble index new_interval
#' @importFrom utils head str
#'
#' @name readtv
NULL
