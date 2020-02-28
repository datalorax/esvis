#' @keywords internal
#' @importFrom ggplot2 ggplot aes_ facet_wrap facet_grid geom_step geom_point
#'   geom_vline ggplot_build geom_label geom_line geom_hline geom_ribbon 
#'   geom_segment labs geom_abline geom_rect 
#' @importFrom dplyr mutate mutate_at select group_by_at arrange distinct 
#'   filter matches group_by_all group_by summarize summarize_at n left_join 
#'   semi_join vars funs ends_with rename tbl_df ungroup everything mutate_if
#'   bind_cols
#' @importFrom tidyr spread fill gather separate crossing nest unnest
#' @importFrom tibble tibble lst
#' @importFrom purrr map map_dbl map_lgl map2 map2_lgl map2_df is_atomic
#' @importFrom rlang := sym syms quo quos .data parse_quo set_names 
#'   quo_get_expr
#' @importFrom Hmisc cut2
#' @importFrom magrittr %>%
#' @importFrom graphics par layout lines segments rect polygon
#' @importFrom utils installed.packages
#' @importFrom grDevices adjustcolor
#' @importFrom sfsmisc integrate.xy
#' @importFrom stats as.formula terms setNames ecdf qnorm na.omit var
"_PACKAGE"

if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))




