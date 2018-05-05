#' @keywords internal
#' @importFrom ggplot2 ggplot aes_ facet_wrap facet_grid geom_step
#'   geom_vline ggplot_build geom_label geom_line geom_ribbon geom_segment labs 
#'   geom_abline geom_rect 
#' @importFrom dplyr mutate mutate_at select group_by_at arrange distinct 
#'   filter matches group_by_all group_by summarize n semi_join
#' @importFrom tidyr spread fill gather separate crossing nest unnest
#' @importFrom purrr map map_dbl map2 map2_lgl
#' @importFrom rlang := sym syms quo .data
#' @importFrom graphics par layout lines segments rect polygon
#' @importFrom utils installed.packages
#' @importFrom grDevices adjustcolor
#' @importFrom sfsmisc integrate.xy
#' @importFrom stats as.formula terms setNames ecdf
"_PACKAGE"

if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))




