#' @keywords internal
#' @importFrom ggplot2 ggplot aes_ stat_ecdf facet_wrap facet_grid geom_vline
#'   ggplot_build geom_label geom_line geom_ribbon geom_segment labs geom_abline
#' @importFrom dplyr mutate mutate_at select group_by_at arrange distinct 
#'   filter matches
#' @importFrom tidyr  spread fill gather separate
#' @importFrom rlang := sym quo .data
#' @importFrom graphics par layout lines segments rect 
#' @importFrom utils installed.packages
#' @importFrom grDevices adjustcolor
#' @importFrom stats as.formula terms setNames
"_PACKAGE"

if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))




