#' Empirical Cumulative Distribution Plot
#' 
#' This is a wrapper function for the \link[ggplot2]{stat_ecdf} function and 
#' helps make it easy to directly compare distributions at specific
#' locations along the scale. 
#' @param data A tidy data frame containing the data to be plotted.
#' @param formula A formula of the type \code{out ~ group} where \code{out} is
#' the outcome variable and \code{group} is the grouping variable. Note this
#' variable can include any arbitrary number of groups. Additional variables 
#' can be included with \code{+} to produce separate plots by the secondary or
#' tertiary varaible (e.g., \code{out ~ group + characteristic1 + 
#' characteristic2}). No more than two additional characteristics can be 
#' supplied at this time.
#' @param ref_cut Optional numeric vector stating the location of reference 
#' line(s) and/or rectangle(s).
#' @param linewidth Width of ECDF lines. Note that the color of the lines can 
#' be controlled through additional functions (e.g., \code{scale_color_brewer,
#'   scale_color_manual}).
#' @param ref_line_cols Optional vector (or single value) of colors for 
#'   \code{ref_cut} lines.
#' @param ref_linetype Optional vector (or single value) of line types for 
#'   \code{ref_cut} lines. Takes any of the arguments supplied by 
#'   \link[ggplot2]{linetype}. 
#' @param center Logical. Should the functions be centered prior to plotting? 
#' Defaults to \code{FALSE}. Note that if paneled/faceted plots are produced, 
#' the centering occurs by group. 
#' @param ref_rect Logical, defaults to \code{TRUE} when \code{ref_cut} takes 
#' any non-null value. Should semi-transparent rectangle(s) be plotted at the 
#' locations of \code{ref_cut}? 
#' @export
#' @examples
#' 
#' ecdf_plot(benchmarks, math ~ ell, 
#'           ref_cut = c(190, 205, 210), 
#'           ref_line_cols = c("#D68EE3", "#9BE38E", "#144ECA")) +
#'   theme_minimal() +
#'   theme(legend.position = "bottom")
#'
#' ecdf_plot(seda, mean ~ grade) +
#'   scale_fill_brewer(palette = "Set2") +
#'   theme_minimal()
#'
#' 
#' ecdf_plot(seda, mean ~ grade) +
#'   scale_fill_brewer(palette = "Set2) +
#'   theme_minimal()

ecdf_plot <- function(data, formula, ref_cut = NULL, linewidth = 1.2, 
                      ref_line_cols = "gray40", ref_linetype = "solid", 
                      center = FALSE, ref_rect = TRUE,
                      ref_rect_col = "gray40", ref_rect_alpha = 0.7) {
  
  vars <- all.vars(formula)
  lhs  <- all.vars(formula)[1]
  rhs  <- labels(terms(formula))
  
  data <- data %>% 
    mutate_at(rhs, as.factor)
  
  if(center) {
      data <- data %>% 
        select(vars) %>% 
        group_by_at(rhs) %>% 
        mutate(!!sym(lhs) := scale(!!sym(lhs), scale = FALSE))
  }

  p <- ggplot(data, aes_(as.name(vars[1]), color = as.name(vars[2])))

  if(length(vars) == 3) {
    p <- p + facet_wrap(as.formula(paste0("~", vars[3])))
  }
  if(length(vars) == 4) {
    p <- p + facet_grid(as.formula(paste0(vars[4], "~", vars[3])))
  }
  
  # Vertical cut lines
  if(!is.null(ref_cut)) {
   p <- p + geom_vline(xintercept = ref_cut, 
                       color = ref_line_cols,
                       linetype = ref_linetype) 
  }
  p + stat_ecdf(size = linewidth)
}

