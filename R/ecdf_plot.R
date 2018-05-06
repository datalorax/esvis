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
#' @param cuts Optional numeric vector stating the location of reference 
#' line(s) and/or rectangle(s).
#' @param linewidth Width of ECDF lines. Note that the color of the lines can 
#' be controlled through additional functions (e.g., \code{scale_color_brewer,
#'   scale_color_manual}).
#' @param ref_line_cols Optional vector (or single value) of colors for 
#'   \code{cuts} lines.
#' @param ref_linetype Optional vector (or single value) of line types for 
#'   \code{cuts} lines. Takes any of the arguments supplied by 
#'   \link[ggplot2]{linetype}. 
#' @param center Logical. Should the functions be centered prior to plotting? 
#' Defaults to \code{FALSE}. Note that if paneled/faceted plots are produced, 
#' the centering occurs by group. 
#' @param ref_rect Logical, defaults to \code{TRUE} when \code{cuts} takes 
#' any non-null value. Should semi-transparent rectangle(s) be plotted at the 
#' locations of \code{cuts}? 
#' @param ref_rect_col Color of the fill for the reference rectangles. Defaults 
#'   to a dark gray.
#' @param ref_rect_alpha Transparency of the fill for the reference rectangles. 
#'   Defaults to 0.7.
#' @export
#' @examples
#' ecdf_plot(benchmarks, math ~ ell, 
#'           cuts = c(190, 205, 210), 
#'           ref_line_cols = c("#D68EE3", "#9BE38E", "#144ECA"))
#' 
#' # Customize the plot with ggplot2 functions
#' library(ggplot2)
#' ecdf_plot(benchmarks, math ~ ell, 
#'           cuts = c(190, 205, 210), 
#'           ref_line_cols = c("#D68EE3", "#9BE38E", "#144ECA")) +
#'   theme_minimal() +
#'   theme(legend.position = "bottom")
#'
#' ecdf_plot(seda, mean ~ grade) +
#'   scale_fill_brewer(palette = "Set2") +
#'   theme_minimal()
#'   
#' # Use within the dplyr pipeline
#' library(dplyr)
#' benchmarks %>% 
#'   mutate(season = factor(season, 
#'                          levels = c("Fall", "Winter", "Spring"))) %>% 
#'   ecdf_plot(math ~ ell + season + frl)

ecdf_plot <- function(data, formula, cuts = NULL, linewidth = 1.2, 
                      ref_line_cols = "gray40", ref_linetype = "solid", 
                      center = FALSE, ref_rect = TRUE,
                      ref_rect_col = "gray40", ref_rect_alpha = 0.15) {
  
  lhs  <- all.vars(formula)[1]
  rhs  <- labels(terms(formula))
  
  if(center) {
      data <- data %>% 
        select(lhs, rhs) %>% 
        group_by_at(rhs) %>% 
        mutate(!!sym(lhs) := scale(!!sym(lhs), scale = FALSE))
  }
  
  d <- ecdf_fun(data, formula, cuts) %>% 
    unnest()

  p <- ggplot(d, aes_(~nd, ~ecdf))

  if(length(rhs) == 2) {
    p <- p + facet_wrap(as.formula(paste0("~", rhs[2])))
  }
  if(length(rhs) == 3) {
    p <- p + facet_grid(as.formula(paste0(rhs[3], "~", rhs[2])))
  }
  
  if(!is.null(cuts)) {
   p <- p + geom_vline(xintercept = cuts, 
                       color      = ref_line_cols,
                       linetype   = ref_linetype) 
   if(ref_rect) {
     ref_cut_d <- as.data.frame(t(cuts)) %>% 
       gather("dis", "nd") 
   
     p <- p + geom_rect(aes_(xmin = ~nd,
                            xmax = Inf,
                            ymin = 0,
                            ymax = Inf),
                        ref_cut_d,
                        fill  = ref_rect_col, 
                        alpha = ref_rect_alpha,
                   inherit.aes = FALSE) 
    }
  }
  p + geom_step(aes_(color = as.name(rhs[1])),
                size = linewidth) +
    labs(x = lhs,
         y = "Proportion")
}

