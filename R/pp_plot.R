#' Produces the paired probability plot for two groups
#' 
#' The paired probability plot maps the probability of obtaining a specific
#'    score for each of two groups. The area under the curve 
#'    (\code{\link{auc}}) corresponds to the probability that a randomly
#'    selected observation from the x-axis group will have a higher score than
#'    a randomly selected observation from the y-axis group. This function
#'    extends the basic pp-plot by allowing multiple curves and faceting to
#'    facilitate a variety of comparisons. Note that because the plotting is
#'    built on top of \link[ggplot2]{ggplot2}, additional customization can 
#'    be made on top of the plots, as illustrated in the examples.
#' 
#' @param data The data frame to be plotted
#' @param formula A formula of the type \code{out ~ group} where \code{out} is
#' the outcome variable and \code{group} is the grouping variable. Note this
#' variable can include any arbitrary number of groups. Additional variables 
#' can be included with \code{+} to produce separate plots by the secondary or
#' tertiary variable of interest (e.g., \code{out ~ group + characteristic1 + 
#' characteristic2}). No more than two additional characteristics can be 
#' supplied at this time.
#' @param ref_group Optional character vector (of length 1) naming the
#'   reference group. Defaults to the group with the highest mean score.
#' @param cuts Integer. Optional vector (or single number) of scores used to 
#' annotate the plot. If supplied, line segments will extend from the 
#' corresponding x and y axes and meet at the PP curve.
#' @param cut_labels Logical. Should the reference lines corresponding to
#'    \code{cuts} be labeled? Defaults to \code{TRUE}.
#' @param cut_label_x The x-axis location of the cut labels. Defaults to 0.02.
#' @param cut_label_size The size of the cut labels. Defaults to 3.
#' @param lines Logical. Should the PP Lines be plotted? Defaults to 
#'    \code{TRUE}.
#' @param linetype The \link[ggplot2]{linetype} for the PP lines. Defaults to 
#'    "solid".
#' @param linewidth The width of the PP lines. Defaults to 1.1 (just
#'    marginally larger than the default ggplot2 lines).
#' @param shade Logical. Should the area under the curve be shaded? Defaults to
#'    \code{TRUE}.
#' @param shade_alpha Transparency of the shading. Defaults to 0.2.
#' @param refline Logical. Should a diagonal reference line be plotted, 
#'    representing the value at which no difference is observed between the
#'    reference and focal distributions? Defaults to \code{TRUE}.
#' @param refline_col Color of the reference line. Defaults to a dark gray.
#' @param refline_type The \link[ggplot2]{linetype} for the reference line.
#'    Defaults to "dashed".
#' @param refline_width The width of the reference line. Defaults to 1, or 
#'    just slightly thinner than the PP lines. 
#' @return A \link[ggplot2]{ggplot2} object displaying the specified PP plot.
#' @export
#' @examples
#' # PP plot examining differences by condition
#' pp_plot(star, math ~ condition)
#' 
#' # The sample size gets very small in the above within cells (e.g., wild 
#' # changes within the "other" group in particular). Overall, the effect doesn't
#' # seem to change much by condition.
#' 
#' # Look at something a little more interesting
#' \dontrun{
#' pp_plot(benchmarks, math ~ ell + season + frl)
#' }
#' # Add some cut scores
#' pp_plot(benchmarks, math ~ ell, cuts = c(190, 210, 215))
#' 
#' ## Make another interesting plot. Use ggplot to customize
#' \dontrun{
#' library(tidyr)
#' library(ggplot2)
#' benchmarks %>% 
#'   gather(subject, score, reading, math) %>% 
#'   pp_plot(score ~ ell + subject + season,
#'           ref_group = "Non-ELL") +
#'   scale_fill_brewer(name = "ELL Status", palette = "Pastel2") +
#'   scale_color_brewer(name = "ELL Status", palette = "Pastel2") +
#'   labs(title = "Differences among English Language Learning Groups",
#'        subtitle = "Note crossing of reference line") +
#'   theme_minimal()
#' }
#' 
pp_plot <- function(data, formula, ref_group = NULL, cuts = NULL, 
                    cut_labels = TRUE, cut_label_x = 0.02, cut_label_size = 3, 
                    lines = TRUE, linetype = "solid", linewidth = 1.1, 
                    shade = TRUE, shade_alpha = 0.2, refline = TRUE, 
                    refline_col = "gray40", refline_type = "dashed", 
                    refline_width = 1.1) {
  
  rhs  <- labels(terms(formula))
  lhs  <- all.vars(formula)[1]
  
  if(is.null(ref_group)) {
    group_means <- tapply(data[[lhs]], data[[rhs[1]]], mean, na.rm = TRUE)
    ref_group <- names(group_means)[which.max(group_means)]
  }
  
  d <- paired_ecdf(data, formula, cuts) %>%
    unnest(cols = .data$matched) %>%
    filter(!!sym(rhs[1]) == ref_group) 
  
  if(length(rhs) == 2) {
    d <- filter(d, !!sym(rhs[2]) == !!sym(paste0(rhs[2], 1)))
  }
  if(length(rhs) == 3) {
    d <- filter(d, 
                !!sym(rhs[2]) == !!sym(paste0(rhs[2], 1)),
                !!sym(rhs[3]) == !!sym(paste0(rhs[3], 1)))
  }
  p <- ggplot(d, aes_(quote(y_foc), quote(y_ref))) 
  
  if(shade) {
    p <- p + 
      geom_ribbon(aes_(fill = as.name(paste0(rhs[1], 1)),
                       ymin = -Inf,
                       ymax = quote(y_ref)),
                  alpha = shade_alpha)
  }
  if(refline) {
    p <- p + geom_abline(intercept = 0, 
                         slope     = 1, 
                         color     = refline_col, 
                         linetype  = refline_type,
                         size      = refline_width)
  }
  if(lines) {
    p <- p + geom_line(aes_(color = as.name(paste0(rhs[1], 1))),
                       linetype   = linetype,
                       size       = linewidth)
  }
  if(!is.null(cuts)) {
    cut_data <- d %>% 
      filter(.data$x %in% cuts)
    
    p <- p +
      geom_segment(aes_(x     = quote(y_foc),
                        xend  = quote(y_foc), 
                        y     = -Inf,
                        yend  = quote(y_ref),
                        color = as.name(paste0(rhs[1], 1))),
                   cut_data) +
      geom_segment(aes_(x     = -Inf,
                        xend  = quote(y_foc), 
                        y     = quote(y_ref), 
                        yend  = quote(y_ref), 
                        color = as.name(paste0(rhs[1], 1))),
                   cut_data)
      if(cut_labels) {
        p <- p + 
          geom_label(aes_(x = 0.02,
                          y = quote(y_ref),
                          label = quote(x)),
                     cut_data, 
                 size = 3)
      }
  }
  if(length(rhs) == 2) {
    p <- p + facet_wrap(as.formula(paste0("~", rhs[2])))
  }
  if(length(rhs) == 3) {
    p <- p + facet_grid(as.formula(paste0(rhs[2], "~", rhs[3])))
  } 
p + labs(x = "Focal Group",
         y = ref_group) 
}
