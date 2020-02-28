#' Quantile-binned effect size plot
#' 
#' Plots the effect size between focal and reference groups by matched (binned) 
#' quantiles (i.e., the results from \link{binned_es}), with the matched
#' quantiles plotted along the x-axis and the effect size plotted along the 
#' y-axis. The intent is to examine how (if) the magnitude of the effect size
#' varies at different points of the distributions. The mean differences within
#' each quantile bin are divided by the overall pooled standard deviation for 
#' the two groups being compared.
#' 
#' @inheritParams pp_plot
#' @param qtile_groups The number of quantile bins to split the data by and 
#' calculate effect sizes. Defaults to 3 bins (lower, middle, upper). 
#' @param es The effect size to plot. Defaults to \code{"g"}, in which case 
#' Hedge's g is plotted, which is better for small samples. At present, the 
#' only other option is \code{"d"} for Cohen's D.
#' @param points Logical. Should points be plotted for each \code{qtiles} be 
#' plotted? Defaults to \code{TRUE}.
#' @param shade Logical. Should the standard errors around the effect size point
#' estimates be displayed? Defaults to \code{TRUE}, with the uncertainty 
#' displayed with shading. 
#' @param shade_alpha Transparency level of the standard error shading.
#' Defaults to 0.40.
#' @param refline Logical. Defaults to \code{TRUE}. Should a diagonal
#' reference line, representing the point of equal probabilities, be plotted?
#' @param refline_col The color of the reference line. Defaults to 
#'   \code{"gray40"}
#' @param refline_lty Line type of the reference line. Defaults to
#'   \code{"solid"}.
#' @param refline_lwd Line width of the reference line. Defaults to \code{1.1}.
#' @param rects Logical. Should semi-transparent rectangles be plotted in the 
#' background to show the binning? Defaults to \code{TRUE}.
#' @param rect_fill Color fill of rectangles to be plotted in the background, if
#' \code{rects == TRUE}. Defaults to "gray20".
#' @param rect_alpha Transparency level of the rectangles in the background when
#' \code{rects == TRUE}. Defaults to 0.35.
#' @export
#' @examples
#' # Binned Effect Size Plot: Defaults to Hedges' G
#' binned_plot(star, math ~ condition)
#'  
#' # Same plot, separated by sex
#' binned_plot(star, math ~ condition + sex)
#' 
#' # Same plot by sex and race
#' \dontrun{
#'   pp_plot(star, math ~ condition + sex + race)
#' }
#' ## Evaluate with simulated data: Plot is most interesting when variance
#' # in the distributions being compared differ.
#' 
#' library(tidyr)
#' library(ggplot2)
#' 
#' # simulate data with different variances
#' set.seed(100)
#' common_vars <- data.frame(low  = rnorm(1000, 10, 1),
#'                         high = rnorm(1000, 12, 1),
#'                         vars = "common")
#' diff_vars <- data.frame(low  = rnorm(1000, 10, 1),
#'                       high = rnorm(1000, 12, 2),
#'                       vars = "diff")
#' d <- rbind(common_vars, diff_vars)
#' 
#' # Plot distributions 
#' d <- d %>% 
#' gather(group, value, -vars) 
#' 
#' ggplot(d, aes(value, color = group)) +
#'  geom_density() +
#'  facet_wrap(~vars)
#'
#' # Note that the difference between the distributions depends on where you're 
#' # evaluating from on the x-axis. The binned plot helps us visualize this. 
#' # The below shows the binned plots when there is a common versus different
#' # variance
#' 
#' binned_plot(d, value ~ group + vars)   

binned_plot <- function(data, formula, ref_group = NULL, qtile_groups = 3, 
                        es = "g", lines = TRUE, points = TRUE, 
                        shade = TRUE, shade_alpha = 0.40,
                        rects = TRUE, rect_fill = "gray20", rect_alpha = 0.35,
                        refline = TRUE, refline_col = "gray40", 
                        refline_lty = "solid", refline_lwd = 1.1) {
  rhs  <- labels(terms(formula))
  lhs  <- all.vars(formula)[1]
  
  if(length(ref_group) > 1) {
    warning(paste0("Please only specify one reference group. Faceting ", 
                   "will be used for other groups. Reference group supplied ", 
                   "for first group will be used."))
    ref_group <- ref_group[1]
  }
  
  if(is.null(ref_group)) {
    group_means <- tapply(data[[lhs]], data[[rhs[1]]], mean, na.rm = TRUE)
    ref_group <- names(group_means)[which.max(group_means)]
  }
  if(is.formula(ref_group)) {
    ref_group <- gsub("~|`", "", as.character(ref_group))[2]
  }
  d <- binned_es(data, formula, ref_group, qtile_groups = qtile_groups,
                 es = es, rename = FALSE) %>% 
    filter(!!sym(paste0(rhs[1], 1)) != ref_group)
  
  if(length(rhs) == 2) {
    d <- filter(d, !!sym(rhs[2]) == !!sym(paste0(rhs[2], 1)))
  }
  if(length(rhs) == 3) {
    d <- filter(d, 
                !!sym(rhs[2]) == !!sym(paste0(rhs[2], 1)),
                !!sym(rhs[3]) == !!sym(paste0(rhs[3], 1)))
  }
  if(shade) {
    d <- d %>% 
      mutate(lb = .data$es + (qnorm(0.025)*.data$es_se),
             ub = .data$es + (qnorm(0.975)*.data$es_se))
  }
  p <- d %>% 
    mutate(midpoint = .data$qtile_ub - (.data$qtile_ub[1] / 2)) %>% 
    ggplot(aes_(~midpoint, ~es))
  
  if(rects) {
    p <- p +
      geom_rect(aes_(xmin = ~qtile_lb, 
                    xmax  = ~qtile_ub, 
                    ymin  = -Inf, 
                    ymax  = Inf),
                filter(d, as.logical(q %% 2)),
                alpha = rect_alpha,
                fill = rect_fill,
                inherit.aes = FALSE)
  }
  if(shade) {
    p <- p + geom_ribbon(aes_(ymin = ~lb, 
                              ymax = ~ub, 
                              fill = as.name(paste0(rhs[1], 1))),
                    alpha = shade_alpha)
  }
  if(refline) {
    p <- p + geom_hline(yintercept = 0, 
                        color = refline_col,
                        lty   = refline_lty,
                        lwd   = refline_lwd)
  }
  if(lines)   p <- p + geom_line(aes_(group = as.name(paste0(rhs[1], 1)), color = as.name(paste0(rhs[1], 1))))
  if(points)  p <- p + geom_point(aes_(group = as.name(paste0(rhs[1], 1)), color = as.name(paste0(rhs[1], 1))))
    
  if(length(rhs) == 2) p <- p + facet_wrap(as.formula(paste0("~", rhs[2])))
  if(length(rhs) == 3) {
    p <- p + facet_grid(as.formula(paste0(rhs[2], "~", rhs[3])))
  } 
  p + labs(x = "Quantile Bin",
           y = "Effect Size Estimate") 
}