#' Internal function to clean a character vector of dahses, spaces, and periods
#' @keywords internal

sanitize_names <- function(x) {
  gsub("-|\\s|\\.", "", x)  
}

#' Produce calculations necessary for \link{pp_plot}.
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
#' @return A dataframe either for use with plotting or for effect size 
#'   calculation.
#' @export
#' @examples 
#' 
#' extract_pp_data(star, math ~ condition)
#'  
#' extract_pp_data(benchmarks, math ~ ell + season + frl)
#' 
  
extract_pp_data <- function(data, formula, ref_group = NULL) {
  rhs  <- labels(terms(formula))
  lhs  <- all.vars(formula)[1]
  
  ecdf <- ecdf_plot(data, formula, center = FALSE)
  build <- ggplot_build(ecdf)
  
  if(is.null(ref_group)) {
    group_means <- tapply(data[[lhs]], data[[rhs[1]]], mean, na.rm = TRUE)
    ref_group <- names(group_means)[which.max(group_means)]
  }
  
  ref_group_s <- sanitize_names(ref_group)
  
  d <- build$data[[1]] %>% 
      select(.data$x, .data$y, .data$group, .data$PANEL) %>% 
      spread(.data$group, .data$y) %>% 
      arrange(.data$PANEL) %>% 
      fill(names(.)) %>% 
      setNames(c("x", 
                 "PANEL", 
                 sanitize_names(levels(build$plot$data[[rhs[1]]])))) %>% 
      gather(!!sym(rhs[1]), "focal_group", 
             -.data$x, -.data$PANEL, -ref_group_s) 
  
  if(length(rhs) ==2) {
    d$panel <- factor(d$PANEL, 
                      levels = unique(d$PANEL),
                      labels = levels(build$plot$data[[rhs[2]]]))
  }
  if(length(rhs) == 3) {
    f1_lab <- sanitize_names(levels(build$plot$data[[rhs[3]]]))
    f2_lab <- sanitize_names(levels(build$plot$data[[rhs[2]]]))
    
    panel_labs <- paste(rep(f1_lab, each = length(f2_lab)),
                        f2_lab, 
                        sep = "-")
    
    d$panel <- panel_labs[match(d$PANEL, seq_along(panel_labs))]
    d <- separate(d, .data$panel, c("f1", "f2"), sep = "-")
  }
  d
}
