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
      gather(!!sym(rhs[1]), .data$focal_group, 
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




#' Produce calculations necessary for \link{pp_plot}.
#' 
#' @param formula  A formula of the type \code{out ~ group} where \code{out} is
#'   the outcome variable and \code{group} is the grouping variable. Note the
#'   grouping variable must only include only two groups.
#' @param data The data frame that the data in the formula come from.
#' @param ref_group Optional character vector (of length 1) naming the
#'   reference group to be plotted on the x-axis. Defaults to the highest
#'   scoring group.
#' @param scheme What color scheme should the lines follow? Defaults to 
#' mimic the ggplot2 color scheme. Other options come from the 
#' \href{https://CRAN.R-project.org/package=viridisLite}{viridisLite}
#' package, and must be installed first. These are the same options available
#' in the package: "viridis", "magma", "inferno", and "plasma". These color 
#' schemes work well for color blindness and print well in black and white.
#' Alternatively, colors can be supplied manually through a call to \code{col}
#' (through \code{...}).
#' @return
#' List with appropriate \code{probs}, name of the reference group, data for
#' the reference group and all other groups, and data for the x/y axes.

pp_calcs <- function(formula, data, ref_group = NULL, scheme = "ggplot2") {
	ps <- probs(formula, data, center = FALSE)
	if(is.null(ref_group)) ref_group <- colnames(ps)[1]

	sq <- seq_len(ncol(ps))
	ref_group_d <- ps[ ,sq[colnames(ps) == as.character(ref_group)] ]
	ps_subset <- ps[ ,-sq[colnames(ps) == as.character(ref_group)], 
					drop = FALSE]
	ps_subset <- ps_subset[ ,order(colnames(ps_subset)), drop = FALSE]

	x_axs <- rep(ref_group_d, ncol(ps_subset))
	
	x_lines <- split(x_axs, 
				rep(seq_len(ncol(ps_subset)), each = nrow(ps_subset)))
	y_lines <- split(ps_subset, 
				rep(seq_len(ncol(ps_subset)), each = nrow(ps_subset)))

	return(list(ps = ps,
			ref_group = ref_group, 
			ref_group_d = ref_group_d, 
			ps_subset = ps_subset, 
			x_lines = x_lines, 
			y_lines = y_lines))
}


#' Annotation function to add AUC/V to a given plot
#' 
#' @param formula  A formula of the type \code{out ~ group} where \code{out} is
#'   the outcome variable and \code{group} is the grouping variable. Note the
#'   grouping variable must only include only two groups.
#' @param data The data frame that the data in the formula come from.
#' @param ref_group Optional character vector (of length 1) naming the
#'   reference group to be plotted on the x-axis. Defaults to the highest
#'   scoring group.
#' @param x The x-axis location for the text.
#' @param y The y-axis location for the text.
#' @param text_size Size of the text used in the annotation
#' @param theme The theme for the plot.

pp_annotate <- function(formula, data, ref_group = NULL, x, y,
	text_size, theme = "standard") {
	
	color <- themes(theme)$line_col
	
	auc_val <- round(auc(formula, data, ref_group, FALSE), 2)
	v_val <- round(v(formula, data, ref_group, FALSE), 2)
	text(x, y, 
		cex = text_size, 
		col = color,
		paste0("AUC = ", auc_val, "\n", "V = ", v_val))
}

#' Create a set of reference lines according to a cut score
#' 
#' @param cut The cut scores on the raw scale
#' @param calcs object from \link{pp_calcs}
#' @param p output from \link{empty_plot}
#' @param scheme What color scheme should the lines follow? Defaults to 
#' mimic the ggplot2 color scheme. Other options come from the 
#' \href{https://CRAN.R-project.org/package=viridisLite}{viridisLite}
#' package, and must be installed first. These are the same options available
#' in the package: "viridis", "magma", "inferno", and "plasma". These color 
#' schemes work well for color blindness and print well in black and white.
#' Alternatively, colors can be supplied manually through a call to \code{col}
#' (through \code{...}).

create_cut_refs <- function(cut, calcs, p, scheme) {
	cuts <- calcs$ps[rownames(calcs$ps) %in% cut, ,drop = FALSE]
	full_cols <- col_scheme(scheme, length(p$col) + nrow(cuts) + 1)
	rem_cols <- col_scheme(scheme, length(p$col) + 1)
	cut_cols <- full_cols[!full_cols %in% rem_cols][seq_along(p$col)]
	
	if(class(cuts) == "numeric") {
		for(i in seq_along(cuts)[-1]) {
			seg_match(cuts[1], cuts[i], 
				col = cut_cols[i - 1],
				pch = 21,
				bg = cut_cols[i - 1],
				lty = 3)
		}
	}

	if(class(cuts) == "matrix") {
		sp_cuts <- split(cuts, seq_len(nrow(cuts)))
		
		lapply(seq_along(sp_cuts), function(j) {
			for(i in seq_along(sp_cuts[[1]])[-1]) {
				seg_match(sp_cuts[[j]][1], sp_cuts[[j]][i], 
					col = cut_cols[j],
					pch = 21,
					bg = cut_cols[j],
					lty = 3)
			}
		})
	}
cut_cols
}