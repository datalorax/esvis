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