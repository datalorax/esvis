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

pp_calcs <- function(formula, data, ref_group = NULL, scheme = "ggplot2",
	...) {
	ps <- probs(formula, data)
	if(is.null(ref_group)) ref_group <- colnames(ps)[1]
	
	sq <- seq_len(ncol(ps))
	ref_group_d <- ps[ ,sq[colnames(ps) == as.character(ref_group)] ]
	ps_subset <- ps[ ,-sq[colnames(ps) == as.character(ref_group)], 
					drop = FALSE]

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

#' Theme settings
#' 
#' Parameters for each theme currently implemented.
#' @param theme The name of the theme.
#' @return list with the \code{par} settings and the primary line color (e.g., 
#' white for theme = "dark" and black for theme = "standard").
themes <- function(theme) {
	op <- switch(theme,
		standard = par(bg = "transparent"),
		dark 	 = par(bg = "gray21", 
					  col.axis = "white", 
					  col.lab = "white",
					  col.main = "white"),
		tan 	 = par(bg = "cornsilk2",
					   col.axis = "darkslategray", 
					   col.lab = "darkslategray",
					   col.main = "darkslategray"),
		blue 	 = par(bg = "cornflowerblue",
					   col.axis = "deeppink", 
					   col.lab = "deeppink",
					   col.main = "deeppink"))

	line_col <- switch(theme,
		standard = "black",
		dark 	 = "white",
		tan 	 = "darkslategray",
		blue 	 = "deeppink")
list(op = op, line_col = line_col)
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
