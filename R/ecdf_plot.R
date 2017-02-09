#' Empirical Cumulative Distribution Plot
#' 
#' This function dresses up the \link[stats]{plot.ecdf} function and provides
#' some additional functionality to directly compare distributions at specific
#' locations along the scale. Specifically, multiple empirical CDFs can be 
#' plotted with a single call, and the differences between any pair, or all, 
#' CDFs can optionally be plotted in terms of both raw percentage differences 
#' and/or in terms of standard deviation units through inverse normal
#' transformations. See 
#' \href{http://journals.sagepub.com/doi/abs/10.3102/1076998611411918}{Ho &
#'  Reardon, 2012}. (Note, not all features implemented yet)
#' @param formula A formula of the type \code{out ~ group} where \code{out} is
#' the outcome variable and \code{group} is the grouping variable. Note this
#' variable can include any arbitrary number of groups.
#' @param data The data frame that the data in the formula come from.
#' @param v_ref Optional argument stating the location of the vertical 
#' reference line(s) to compare groups at a specific location on the scale.
#' @param ref_groups Optional character vector indicating the reference groups
#' (CDFs) to be compared at the location of \emph{v_ref}.
#' @param ref_col Optional argument stating the color of the vertical reference
#' line. Defaults to gray70.
#' @param text Logical. If reference groups are compared, should the difference
#' between the curves be annotated to the plot along the y-axis?
#' @param colors Optional vector indicating the colors of the CDF step function
#' lines.
#' @param ... Additional arguments passed to \link[graphics]{plot}.
#' @import graphics 
#' @export


ecdf_plot <- function(formula, data, v_ref = NULL, ref_groups = NULL, ref_col = NULL, text = TRUE, colors = NULL, ...) {
	splt <- parse_form(formula, data)
	ecdfs <- cdfs(formula, data)

	colors <- col_hue(length(splt))

	op <- par()
	op <- op[-grep(c("cin|cra|csi|cxy|din|page"), names(op))]
	on.exit(par(op))

	xlim <- seq(min(sapply(splt, min, na.rm = TRUE)),
				max(sapply(splt, max, na.rm = TRUE)), 
				.1)

	plot(xlim, seq(0, 1, length = length(xlim)), 
		type = "n",
		bty = "n",
		main = paste(as.character(formula)[c(2, 1, 3)], collapse = " "),
		xlab = "Scale",
		ylab = "f(x)",
		...)
	for(i in seq_along(ecdfs)) {
		lines(ecdfs[[i]], 
			col = colors[i], 
			lwd = 2,
			col.01line = FALSE, 
			verticals = TRUE, 
			do.points = FALSE)
	}
	
	if(!is.null(v_ref)) {
		if(is.null(ref_col)) ref_col <- "gray70"
		if(is.null(ref_groups)) {
			abline(v = v_ref, col = ref_col, lty = 2, lwd = 2)
		}
		if(!is.null(ref_groups)) {
			y_refs <- sapply(ecdfs, function(x) x(200))

			segments(x0 = v_ref, x1 = v_ref,
					 y0 = y_refs[ref_groups[1]], y1 = y_refs[ref_groups[2]],
					 col = ref_col,
					 #lty = 2,
					 lwd = 2)
			if(text == TRUE) {
				text(x = min(xlim), 
					 y = mean(y_refs[ref_groups[1]], y_refs[ref_groups[2]]),
					 abs(round(abs(y_refs[ref_groups[1]]) - 
					 		   abs(y_refs[ref_groups[2]]), 2)),
					 col = ref_col)
			}
		}
	}

}