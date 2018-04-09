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
#' @param ref_cut Optional numeric vector stating the location of reference 
#' line(s) and/or rectangle(s).
#' @param center Logical. Should the functions be centered prior to plotting? Defaults to \code{FALSE}.
#' @param max_line Logical. Should the maximum distance between any two curves
#' be plotted? This distance is equivalent to the value tested by the 
#' Kolmogorov-Smirnov test. Defaults to \code{FALSE}.
#' @param ref_hor Logical, defaults to \code{FALSE}. Should horizontal
#'  reference lines be plotted at the location of \code{ref_cut}? 
#' @param ref_rect Logical, defaults to \code{TRUE}. Should semi-transparent 
#' rectangle(s) be plotted at the locations of \code{ref_cut}? 
#' @param scheme What color scheme should the lines follow? Defaults to 
#' mimic the ggplot2 color scheme. Other options come from the 
#' \href{https://CRAN.R-project.org/package=viridisLite}{viridisLite}
#' package, and must be installed first. These are the same options available
#' in the package: "viridis", "magma", "inferno", and "plasma". These color 
#' schemes work well for color blindness and print well in black and white.
#' Alternatively, colors can be supplied manually through a call to \code{col}
#' (through \code{...}).
#' @param legend The type of legend to be displayed, with possible values 
#' \code{"base"}, \code{"side"}, or \code{"none"}. Defaults to \code{"side"}, 
#' when there are more than two groups and \code{"none"} when only comparing
#' two groups. If the option \code{"side"} is used the plot is split into two
#' plots, via \link[graphics]{layout}, with the legend displayed in the second 
#' plot. This scales better than the base legend (i.e., manually manipulating
#' the size of the plot after it is rendered), but is not compatible with 
#' multi-panel plotting (e.g., \code{par(mfrow = c(2, 2))} for a 2 by 2 plot).
#' When producing multi-panel plots, use \code{"none"} or \code{"base"}, the
#' latter of which produces the legend with the base \link[graphics]{legend}
#' function.
#' @param theme Visual properties of the plot. There are currently only two
#' themes implemented - a standard plot and a dark theme. If \code{NULL} 
#' (default), the theme will be produced with a standard white background. If
#' \code{"dark"}, a dark gray background will be used with white text and axes.
#' @param annotate Logical. Defaults to \code{FALSE}. When \code{TRUE} and 
#' \code{legend == "side"} the plot is rendered such that additional
#' annotations can be made on the plot using low level base plotting functions
#' (e.g., \link[graphics]{arrows}). However, if set to \code{TRUE}, 
#' \link[grDevices]{dev.off} must be called before a new plot is rendered 
#' (i.e., close the current plotting window). Otherwise the plot will be
#' attempted to be rendered in the region designated for the legend. Argument
#' is ignored when \code{legend != "side"}.
#' @param ... Additional arguments passed to \link[graphics]{plot}. Note that
#' it is best to use the full argument rather than partial matching, given the
#' method used to call the plot. While some partial matching is supported 
#' (e.g., \code{m} for \code{main}, it is generally safest to supply the full
#' argument).
#' @importFrom graphics par layout lines segments rect 
#' @importFrom utils installed.packages
#' @importFrom grDevices adjustcolor
#' @export
#' @examples
#' # Produce base empirical cummulative distribution plot
#' ecdf_plot(mean ~ grade, seda)
#' 
#' # Shade distributions to the right of three cut scores
#' ecdf_plot(mean ~ grade, 
#' 		seda,
#' 		ref_cut = c(225, 245, 265))
#' 
#' # Add horizontal reference lines
#' ecdf_plot(mean ~ grade, 
#' 		seda,
#' 		ref_cut = c(225, 245, 265),
#' 		ref_hor = TRUE)
#' 
#' # Apply dark theme
#' ecdf_plot(mean ~ grade, 
#' 		seda,
#' 		ref_cut = c(225, 245, 265),
#' 		theme = "dark")

ecdf_plot <- function(formula, data, ref_cut = NULL, 
	center = FALSE, max_line = FALSE, ref_hor = FALSE, ref_rect = TRUE, 
	scheme = "ggplot2", legend = "side", annotate = FALSE, 
	theme = "standard", ...) {
	
	splt <- parse_form(formula, data)
	if(center) {
		splt <- lapply(splt, function(x) as.numeric(scale(x, scale = FALSE)))
	}
	ecdfs <- cdfs(formula, data, center)

	op <- themes(theme)$op	
	on.exit(par(op))

	x_lim <- seq(min(vapply(splt, min, na.rm = TRUE, numeric(1))),
				max(vapply(splt, max, na.rm = TRUE, numeric(1))), 
				.1)

	if(legend == "side") {
		max_char <- max(nchar(names(splt)))
		wdth <- 0.9 - (max_char * 0.01)
		layout(t(c(1, 2)), widths = c(wdth, 1 - wdth))	
	}

	p <- empty_plot(x_lim, seq(0, 1, length = length(x_lim)), 
					default_xlab = all.vars(formula)[1],
					default_ylab = "Proportion",
					default_main = paste(as.character(formula)[c(2, 1, 3)], 
								collapse = " "),
					theme = theme,
					...)

	if(is.null(p$lwd)) p$lwd <- 2
	if(is.null(p$lty)) p$lty <- 1
	if(is.null(p$col)) p$col <- col_scheme(scheme, length(splt))

	Map(lines, 
		ecdfs,
	    col = p$col, 
		lwd = p$lwd,
		lty = p$lty,
		col.01line = FALSE, 
		verticals = TRUE, 
		do.points = FALSE)
	
	if(max_line) {
		differences <- probs(formula, data, center)
		differences <- apply(differences, 1, function(x) max(abs(diff(x))))
		x_int <- as.numeric(names(differences[which.max(differences)]))
		ys <- vapply(ecdfs, function(x) x(x_int), numeric(1))
		y_start <- min(ys)
		y_end <- max(ys)
		anno_col <- col_scheme(scheme, length(p$col) + 1)
		anno_col <- anno_col[!anno_col %in% p$col]
		anno_col <- anno_col[!anno_col %in% p$col][length(anno_col)]
		segments(x_int, y_start, x_int, y_end, lwd = 4, col = "gray40")
	}

	if(!is.null(ref_cut)) {
		if(ref_hor) {
			x_ints <- split(rep(ref_cut, length(ref_cut)), 
						rep(seq(1, length(ref_cut)), each = length(ref_cut)))

			y_ints <- lapply(ecdfs, function(x) x(ref_cut))
			Map(segments, 
				x0 = 0, 
				x1 = x_ints,
				y0 = y_ints,
				y1 = y_ints,
				col = rep(p$col, length(x_ints)),
				lty = rep(3, length(x_ints)),
				lwd = rep(1.5, length(x_ints))) 
		}
		if(ref_rect) {
				rect(ref_cut, -1, 1000, 2, 
					col = adjustcolor(themes(theme)$line_col, alpha.f = 0.2), 
					lwd = 0)
			}
		}
	if(legend == "side") {
		create_legend(length(splt), names(splt), 
			main_cols = p$col, 
			lwd = p$lwd, 
			lty = p$lty,
			left_mar = max_char * .35)
	}
	if(legend == "base") {
		create_base_legend(names(splt), 
			col = p$col, 
			lwd = p$lwd, 
			lty = p$lty, 
			text.col = themes(theme)$line_col)
	}
	if(annotate == TRUE) {
		par(mfg = c(1, 1))
		empty_plot(x_lim, seq(0, 1, length = length(x_lim)), 
			"", "", xaxt = "n", yaxt = "n", ...)
	} 
invisible(c(as.list(match.call()), p))
}