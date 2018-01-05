#' Quantile-binned effect size plot
#' 
#' Plots the effect size between two groups by matched (binned) quantiles 
#' (i.e., the results from \link{qtile_es}), with the matched
#' quantiles plotted along the x-axis and the effect size plotted along the 
#' y-axis. The intent is to examine how (if) the magnitude of the effect size
#' varies at different points of the distributions.
#' 
#' @param formula  A formula of the type \code{out ~ group} where \code{out} is
#'   the outcome variable and \code{group} is the grouping variable. Note the
#'   grouping variable must only include only two groups.
#' @param data The data frame that the data in the formula come from.
#' @param ref_group Optional character vector (of length 1) naming the
#'   reference group to be plotted on the x-axis. Defaults to the highest
#'   scoring group.
#' @param qtiles The quantile bins to split the data by and calculate effect 
#' sizes. This argument is passed directly to \link{qtile_es}. 
#' Essentially, this is the binning argument. Defaults to \code{seq(0, 1, .33)}
#' which splits the distribution into thirds (lower, middle, upper). Any 
#' sequence is valid, but it is recommended the bins be even. For example
#' \code{seq(0, 1, .1)} would split the distributions into deciles.
#' @param scheme What color scheme should the lines follow? Defaults to 
#' mimic the ggplot2 color scheme. Other options come from the 
#' \href{https://CRAN.R-project.org/package=viridisLite}{viridisLite}
#' package, and must be installed first. These are the same options available
#' in the package: "viridis", "magma", "inferno", and "plasma". These color 
#' schemes work well for color blindness and print well in black and white.
#' Alternatively, colors can be supplied manually through a call to \code{col}
#' (through \code{...}).
#' @param se Logical. Should the standard errors around the effect size point
#' estimates be displayed? Defaults to \code{TRUE}, with the uncertainty 
#' displayed with shading. 
#' @param shade_col Color of the standard error shading, if \code{se == TRUE}.
#' Defaults to the same color as the lines.
#' @param shade_alpha Transparency level of the standard error shading.
#' Defaults to 0.3.
#' @param annotate Logical. Defaults to \code{FALSE}. When \code{TRUE} and 
#' \code{legend == "side"} the plot is rendered such that additional
#' annotations can be made on the plot using low level base plotting functions
#' (e.g., \link[graphics]{arrows}). However, if set to \code{TRUE}, 
#' \link[grDevices]{dev.off} must be called before a new plot is rendered 
#' (i.e., close the current plotting window). Otherwise the plot will be
#' attempted to be rendered in the region designated for the legend). Argument
#' is ignored when \code{legend != "side"}.
#' @param refline Logical. Defaults to \code{TRUE}. Should a diagonal
#' reference line, representing the point of equal probabilities, be plotted?
#' @param refline_lty Line type of the reference line. Defaults to \code{2}.
#' @param refline_lwd Line width of the reference line. Defaults to \code{2}.
#' @param rects Logical. Should semi-transparent rectangles be plotted in the 
#' background to show the binning? Defaults to \code{TRUE}.
#' @param rect_colors Color of rectangles to be plotted in the background, if
#' \code{rects == TRUE}. Defaults to alternating gray and transparent. 
#' Currently not alterable when \code{theme == "dark"}, in which case the rects
#' alternate a semi-transparent white and transparent.
#' @param lines Logical. Should the points between effect sizes across 
#' \code{qtiles} be connected via a line? Defaults to \code{TRUE}.
#' @param points Logical. Should points be plotted for each \code{qtiles} be 
#' plotted? Defaults to \code{TRUE}.
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
#' @param ... Additional arguments passed to \link[graphics]{plot}. Note that
#' it is best to use the full argument rather than partial matching, given the
#' method used to call the plot. While some partial matching is supported 
#' (e.g., \code{m} for \code{main}, it is generally safest to supply the full
#' argument).
#' @importFrom graphics par layout axis rect points abline lines
#' @importFrom grDevices rgb adjustcolor
#' @importFrom utils installed.packages
#' @export
#' @examples
#' 
#' # Default binned effect size plot
#' binned_plot(math ~ condition, star)
#' 
#' # Change the reference group to regular sized classrooms
#' binned_plot(math ~ condition, 
#' 		star,
#' 		ref_group = "reg")
#' 
#' # Change binning to deciles
#' binned_plot(math ~ condition, 
#' 		star,
#' 		ref_group = "reg",
#' 		qtiles = seq(0, 1, .1))
#' 
#' # Suppress the standard error shading
#' binned_plot(math ~ condition, 
#' 		star,
#' 		se = FALSE)
#' 
#' # Change to dark theme
#' binned_plot(math ~ condition, 
#' 		star,
#' 		theme = "dark")

binned_plot <- function(formula, data, ref_group = NULL,
	qtiles = seq(0, 1, .3333), scheme = "ggplot2", se = TRUE, shade_col = NULL,
	shade_alpha = 0.3, annotate = FALSE, refline = TRUE,
	refline_lty = 2, refline_lwd = 2, rects = TRUE, 
	rect_colors = c(rgb(.2, .2, .2, .1), rgb(0.2, 0.2, 0.2, 0)), lines = TRUE,
	points = TRUE, legend = NULL, theme = "standard", ...) {

	args <- as.list(match.call())
	
	op <- themes(theme)$op
	on.exit(par(op))

	d <- qtile_es(formula, data, ref_group, qtiles) 
	d <- d[order(d$foc_group), ]
	
	if(length(unique(d$foc_group)) > 1) {
		if(is.null(legend)) legend <- "side"
	}
	if(length(unique(d$foc_group)) == 1) {
		if(is.null(legend)) legend <- "none"
	}

	if(legend == "side") {
		max_char <- max(nchar(as.character(d$foc_group)))
		wdth <- 0.9 - (max_char * 0.01)
		layout(t(c(1, 2)), widths = c(wdth, 1 - wdth))	
	}
	min_est <- min(d$es - d$se, na.rm = TRUE)
	max_est <- max(d$es + d$se, na.rm = TRUE)

	default_ylim_low <- ifelse(min_est < 0, 0.05*min_est + min_est, -0.1)
	default_ylim_high <- ifelse(max_est < 0, 0.1, 0.05*max_est + max_est)

	p <- with(d, empty_plot(midpoint, es,
					paste0("Quantiles (ref group: ", unique(d$ref_group), ")"),
					"Effect Size",
					paste(as.character(formula)[c(2, 1, 3)], collapse = " "),
					default_xlim = c(0, 1),
					default_ylim = c(default_ylim_low, default_ylim_high),
					default_yaxt = "n",
					default_xaxt = "n",
					theme = theme,
					...))

	xaxes <- split(d$midpoint, d$foc_group)
	xaxes <- xaxes[-which.min(vapply(xaxes, length, numeric(1)))]
	yaxes <- split(d$es, d$foc_group)
	yaxes <- yaxes[-which.min(vapply(yaxes, length, numeric(1)))]

	if(rects) {
		rect_left <- unique(d$low_qtile)
		rect_right <- unique(d$high_qtile)
		
		rect(rect_left, -1e5, rect_right, 1e5, 
			col = c(adjustcolor(themes(theme)$line_col, alpha.f = 0.2),
					    adjustcolor(themes(theme)$line_col, alpha.f = 0)),
			lwd = 0)
	}

	if(is.null(p$lwd)) p$lwd <- 2
	if(is.null(p$lty)) p$lty <- 1
	if(is.null(p$col)) p$col <- col_scheme(scheme, length(xaxes))

	if(se) {
		x_shade <- split(d$midpoint, as.character(d$foc_group))
		x_shade <- lapply(x_shade, function(x) {
				x[1] <- x[1] - 0.01
				x[length(x)] <- x[length(x)] + 0.01
			return(c(x, rev(x)))
			})
		y_shade <- split(d, as.character(d$foc_group))
		y_shade <- lapply(y_shade, function(x) {
				lower <- x$es - x$se
				upper <- x$es + x$se
			return(c(lower, rev(upper)))
		})
		if(is.null(shade_col)) {
			shade_col <- adjustcolor(p$col, alpha.f = shade_alpha)
		}
		Map(polygon, x_shade, y_shade, col = shade_col, border = NA)		
	}

	if(lines) {
		Map(lines, xaxes, yaxes, col = p$col, lwd = p$lwd, lty = p$lty)	
	}
	if(points) {
		if(is.null(p$pch)) p$pch <- 21
		if(is.null(p$cex)) p$cex <- 1
		if(is.null(p$bg)) p$bg <- p$col
		
		Map(points, xaxes, yaxes, 
				col = p$col, 
				pch = p$pch,
				cex = p$cex,
				bg = p$bg)
	}
	if(refline) {
		abline(h = 0, 
			col = themes(theme)$line_col, 
			lwd = refline_lwd,
			lty = refline_lty)
	}

	if(legend == "side") {
		create_legend(length(xaxes), names(xaxes), 
			main_cols = p$col, 
			lwd = p$lwd, 
			lty = p$lty,
			left_mar = max_char * .35)
	}
	if(legend == "base") {
		create_base_legend(names(xaxes), 
			col = p$col, 
			lwd = p$lwd, 
			lty = p$lty, 
			text.col = themes(theme)$line_col)
	}
	if(annotate == TRUE) {
		par(mfg = c(1, 1))
		empty_plot(d$midpoint, d$es, 
			"", 
			"",
			xlim = c(0, 1),
			ylim = c(default_ylim_low, default_ylim_high), 
			xaxt = "n", 
			yaxt = "n")
	} 
invisible(c(as.list(match.call()), p, list(op)))
}
