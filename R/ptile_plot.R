pooled_sd <- function(formula, data) {
	splt <- parse_form(formula, data)

	vars <- sapply(splt, var, na.rm = TRUE)
	ns <- sapply(splt, length)

	pooled <- function(v) {
		sqrt((((ns[1] - 1)*vars[1]) + ((ns[2] - 1)*vars[2])) / 
			(sum(ns[v]) - 2))
	}
tidy_out(names(splt), pooled)
}

ptile_mean_diffs <- function(formula, data, ptiles = seq(0, 1, .33)) {
	splt <- parse_form(formula, data)
	ptile_l <- lapply(splt, function(x) split(x, cut(x, quantile(x, ptiles, na.rm = TRUE))))

	mean_diffs <- function(v) {
		Map(function(x, y) mean(x, na.rm = TRUE) - mean(y, na.rm = TRUE),
			ptile_l[[ v[1] ]], 
			ptile_l[[ v[2] ]])
	}
	td <- tidy_out(names(ptile_l), mean_diffs)	
	td$estimate <- unlist(td$estimate)

	low_ptiles <- ptiles[-length(ptiles)]
	high_ptiles <- ptiles[-1]

	td$cut <- rep(rep(low_ptiles, each = length(combn(names(splt), 2)) / 2), 2)
	td$high_ptile <- rep(rep(high_ptiles, 
						each = length(combn(names(splt), 2)) / 2), 2)
	names(td)[3] <- "low_ptile"

td[ ,c(1:3, 5, 4)]
}

#' Compute effect sizes by percentile bins
#' 
#' Returns a data frame with the estimated effect size by the provided 
#' percentiles. Currently, the effect size is equivalent to Cohen's d, but 
#' future development will allow this to vary.
#' 
#' @param formula  A formula of the type \code{out ~ group} where \code{out} is
#'   the outcome variable and \code{group} is the grouping variable. Note the
#'   grouping variable must only include only two groups.
#' @param data The data frame that the data in the formula come from.
#' @param ref_group Optional character vector (of length 1) naming the
#'   reference group to be plotted on the x-axis. Defaults to the highest
#'   scoring group.
#' @param ptiles The percentiles to split the data by and calculate effect 
#' sizes. Essentially, this is the binning argument. Defaults to 
#' \code{seq(0, 1, .33)}, which splits the distribution into thirds (lower,
#' middle, upper). Any sequence is valid, but it is recommended the bins be
#' even. For example \code{seq(0, 1, .1)} would split the distributions into
#' deciles.
#' @export


ptile_es <- function(formula, data, ref_group = NULL, 
	ptiles = seq(0, 1, .33)) {
	if(is.null(ref_group)) {
		splt <- parse_form(formula, data)
		ref_group <- names(which.max(sapply(splt, mean, na.rm = TRUE)))
	}

	means <- ptile_mean_diffs(formula, data, ptiles)
	means <- means[means$ref_group == ref_group, ]

	sds <- pooled_sd(formula, data)
	names(sds)[3] <- "pooled_sd"

	es <- merge(means, sds, by = c("ref_group", "foc_group"), all.x = TRUE)
	es$es <- es$estimate / es$pooled_sd
	es$midpoint <- (es$low_ptile + es$high_ptile) / 2

es[order(es$midpoint), ]
}

#' Percentile-matched effect size plot
#' 
#' This plot plots effect size between two groups by matched percentiles 
#' (i.e., the results from \link{ptile_es}), with the matched
#' percentiles plotted along the x-axis and the effect size plotted along the 
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
#' @param ptiles The percentiles to split the data by and calculate effect 
#' sizes. This argument is passed directly to \link{ptile_es}. 
#' Essentially, this is the binning argument. Defaults to \code{seq(0, 1, .33)}
#' which splits the distribution into thirds (lower, middle, upper). Any 
#' sequence is valid, but it is recommended the bins be even. For example
#' \code{seq(0, 1, .1)} would split the distributions into deciles.
#' @param refline Logical. Defaults to \code{TRUE}. Should a diagonal
#' reference line, representing the point of equal probabilities, be plotted?
#' @param refline_col Color of the reference line. Defaults to \code{"gray"}.
#' @param refline_lty Line type of the reference line. Defaults to \code{1}.
#' @param refline_lwd Line width of the reference line. Defaults to \code{2}.
#' @param rects Logical. Should semi-transparent rectangles be plotted in the 
#' background to show the binning? Defaults to \code{TRUE}.
#' @param rect_colors Color of rectangles to be plotted in the background, if
#' \code{rects == TRUE}. Defaults to alternating gray and transparent. 
#' Currently not alterable when \code{theme == "dark"}, in which case the rects
#' alternate a semi-transparent white and transparent.
#' @param lines Logical. Should the points between effect sizes across 
#' \code{ptiles} be connected via a line? Defaults to \code{TRUE}.
#' @param points Logical. Should points be plotted for each \code{ptiles} be 
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
#' @import utils
#' @export

ptile_plot <- function(formula, data, ref_group = NULL,
	ptiles = seq(0, 1, .33), refline = TRUE, refline_col = "black", 
	refline_lty = 1, refline_lwd = 2, rects = TRUE, 
	rect_colors = c(rgb(.2, .2, .2, .1), rgb(0.2, 0.2, 0.2, 0)),
	lines = TRUE, points = TRUE, legend = NULL, theme = NULL, 
	 ...) {

	args <- as.list(match.call())
	
	if(!is.null(theme)) {
		if(theme == "dark") {
			op <- par(bg = "gray21", 
					  col.axis = "white", 
					  col.lab = "white",
					  col.main = "white")
		}
	}
	else {
		op <- par(bg = "transparent")	
	}
	on.exit(par(op))

	d <- ptile_es(formula, data, ref_group, ptiles) 

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
	min_est <- min(d$es, na.rm = TRUE)
	max_est <- max(d$es, na.rm = TRUE)

	default_ylim_low <- ifelse(min_est < 0, 0.05*min_est + min_est, -0.1)
	default_ylim_high <- ifelse(max_est < 0, 0.1, 0.05*max_est + max_est)

	p <- with(d, empty_plot(midpoint, es,
					"Percentiles",
					"Effect Size",
					paste(as.character(formula)[c(2, 1, 3)], collapse = " "),
					xlim = c(0, 1),
					ylim = c(default_ylim_low, default_ylim_high),
					yaxt = "n",
					...))
	
	if(is.null(args$yaxt)) {
		axis(2, at = seq(round(default_ylim_low - 2), 
						 round(default_ylim_high + 2), 
						 .2), 
						 las = 2)
	}

    if(!is.null(theme)) {
		if(theme == "dark") {
			if(is.null(p$xaxt))	axis(1, col = "white")
			if(is.null(args$yaxt))  {
				axis(2, at = seq(round(default_ylim_low - 2), 
						 round(default_ylim_high + 2), 
						 .2), 
						 las = 2,
						 col = "white")
			}
			if(refline_col == "gray") refline_col = "white"
			if(refline_lwd == 1) refline_lwd = 2
		}
	}

	xaxes <- split(d$midpoint, d$foc_group)
	xaxes <- xaxes[-which.min(sapply(xaxes, length))]
	yaxes <- split(d$es, d$foc_group)
	yaxes <- yaxes[-which.min(sapply(yaxes, length))]

	if(rects) {
		rect_left <- unique(d$low_ptile)
		rect_right <- unique(d$high_ptile)

		if(is.null(theme)) {
			rect(rect_left, 
				min(d$es, na.rm = TRUE) - 1, 
				rect_right, 
				max(d$es, na.rm = TRUE) + 1, 
				col = rect_colors, 
				lwd = 0)
		}
		if(!is.null(theme)) {
			if(theme == "dark") {
				rect(rect_left, 
					min(d$es, na.rm = TRUE) - 1, 
					rect_right, 
					max(d$es, na.rm = TRUE) + 1, 
					col = c(rgb(1, 1, 1, .2), 
							rgb(0.1, 0.3, 0.4, 0)), 
					lwd = 0)
			}
		}
	}
	
	if(is.null(p$lwd)) p$lwd <- 2
	if(is.null(p$lty)) p$lty <- 1
	if(is.null(p$col)) p$col <- col_hue(length(xaxes))
	
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
		if(is.null(theme)) {
			abline(h = 0, 
				col = refline_col, 
				lwd = refline_lwd,
				lty = refline_lwd)
		}
		if(!is.null(theme)) abline(h = 0, lwd = 2, lty = 2, col = "white")
	}

	if(legend == "side") {
			create_legend(length(xaxes), names(xaxes), 
				col = p$col, 
				lwd = p$lwd, 
				lty = p$lty,
				left_mar = max_char * .35)
	}
	if(legend == "base") {
		if(is.null(theme)) {
			create_base_legend(names(xaxes), 
				col = p$col, 
				lwd = p$lwd, 
				lty = p$lty)
		}
		if(!is.null(theme)) {
			if(theme == "dark") {
				create_base_legend(names(xaxes), 
					col = p$col, 
					lwd = p$lwd, 
					lty = p$lty,
					text.col = "white")
				}
			}
		}
invisible(list(as.list(args), p))
}
