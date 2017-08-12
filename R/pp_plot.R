#' Produces the paired probability plot for two groups
#' 
#' The paired probability plot maps the probability of obtaining a specific
#'    score for each of two groups. The area under the curve 
#'    (\code{\link{auc}}) corresponds to the probability that a randomly
#'    selected observation from the x-axis group will have a higher score than
#'    a randomly selected observation from the y-axis group.
#' 
#' @param formula  A formula of the type \code{out ~ group} where \code{out} is
#'   the outcome variable and \code{group} is the grouping variable. Note the
#'   grouping variable must only include only two groups.
#' @param data The data frame that the data in the formula come from.
#' @param ref_group Optional character vector (of length 1) naming the
#'   reference group to be plotted on the x-axis. Defaults to the highest
#'   scoring group.
#' @param annotate Logical. Defaults to \code{FALSE}. When \code{TRUE} and 
#' \code{legend == "side"} the plot is rendered such that additional
#' annotations can be made on the plot using low level base plotting functions
#' (e.g., \link[graphics]{arrows}). However, if set to \code{TRUE}, 
#' \link[grDevices]{dev.off} must be called before a new plot is rendered 
#' (i.e., close the current plotting window). Otherwise the plot will be
#' attempted to be rendered in the region designated for the legend). Argument
#' is ignored when \code{legend != "side"}.
#' @param refline Logical. Defaults to \code{TRUE}. Should a diagonal
#'    reference line, representing the point of equal probabilities, be
#' 	  plotted?
#' @param refline_col Color of the reference line.
#' @param refline_lty Line type of the reference line.
#' @param refline_lwd Line width of the reference line.
#' @param text Logical. Should the \code{link{auc}} and \code{link{v}}
#'    statistics be displayed on the plot? Defaults to \code{TRUE} when there
#' 	  are two groups. Cannot currently be displayed for more than two groups.
#' @param text_size The size of the text to be displayed. Defaults to 2. 
#' @param shade Logical. Should the area under the curve be shaded? Defaults
#'    to \code{TRUE} if there are only two group. Currently it cannot be 
#'    produced for more than two groups.
#' @param shade_rgb The color of the shading via \link[grDevices]{rgb}. 
#'    Defaults to \code{rgb(102, 178, 255, alpha = 30, max = 255)} which is a 
#'    light blue color.
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
#' @param plot Logical. Should the plot be produced? Defaults to \code{TRUE}. 
#' Sometimes it is useful to only get the output from the plot, which is why
#' this functionality exists (likely to be implemented in a panel plot). 
#' @param theme Visual properties of the plot. There are currently only two
#' themes implemented - a standard plot and a dark theme. If \code{NULL} 
#' (default), the theme will be produced with a standard white background. If
#' \code{"dark"}, a dark gray background will be used with white text and axes.
#' @param ... Additional arguments passed to \link[graphics]{plot}. Note that
#' it is best to use the full argument rather than partial matching, given the
#' method used to call the plot. While some partial matching is supported 
#' (e.g., \code{m} for \code{main}, it is generally safest to supply the full
#' argument).
#' @return
#' The arguments supplied to the plot are silently returned for testing 
#' purposes.
#' @importFrom graphics par layout abline lines text polygon
#' @importFrom grDevices rgb
#' @export
#' @examples
#' # Prouduce default Probability-Probability plot with two groups
#' dev.off()
#' pp_plot(math ~ freelunch, star)
#' 
#' # Suppress shading and effect-size annotation
#' pp_plot(math ~ freelunch, 
#' 		star, 
#' 		shade = FALSE, 
#' 		text = FALSE)
#' 
#' # Change color of shading & line, line width, and title
#' pp_plot(math ~ freelunch, 
#' 		star, 
#' 		shade_rgb = rgb(0.1, 0.8, 0.2, 0.5), 
#' 		col = "purple", lwd = 5, 
#' 		main = "Probability-Probability Plot")
#' 
#' # Change to dark theme
#' pp_plot(math ~ freelunch, star, theme = "dark")
#' 
#' # Produce default PP plot w/multiple groups
#' pp_plot(mean ~ grade, seda)
#' 
#' # Change reference group to third grade
#' pp_plot(mean ~ grade, 
#' 		seda, 
#' 		ref_group = "3")
#' 

pp_plot <- function(formula, data, ref_group = NULL, annotate = FALSE, 
	refline = TRUE, refline_col = "gray40", refline_lty = 2, refline_lwd = 2,
	text = NULL, text_size = 2, shade = NULL, 
	shade_rgb = rgb(102, 178, 255, alpha = 30, maxColorValue = 255), 
 	legend = NULL, plot = TRUE, theme = NULL, ...) {

	ps <- probs(formula, data)

	if(ncol(ps) > 2 & !is.null(shade)) {
		if(shade == TRUE) {
			warning(
				paste("The area under the curve can only be shaded with two",
					"groups. Argument `shade = TRUE` ignored")
				)	
		}
		
	}
	if(ncol(ps) > 2 & !is.null(text)) {
		if(text == TRUE) {
			warning(
				paste("Text annotations can only be produced automatically",
					"with two groups. Argument `text = TRUE` ignored")
				)	
		}
		
	}
	if(ncol(ps) == 2 & all(par()$mfrow == c(1, 2))) {
		warning(
			paste("Two-panel plot detected without a legend. Call `dev.off()`",
				"and then rerun the plot to avoid wasted space, if only",
				"trying to plot a single curve. Ignore otherwise.")
			)
	}

	if(is.null(ref_group)) ref_group <- colnames(ps)[1]
	
	if(ncol(ps) > 2) {
		shade <- FALSE
		text <- FALSE
		if(is.null(legend)) legend <- "side"
	}
	if(ncol(ps) == 2) {
		if(is.null(legend)) legend <- "none"
		if(is.null(text)) text <- TRUE
		if(is.null(shade)) shade <- TRUE
	}

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

	if(any(par()$mfrow > 2) & any(par()$mfrow != c(1,2)) & legend == "side") {
		message(
			paste("Multi-panel settings detected with `legend == 'side'`.",
				"Change legend option if trying to produce a multi-panel", 
				"plot.")
			)
	}

	sq <- seq_len(ncol(ps))
	ref_group_d <- ps[ ,sq[colnames(ps) == as.character(ref_group)] ]

	if(plot == TRUE) {
		if(legend == "side") {
			layout(t(c(1, 2)), widths = c(0.9, 0.1))	
		}
		if(ncol(ps) == 2) {
			p <- empty_plot(ref_group_d, ps[ ,2], 
				paste0("p(",colnames(ps)[1],")"),
				paste0("p(",colnames(ps)[2],")"),
				paste(as.character(formula)[c(2, 1, 3)], collapse = " "),
				...)
		}
		if(ncol(ps) > 2) {
			p <- empty_plot(ref_group_d, ps[ ,2], 
				paste0("p(", ref_group, ")"),
				"p(Focal Group)",
				paste(as.character(formula)[c(2, 1, 3)], collapse = " "),
				...)
		}
		
		if(!is.null(theme)) {
			if(theme == "dark") {
				if(is.null(p$xaxt))	axis(1, col = "white")
				if(is.null(p$yaxt)) axis(2, col = "white")
				if(refline_col == "gray") refline_col <- "white"
				if(refline_lwd == 1) refline_lwd <- 2
			}
		}
		if(refline == TRUE) {
			abline(0, 1, 
				col = refline_col, 
				lty = refline_lty, 
				lwd = refline_lwd)
		}
	}
	ps_subset <- ps[ ,-sq[colnames(ps) == as.character(ref_group)], 
					drop = FALSE]
	
	
	if(is.null(p$lwd)) p$lwd <- 2
	if(is.null(p$lty)) p$lty <- 1
	if(is.null(p$col)) p$col <- col_hue(ncol(ps_subset))

	if((length(p$col) !=1) & (length(p$col) < ncol(ps_subset))) {
		warning(
			paste("Not enough colors supplied. Colors will be recycled",
				"when drawing lines.")
			)
	}
	if((length(p$lty) !=1) & (length(p$lty) < ncol(ps_subset))) {
		warning(
			paste("Not enough line types supplied. Line types will be",
				"recycled when drawing lines.")
			)
	}

	x_axs <- rep(ref_group_d, ncol(ps_subset))

	if(plot == TRUE) {
		Map(lines, 
			x = split(x_axs, 
					rep(seq_len(ncol(ps_subset)), each = nrow(ps_subset))), 
			y = split(ps_subset, 
					rep(seq_len(ncol(ps_subset)), each = nrow(ps_subset))),
		    col = p$col, 
			lwd = p$lwd,
			lty = p$lty)

		if(text == TRUE) {
			if(!is.null(theme)) {
				if(theme == "dark") {
					text(0.8, 0.2, cex = text_size, col = "white",
					paste0("AUC = ", 
								round(auc(formula, data, ref_group, FALSE), 2),
						   "\n", 
						   "V = ", 
						   		round(v(formula, data, ref_group, FALSE), 2)))
				}	
			}
			else {
				text(0.8, 0.2, cex = text_size, 
					paste0("AUC = ", 
								round(auc(formula, data, ref_group, FALSE), 2), 
						   "\n", 
						   "V = ", 
						   		round(v(formula, data, ref_group, FALSE), 2)))
			}
		}

		if(shade == TRUE) {
			xlims <- seq(0, 1, length = nrow(ps))
			polygon(c(xlims, rev(ps[ ,1])), 
					c(rep(-1, length(xlims)), rev(ps[ ,2])),
				col = shade_rgb,
				border = NA)
		}
		if(legend == "side") {
			create_legend(ncol(ps_subset), colnames(ps_subset), 
				col = p$col, 
				lwd = p$lwd, 
				lty = p$lty)
		}
		if(legend == "base") {
			if(is.null(theme)) {
				create_base_legend(colnames(ps_subset), 
					col = p$col, 
					lwd = p$lwd, 
					lty = p$lty)
			}
			if(!is.null(theme)) {
				if(theme == "dark") {
					create_base_legend(colnames(ps_subset), 
						col = p$col, 
						lwd = p$lwd, 
						lty = p$lty,
						text.col = "white")
				}
			}
		}
	}
	if(annotate == TRUE) {
		par(mfg = c(1, 1))
		empty_plot(ref_group_d, ps[ ,2], "", "", xaxt = "n", yaxt = "n", ...)
	} 
invisible(c(as.list(match.call()), p, op))
}
