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
#' @param cut Integer. Optional vector (or single number) of scores used to 
#' annotate the plot. If supplied, line segments will extend from the 
#' corresponding x and y axes and meet at the PP curve.
#' @param cut_table Logical. Should a data.frame of the cuts and corresponding
#' proportions be returned? Defaults to FALSE.
#' @param grid Logical. Should gridlines behind the plot be displayed 
#' according to the theme?
#' @param scheme What color scheme should the lines follow? Defaults to 
#' mimic the ggplot2 color scheme. Other options come from the 
#' \href{https://CRAN.R-project.org/package=viridisLite}{viridisLite}
#' package, and must be installed first. These are the same options available
#' in the package: "viridis", "magma", "inferno", and "plasma". These color 
#' schemes work well for color blindness and print well in black and white.
#' Alternatively, colors can be supplied manually through a call to \code{col}
#' (through \code{...}).
#' @param annotate Logical. Defaults to \code{FALSE}. When \code{TRUE} and 
#' \code{leg == "side"} the plot is rendered such that additional
#' annotations can be made on the plot using low level base plotting functions
#' (e.g., \link[graphics]{arrows}). However, if set to \code{TRUE}, 
#' \link[grDevices]{dev.off} must be called before a new plot is rendered 
#' (i.e., close the current plotting window). Otherwise the plot will be
#' attempted to be rendered in the region designated for the legend). Argument
#' is ignored when \code{leg != "side"}.
#' @param refline Logical. Defaults to \code{TRUE}. Should a diagonal
#'    reference line, representing the point of equal probabilities, be
#' 	  plotted?
#' @param refline_col Color of the reference line.
#' @param refline_lty Line type of the reference line.
#' @param refline_lwd Line width of the reference line.
#' @param text Logical. Should the \code{link{auc}} and \code{link{v}}
#'    statistics be displayed on the plot? Defaults to \code{TRUE} when there
#' 	  are two groups. Cannot currently be displayed for more than two groups.
#' @param text_x The x-axis location for the text.
#' @param text_y The y-axis location for the text.
#' @param text_size The size of the text to be displayed. Defaults to 2. 
#' @param shade Logical. Should the area under the curve be shaded? Defaults
#'    to \code{TRUE} if there are only two group. Currently it cannot be 
#'    produced for more than two groups.
#' @param shade_col The color of the shading. Defaults to the second color in
#' the chosen color \code{scheme}.
#' @param leg The type of legend to be displayed, with possible values 
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
#' @param n_1 Logical. Should the lines on the legend be displayed when there
#' is only one curve? Defaults to \code{FALSE}, and is relevant when values to
#' \code{cut} are provided. Forced to \code{TRUE} if \code{legend == "side"}
#' and no values to \code{cut} are supplied.
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
#' 		shade_col = grDevices::rgb(0.1, 0.8, 0.2, 0.5), 
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

pp_plot <- function(formula, data, ref_group = NULL, cut = NULL, 
	cut_table = FALSE, grid = NULL, scheme = "ggplot2", annotate = FALSE,
	refline = TRUE, refline_col = NULL, refline_lty = 2, refline_lwd = 2,
	text = NULL, text_x = 0.8, text_y = 0.2, text_size = 1.5, shade = NULL,
	shade_col = NULL, leg = NULL, n_1 = FALSE, theme = "standard", ...) {

	call <- as.list(match.call())
	
	calcs <- pp_calcs(formula, data, ref_group)
	
	if(!is.null(grid)) {
		if(grid & !is.null(cut)) {
			warning(paste("Grid suppressed when cut lines are also provided",
				"(plot becomes overly busy otherwise)."))
		}
	}

	if(!is.null(cut)) grid <- FALSE
	if(is.null(cut) & is.null(grid))  grid <- TRUE

	if(!is.null(leg)) {
		if(leg == "side") n_1 <- TRUE	
	} 
	if(ncol(calcs$ps) > 2 & !is.null(shade)) {
		if(shade == TRUE) {
			warning(
				paste("The area under the curve can only be shaded with two",
					"groups. Argument `shade = TRUE` ignored")
				)	
		}	
	}
	if(ncol(calcs$ps) > 2 & !is.null(text)) {
		if(text == TRUE) {
			warning(
				paste("Text annotations can only be produced automatically",
					"with two groups. Argument `text = TRUE` ignored")
				)	
		}
		
	}
	if(ncol(calcs$ps) == 2 & all(par()$mfrow == c(1, 2)) & is.null(cut)) {
		warning(
			paste("Two-panel plot detected without a legend. Call `dev.off()`",
				"and then rerun the plot to avoid wasted space, if only",
				"trying to plot a single curve. Ignore otherwise.")
			)
	}
	if(ncol(calcs$ps) > 2) {
		shade <- FALSE
		text <- FALSE
		if(is.null(leg)) leg <- "side"
	}
	if(ncol(calcs$ps) == 2) {
		if(is.null(leg) & is.null(cut)) leg <- "none"
		if(is.null(leg) & !is.null(cut)) leg <- "side"
		if(is.null(text)) text <- TRUE
		if(is.null(shade)) shade <- TRUE
	}

	op <- themes(theme)$op	
	on.exit(par(op))

	if(any(par()$mfrow > 2) & any(par()$mfrow != c(1,2)) & leg == "side") {
		message(
			paste("Multi-panel settings detected with `legend == 'side'`.",
				"Change legend option if trying to produce a multi-panel", 
				"plot.")
			)
	}

	if(leg == "side") {
		max_char <- max(nchar(colnames(calcs$ps_subset)))
		score_len <- max(nchar(paste0("Score: ", rownames(calcs$ps))))
		if(max_char < score_len & !is.null(cut)) max_char <- score_len
		wdth <- 0.9 - (max_char * 0.01)
		layout(t(c(1, 2)), widths = c(wdth, 1 - wdth))	
	}

	if(ncol(calcs$ps) == 2) {
		p <- empty_plot(calcs$ref_group_d, calcs$ps[ ,2], 
			paste0("p(",colnames(calcs$ps)[1],")"),
			paste0("p(",colnames(calcs$ps)[2],")"),
			paste(as.character(formula)[c(2, 1, 3)], collapse = " "),
			default_xaxt = "n",
			default_yaxt = "n",
			theme = theme,
			...)
	}
	if(ncol(calcs$ps) > 2) {
		p <- empty_plot(calcs$ref_group_d, calcs$ps[ ,2], 
			paste0("p(", calcs$ref_group, ")"),
			"p(Focal Group)",
			paste(as.character(formula)[c(2, 1, 3)], collapse = " "),
			default_xaxt = "n",
			default_yaxt = "n",
			theme = theme,
			...)
	}
	if(is.null(p$lwd)) p$lwd <- 2
	if(is.null(p$lty)) p$lty <- 1
	if(is.null(p$col)) p$col <- col_scheme(scheme, ncol(calcs$ps_subset))

	if((length(p$col) !=1) & (length(p$col) < ncol(calcs$ps_subset))) {
		warning(
			paste("Not enough colors supplied. Colors will be recycled",
				"when drawing lines.")
			)
	}
	if((length(p$lty) !=1) & 
		(length(p$lty) < ncol(calcs$ps_subset))) {
		warning(
			paste("Not enough line types supplied. Line types will be",
				"recycled when drawing lines.")
			)
	}
	if(grid){
		grid(col = adjustcolor(themes(theme)$line_col, alpha.f = 0.3))
	}
	if(shade) {
		xlims <- seq(0, 1, length = nrow(calcs$ps))
		if(is.null(shade_col))  {
			shade_col <- col_scheme(scheme, 2, alpha = .3)[2]
		}
		polygon(c(xlims, rev(calcs$ps[ ,1])), 
				c(rep(-1, length(xlims)), rev(calcs$ps[ ,2])),
			col = shade_col,
			border = NA)
	}
	if(text) {
		pp_annotate(formula, data, calcs$ref_group, text_x, text_y, text_size,
			theme)
	}
	if(refline) {
		abline(0, 1, 
			col = themes(theme)$line_col, 
			lty = refline_lty, 
			lwd = refline_lwd)
	}
	Map(lines, 
		x = calcs$x_lines, 
		y = calcs$y_lines,
	    col = p$col, 
		lwd = p$lwd,
		lty = p$lty)

	if(!is.null(cut)) cut_cols <- create_cut_refs(cut, calcs, p, scheme)

	if(leg == "side") {
		create_legend(ncol(calcs$ps_subset),
			colnames(calcs$ps_subset),
			main_cols = p$col,
			cut_cols = cut_cols,
			lwd = 2,
			left_mar = max_char * .35,
			n_1 = n_1,
			cut = cut)	
	}
	if(leg == "base") {
		create_base_legend(colnames(calcs$ps_subset), 
			col = p$col, 
			lwd = p$lwd, 
			lty = p$lty,
			text.col = "white")
	
	}
	if(annotate == TRUE) {
		par(mfg = c(1, 1))
		empty_plot(calcs$ref_group_d, calcs$ps[ ,2], 
			"", 
			"", 
			xaxt = "n", 
			yaxt = "n", 
			...)
	} 
	if(cut_table) {
		cuts <- as.data.frame.table(cuts, responseName = "proportion")
		names(cuts)[1:2] <- c("cut", "group")
	return(cuts)
	}
invisible(c(call, p, op))
}
