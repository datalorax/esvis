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
#' @param es If text annotations should be made, which effect size should be
#' plotted? Options include \code{\link{pac}} or \code{\link{tpac}} and 
#' defaults to the latter.
#' @param legend Logical. Should the legend be plotted? Defaults to 
#' \code{TRUE}.
#' @param return Logical. Should the arguments passed to \link[graphics]{plot}
#' returned (in quoted form)? Defaults to \code{FALSE}.
#' @param ... Additional arguments passed to \link[graphics]{plot}. Note that
#' it is best to use the full argument rather than partial matching, given the
#' method used to call the plot. While some partial matching is supported 
#' (e.g., \code{m} for \code{main}, it is generally safest to supply the full
#' argument).
#' @import graphics 
#' @export


ecdf_plot <- function(formula, data, v_ref = NULL, ref_groups = NULL, ref_col = NULL, text = TRUE, es = "tpac", legend = TRUE, return = FALSE, ...) {
	splt <- parse_form(formula, data)
	ecdfs <- cdfs(formula, data)

	op <- par()
	op <- op[-grep(c("cin|cra|csi|cxy|din|page"), names(op))]
	on.exit(par(op))

	x_lim <- seq(min(sapply(splt, min, na.rm = TRUE)),
				max(sapply(splt, max, na.rm = TRUE)), 
				.1)

	pargs <- list(x = x_lim, 
				  y = seq(0, 1, length = length(x_lim)),
				  type = "n",
				  ...)

	if(legend == TRUE) {
		layout(t(c(1, 2)), widths = c(0.9, 0.1))
	}
	if(is.null(pargs$xlab)) {
		if(!is.null(pargs$xla)) {
			pargs$xlab <- pargs$xla
			pargs$xla <- NULL
		}
		pargs$xlab <- "Scale"
	}
	if(is.null(pargs$ylab)) {
		if(!is.null(pargs$yla)) {
			pargs$ylab <- pargs$yla
			pargs$yla <- NULL
		}
		pargs$ylab <- "f(x)"
	}
	if(is.null(pargs$main)) {

		# check for partial matching
		if(length(grep("m", names(pargs))) > 0) {
			pargs$main <- pargs[[grep("^m", names(pargs))]]
			pargs[grep("^m", names(pargs))[1]] <- NULL
		}
		else { 
			pargs$main <- paste(as.character(formula)[c(2, 1, 3)],
							collapse = " ")
		}
	}
	if(is.null(pargs$bty)) pargs$bty <- "n"
	
	do.call("plot", pargs)

	if(is.null(pargs$lwd)) pargs$lwd <- 2
	if(is.null(pargs$lty)) pargs$lty <- 1
	if(is.null(pargs$col)) pargs$col <- col_hue(length(splt))

	Map(lines, 
		ecdfs,
	    col = pargs$col, 
		lwd = pargs$lwd,
		lty = pargs$lty,
		col.01line = FALSE, 
		verticals = TRUE, 
		do.points = FALSE)
	
	if(!is.null(v_ref)) {
		if(is.null(ref_col)) ref_col <- "gray70"
		if(is.null(ref_groups)) {
			abline(v = v_ref, col = ref_col, lty = 2, lwd = 2)
		}
		
		if(!is.null(ref_groups)) {
			tpacs <- tpac(formula, data, v_ref)
			y_refs <- sapply(ecdfs, function(x) x(v_ref))

			segments(x0 = v_ref, x1 = v_ref,
					 y0 = y_refs[ref_groups[1]], y1 = y_refs[ref_groups[2]],
					 col = ref_col,
					 #lty = 2,
					 lwd = 2)
			if(text == TRUE) {
				text(x = min(x_lim), 
					 y = mean(y_refs[ref_groups[1]], y_refs[ref_groups[2]]),
					 abs(round(abs(y_refs[ref_groups[1]]) - 
					 		   abs(y_refs[ref_groups[2]]), 2)),
					 col = ref_col)
			}
		}
	}

	if(legend == TRUE) {
		create_legend(length(splt), names(splt),
			col = pargs$col,
			lwd = pargs$lwd,
			lty = pargs$lty)
	}

if(return == TRUE) c(as.list(match.call()), pargs)
}