#' Produces the paired probability plot for two groups
#' 
#' The paired probability plot maps the probability of obtaining a specific
#'    score for each of two groups. The area under the curve 
#'    (\code{\link{auc}}) corresponds to the probability that the x-axis group
#' 	   will score higher than the y-axis group.
#' 
#' @param formula  A formula of the type \code{out ~ group} where \code{out} is
#'   the outcome variable and \code{group} is the grouping variable. Note the
#'   grouping variable must only include only two groups.
#' @param data The data frame that the data in the formula come from.
#' @param ref_group Optional character vector (of length 1) naming the
#'   reference group to be plotted on the x-axis. Defaults to the highest
#'   scoring group.
#' @param refline Logical. Defaults to \code{TRUE}. Should a diagonal
#'    reference line, representing the point of equal probabilities, be
#' 	  plotted?
#' @param text Logical. Should the \code{link{auc}} and \code{link{v}}
#'    statistics be displayed on the plot? Defaults to \code{TRUE} when there
#' 	  are two groups. Cannot currently be displayed for more than two groups. 
#' @param shade Logical. Should the area under the curve be shaded? Defaults
#'    to \code{TRUE} if there are only two group. Currently it cannot be 
#'    produced for more than two groups.
#' @param shade_rgb The color of the shading via \link[grDevices]{rgb}. 
#'    Defaults to \code{rgb(102, 178, 255, alpha = 30, max = 255)} which is a 
#'    light blue color.
#' @param legend Logical. Should the legend be plotted? Defaults to \code{TRUE}
#'    when the number of groups is greater than 2. Legend not available when
#'    the number of groups == 2.
#' @param return Logical. Should the arguments passed to \link[graphics]{plot}
#' returned (in quoted form)? Defaults to \code{FALSE}.
#' @param ... Additional arguments passed to \link[graphics]{plot}. Note that
#' it is best to use the full argument rather than partial matching, given the
#' method used to call the plot. While some partial matching is supported 
#' (e.g., \code{m} for \code{main}, it is generally safest to supply the full
#' argument).
#' @import graphics grDevices
#' @export
#' @examples
#' free_reduced <- rnorm(800, 80, 20)
#'  pay <- rnorm(500, 100, 10)
#'  d <- data.frame(score = c(free_reduced, pay), 
#'  				frl = c(rep("free_reduced", 800),  
#' 							rep("pay", 500)))
#' 
#'  pp_plot(score ~ frl, d)

pp_plot <- function(formula, data, ref_group = NULL, refline = TRUE, 
	text = NULL, shade = NULL, 
	shade_rgb = rgb(102, 178, 255, alpha = 30, maxColorValue = 255), 
 	legend = NULL, return = FALSE, ...) {

	op <- par()
	op <- op[-grep(c("cin|cra|csi|cxy|din|page"), names(op))]
	on.exit(par(op))

	ps <- probs(formula, data)

	if(ncol(ps) > 2 & !is.null(shade)) {
		if(shade == TRUE) {
			warning("The area under the curve can only be shaded with two groups. Argument `shade = TRUE` ignored")	
		}
		
	}
	if(ncol(ps) > 2 & !is.null(text)) {
		if(text == TRUE) {
			warning("Text annotations can only be produced automatically with two groups. Argument `text = TRUE` ignored")	
		}
		
	}

	if(is.null(ref_group)) ref_group <- colnames(ps)[1]
	
	if(ncol(ps) > 2) {
		shade <- FALSE
		text <- FALSE
		if(is.null(legend)) legend <-  TRUE
	}
	if(ncol(ps) == 2) {
		if(is.null(legend)) legend <- FALSE
		if(is.null(text)) text <- TRUE
		if(is.null(shade)) shade <- TRUE
	}

	if(legend == TRUE) {
		layout(t(c(1, 2)), widths = c(0.9, 0.1))
	}

	sq <- 1:ncol(ps)
	ref_group_d <- ps[ ,sq[colnames(ps) == as.character(ref_group)] ]

	pargs <- list(x = quote(ref_group_d), 
				  y = quote(ps[ ,2]),
				  type = "n",
				  ...)
	
	if(is.null(pargs$xlab)) {
		if(ncol(ps) == 2) pargs$xlab <- paste0("p(",colnames(ps)[1],")")
		if(ncol(ps) > 2) pargs$xlab <- paste0("p(", ref_group, ")")

		# check for partial matching
		if(!is.null(pargs$xla)) {
			pargs$xlab <- pargs$xla
			pargs$xla <- NULL
		}
	}
	if(is.null(pargs$ylab)) {
		if(ncol(ps) == 2) pargs$ylab <- paste0("p(",colnames(ps)[2],")")
		if(ncol(ps) > 2) pargs$ylab <- "p(Focal Group)"

		if(!is.null(pargs$yla)) {
			pargs$ylab <- pargs$yla
			pargs$yla <- NULL
		}
	}
	if(is.null(pargs$main)) {
		
		# check for partial matching
		if(length(grep("m", names(pargs))) > 0) {
			pargs$main <- pargs[[grep("m", names(pargs))]]
			pargs[grep("m", names(pargs))[1]] <- NULL
		}
		else { 
			pargs$main <- paste(as.character(formula)[c(2, 1, 3)],
							collapse = " ")
		}
	}
	if(is.null(pargs$bty)) pargs$bty <- "n"
	
	do.call("plot", pargs)
	
	ps_subset <- ps[ ,-sq[colnames(ps) == as.character(ref_group)], 
					drop = FALSE]
	
	
	if(is.null(pargs$lwd)) pargs$lwd <- 2
	if(is.null(pargs$lty)) pargs$lty <- 1
	if(is.null(pargs$col)) pargs$col <- col_hue(ncol(ps_subset))

	if((length(pargs$col) !=1) & (length(pargs$col) < ncol(ps_subset))) {
		warning("Not enough colors supplied. Colors will be recycled when drawing lines.")
	}
	if((length(pargs$lty) !=1) & (length(pargs$lty) < ncol(ps_subset))) {
		warning("Not enough line types supplied. Line types will be recycled when drawing lines.")
	}

	x_axs <- rep(ref_group_d, ncol(ps_subset))

	Map(lines, 
		x = split(x_axs, rep(1:ncol(ps_subset), each = nrow(ps_subset))), 
		y = split(ps_subset, rep(1:ncol(ps_subset), each = nrow(ps_subset))),
	    col = pargs$col, 
		lwd = pargs$lwd,
		lty = pargs$lty)

	if(refline == TRUE)	abline(0, 1, col = "gray", lty = 2)

	if(text == TRUE) {
		text(0.8, 0.2, cex = 2, 
			paste0("AUC = ", round(auc(formula, data, ref_group, FALSE), 2), 
				   "\n", 
				   "V = ", round(v(formula, data, ref_group, FALSE), 2)))	
	}

	if(shade == TRUE) {
		xlims <- seq(0, 1, length = nrow(ps))
		polygon(c(xlims, rev(ps[ ,1])), 
				c(rep(-1, length(xlims)), rev(ps[ ,2])),
			col = shade_rgb,
			border = NA)
	}

	if(legend == TRUE) {
		create_legend(ncol(ps_subset), colnames(ps_subset), 
			col = pargs$col, 
			lwd = pargs$lwd, 
			lty = pargs$lty)
	}
if(return == TRUE) c(as.list(match.call()), pargs)
}
