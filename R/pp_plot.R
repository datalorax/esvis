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
#' @param colors Color of the lines to be plotted. Defaults to
#'    \code{\link{col_hue}} with the corresponding number of groups.
#' @param return Logical. Should the arguments passed to \link[graphics]{plot}
#' returned (in quoted form)? Defaults to \code{FALSE}.
#' @param ... Additional arguments passed to \link[graphics]{plot}.
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
 	legend = NULL, colors = NULL, return = FALSE, ...) {

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
	
	if(is.null(pargs$xlim)) {
		pargs$xlim <- quote(c(0, 1))
	}
	if(is.null(pargs$ylim)) {
		pargs$ylim <- quote(c(0, 1))
	}	
	if(is.null(pargs$xlab)) {
		if(ncol(ps) == 2) pargs$xlab <- quote(paste0("p(",colnames(ps)[1],")"))
		if(ncol(ps) > 2) pargs$xlab <- quote(paste0("p(", ref_group, ")"))
	}
	if(is.null(pargs$ylab)) {
		if(ncol(ps) == 2) pargs$ylab <- quote(paste0("p(",colnames(ps)[2],")"))
		if(ncol(ps) > 2) pargs$ylab <- quote("p(Focal Group)")
	}
	if(is.null(pargs$main)) {
		pargs$main <- quote(paste(as.character(formula)[c(2, 1, 3)], 
						collapse = " "))
	}
	if(is.null(pargs$bty)) {
		pargs$bty <- quote("n")
	}
	do.call("plot", pargs)
	
	ps_subset <- ps[ ,-sq[colnames(ps) == as.character(ref_group)], 
					drop = FALSE]

	if(is.null(colors)) colors <- col_hue(ncol(ps_subset))
	
	if(is.null(pargs$lwd)) pargs$lwd <- 2
	if(is.null(pargs$lty)) pargs$lty <- 1
	
	for(i in 1:ncol(ps_subset)) {
		lines(ref_group_d, 
			  ps_subset[ ,i], 
			  col = colors[i], 
			  lwd = pargs$lwd,
			  lty = pargs$lty) 	
	} 
	
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
		par(mar = c(5.1, 0, 4.1, 0))
		plot(seq(0, 1, length = ncol(ps_subset) * 1.75), 
			 seq(1, ncol(ps_subset) * 1.75, length = ncol(ps_subset) * 1.75),
			type = "n",
			bty = "n", 
			xaxt = "n",
			xlab = "", 
			yaxt = "n",
			ylab = "")

		axes <- cbind(c(0, 1), rep(1:ncol(ps_subset), each = 2))	
		
		Map(lines, 
			split(axes[ ,1], axes[ ,2]), 
			split(axes[ ,2], axes[ ,2]),
			col = as.list(colors),
			lwd = 2)
		axis(2, 
			lwd = 0, 
			at = 1:ncol(ps_subset), 
			labels = colnames(ps_subset), 
			las = 2)
	}
if(return == TRUE) pargs
}
