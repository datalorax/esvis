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
#' @param shade_rgb The color of the shading via \code{\link{grDevices::rgb}}. 
#'    Defaults to \code{rgb(102, 178, 255, alpha = 30, max = 255)} which is a 
#'    light blue color.
#' @param legend Logical. Should the legend be plotted? Defaults to \code{TRUE}
#'    when the number of groups is greater than 2. Legend not available when
#'    the number of groups == 2. 
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
	shade_rgb = rgb(102, 178, 255, alpha = 30, max = 255), 
 	legend = NULL, colors = NULL, ...) {

	op <- par()
	op <- op[-grep(c("cin|cra|csi|cxy|din|page"), names(op))]
	on.exit(par(op))

	ps <- probs(formula, data)

	if(ncol(ps) > 2 & !is.null(shade)) {
		if(shade == TRUE) {
			stop("The area under the curve can only be shaded with two groups. Remove `shade = TRUE` argument.")	
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
	plot(ref_group_d, ps[ ,2], 
		type = "n",
		lwd = 2, 
		xlim = c(0, 1), 
		ylim = c(0, 1),
		xlab = "",
		ylab = "",
		main = paste(as.character(formula)[c(2, 1, 3)], collapse = " "),
		bty = "n",
		...)
	
	if(ncol(ps) == 2) {
		title(xlab = paste0("p(", colnames(ps)[1], ")"), 	
			  ylab = paste0("p(", colnames(ps)[2], ")"))
	} 
	
	if(ncol(ps) > 2) {
		title(xlab = paste0("p(", ref_group, ")"),
			  ylab = "p(Focal Group)")	
	} 

	ps_subset <- ps[ ,-sq[colnames(ps) == as.character(ref_group)], 
					drop = FALSE]

	if(is.null(colors)) colors <- col_hue(ncol(ps_subset))
	
	for(i in 1:(ncol(ps) - 1)) {
		lines(ref_group_d, 
			  ps_subset[ ,i], 
			  col = colors[i], 
			  lwd = 2) 	
	} 
	
	if(refline == TRUE)	abline(0, 1, col = "gray", lty = 2)

	if(text == TRUE) {
		text(0.8, 0.2, cex = 2, 
			paste0("AUC = ", round(auc(ps), 2), "\n", "V = ", round(v(ps), 2)))
			
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
	
}
