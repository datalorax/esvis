#' Parse formula
#' @param formula A formula of the type \code{out ~ group} where \code{out} is
#' the outcome variable and \code{group} is the grouping variable. Note this
#' variable can include any arbitrary number of groups.
#' @param data The data frame that the data in the formula come from.
#' @param order Logical. Defaults to \code{TRUE}. Should the groups be ordered 
#' according to their mean?
#' @return A list of data split by the grouping factor.
parse_form <- function(formula, data, order = TRUE) {
	out <- data[[ all.vars(formula)[1] ]]
	group <- data[[ all.vars(formula)[2] ]]	

	splt <- split(out, group)
	if(order == TRUE) {
		means <- vapply(splt, mean, na.rm = TRUE, numeric(1))
		splt <- splt[order(means, decreasing = TRUE)]
	}
splt
}


#' Theme settings
#' 
#' Parameters for each theme currently implemented.
#' @param theme The name of the theme.
#' @return list with the \code{par} settings and the primary line color (e.g., 
#' white for theme = "dark" and black for theme = "standard").
themes <- function(theme) {
	op <- switch(theme,
		standard = par(bg = "white"),
		dark 	 = par(bg = "gray21", 
					  col.axis = "white", 
					  col.lab = "white",
					  col.main = "white"),
		tan 	 = par(bg = "cornsilk2",
					   col.axis = "darkslategray", 
					   col.lab = "darkslategray",
					   col.main = "darkslategray"),
		blue 	 = par(bg = "cornflowerblue",
					   col.axis = "deeppink", 
					   col.lab = "deeppink",
					   col.main = "deeppink"))

	line_col <- switch(theme,
		standard = "black",
		dark 	 = "white",
		tan 	 = "darkslategray",
		blue 	 = "deeppink")
list(op = op, line_col = line_col)
}


#' Create a named vector of all possible combinations
#' 
#' Alternative to tidied data frame return.
#' 
#' @param levs The levels of the grouping factor from which to create the
#' matrix
#' @param fun The function to apply.
#' @return Matrix of values according to the function supplied.

create_vec <- function(levs, fun) {
	combos_1 <- t(utils::combn(levs, 2))
	combos_2 <- t(utils::combn(rev(levs), 2))

	diff_1 <- mapply(fun, split(combos_1, seq_len(nrow(combos_1))))
	diff_2 <- mapply(fun, split(combos_2, seq_len(nrow(combos_2))))

	set1 <- vapply(split(combos_1, seq_len(nrow(combos_1))), 
				paste0, collapse = "-", character(1))
	set2 <- vapply(split(combos_2, seq_len(nrow(combos_2))), 
				paste0, collapse = "-", character(1))

	vec <- c(mapply(fun, split(combos_1, seq_len(nrow(combos_1)))),
			 mapply(fun, split(combos_2, seq_len(nrow(combos_2)))))
	names(vec) <- c(set1, set2)
vec
}

tidy_out <- function(levs, fun) {
	combos_1 <- t(utils::combn(levs, 2))
	combos_2 <- t(utils::combn(rev(levs), 2))

	diff_1 <- mapply(fun, split(combos_1, seq_len(nrow(combos_1))))
	diff_2 <- mapply(fun, split(combos_2, seq_len(nrow(combos_2))))

	if(length(dim(diff_1)) == 0) {
		td <- as.data.frame(rbind(combos_1, combos_2))
		names(td) <- c("ref_group", "foc_group")
		td$estimate <- c(diff_1, diff_2)
	}
	else{
		g1 <- as.data.frame(
				split(
					rep(combos_1, nrow(diff_1)), 
					rep(1:2, each = nrow(combos_1))
					)
				)
		g2 <- as.data.frame(
				split(
					rep(combos_2, nrow(diff_2)), 
					rep(1:2, each = nrow(combos_2))
					)
				)
		
		td <- as.data.frame(rbind(g1, g2))
		names(td) <- c("ref_group", "foc_group")
		
		if(is.null(rownames(diff_1))) {
			warning("Cut score not specified as row names")
		}
		
		td$cut <- c(rep(rownames(diff_1), each = nrow(combos_1)), 
					rep(rownames(diff_2), each = nrow(combos_2)))

		colnames(diff_1) <- apply(combos_1, 1, paste, collapse = "-")
		colnames(diff_2) <- apply(combos_2, 1, paste, collapse = "-")

		td$estimate <- c(t(diff_1), t(diff_2))
	}
td
}


#' Compute the empirical distribution functions for each of several groups.
#' 
#' This function is a simple wrapper that splits the data frame by the 
#' grouping variable, then loops \link[stats]{ecdf} through the split
#' data to return a CDF function for each group.
#' @param formula A formula of the type \code{out ~ group} where \code{out} is
#' the outcome variable and \code{group} is the grouping variable. Note this
#' variable can include any arbitrary number of groups.
#' @param data The data frame that the data in the formula come from.
#' @param center Logical. Should the functions be centered prior to plotting?
#' @return A list with one function per group (level in the grouping factor).
#' @export
#' @examples
#' cdfs(math ~ condition, star)

cdfs <- function(formula, data, center = FALSE) {
	splt <- parse_form(formula, data)
	if(center) {
		splt <- lapply(splt, function(x) as.numeric(scale(x, scale = FALSE)))
	}
lapply(splt, stats::ecdf)
}

#' Compute probabilities from the empirical CDFs of a grouping variable for
#' each group.
#' 
#' This formula returns the paired probabilities for any 
#' @param formula A formula of the type \code{out ~ group} where \code{out} is
#' the outcome variable and \code{group} is the grouping variable. Note this
#' variable can include any arbitrary number of groups.
#' @param data The data frame that the data in the formula come from.
#' @param center Logical. Should the functions be centered prior to plotting?
#' @return A matrix of probabilities with separate columns for each group and
#' \code{rownames} corresponding to the value the paired probabilities are 
#' calculated from.
#' @examples
#' probs(math ~ condition, star)
#' @importFrom stats sd
#' @export

probs <- function(formula, data, center = FALSE) {
	ecdfs <- cdfs(formula, data, center)
	out <- data[[ all.vars(formula)[1] ]]
	if(center) {
		out <- scale(out, scale = FALSE)
	}
	range <- seq(min(out, na.rm = TRUE) - floor(sd(out, na.rm = TRUE)), 
				  max(out, na.rm = TRUE) + ceiling(sd(out, na.rm = TRUE)),
				  1)

	ps <- vapply(ecdfs, function(x) x(range), numeric(length(range)))
	colnames(ps) <- names(ecdfs)
	rownames(ps) <- range
ps
}

#' Match segments on a plot
#'
#' Given an x and y coordinate, this function will produce segments that 
#' extend from -1 to the corresponding xy intersection, with a point
#' at the intersection. 
#' 
#' @param x The x-coordinate.
#' @param y The y-coordinate.
#' @param ... Additional parameters passed to \link[graphics]{segments}. Note
#' that whatever parameters are passed here are also passed to 
#' \link[graphics]{points} (e.g., \code{col}).
#' @export
#' @examples
#' plot(1:10, (1:10)^2, type = "l")
#' seg_match(3, 9)
#' seg_match(c(6, 8), c(36, 64), 
#' 	col = c("blue", "green"),
#' 	pch = 21, 
#' 	bg = c("blue", "green"), 
#' 	lty = 3)
seg_match <- function(x, y, ...) {
	segments(x, -1, x, y, ...)
	segments(-1, y, x, y, ...)
	points(x, y, ...)
}

#' Color hues
#'
#' Emulates ggplot's default colors. Evenly spaced hues around the color wheel.
#' 
#' @param n The number of colors to be produced
#' @param ... Additional arguments passed to \link[grDevices]{hcl}, such as 
#' \code{alpha}.
#' @export
#' @examples
#' col_hue(1)
#' col_hue(5)
#' col_hue(20)

col_hue <- function(n, ...) {
  hues <- seq(15, 375, length = n + 1)
  grDevices::hcl(h = hues, c = 100, l = 65, ...)[seq_len(n)]
}

#' Determine the color scheme to be used for the plotting
#' 
#' @param scheme The chosen color scheme. Options are "ggplot2", "viridis",
#' "magma", "inferno", or "plasma". Note all but "ggplot2" depend upon the
#' \href{https://CRAN.R-project.org/package=viridisLite}{viridisLite} package
#' @param n The number of colors to be produced for the given scheme.
#' @param ... Additional arguments passed, typically being \code{alpha}.

col_scheme <- function(scheme, n, ...) {
	if(any(scheme == "viridis" |
			   scheme == "magma" |
			   scheme == "inferno" |
			   scheme == "plasma")) {
		if(!any(is.element("viridisLite", installed.packages()[ ,1]))) {
			stop(paste0("Please install the viridisLite ",
			 	"package to use this color scheme"))
		}
	}
	switch(scheme,
			ggplot2 = col_hue(n, ...),
			viridis = viridisLite::viridis(n, ...),
			magma 	= viridisLite::magma(n, ...),
			inferno	= viridisLite::inferno(n, ...),
			plasma 	= viridisLite::plasma(n, ...)
			)
} 

#' Create a legend for a plot
#' 
#' This is an alternative legend for plots which uses the actual 
#' plotting environment to create the legend, rather than overlaying it. I 
#' prefer this legend because it scales better than the base legend. It is
#' currently only implemented to support lines.
#' 
#' @param n Number of lines to produce on the legend.
#' @param leg_labels Labels for the lines in the legend.
#' @param left_mar Left margin argument. Defaults to 0. Larger numbers push the
#' legend more to the right.
#' @param height The height of the legend. Counter-intuitively, larger numbers
#' result in a smaller legend (more squished to the bottom). 
#' @param main_cols Primary colors (of the lines, rather than the cut scores)
#' @param cut Cut scores (see \link{pp_plot}).
#' @param cut_cols The color of the lines/points for the cut scores.
#' @param n_1 Should the lines on the legend be displayed when there
#' is only one value? Defaults to \code{FALSE}, and is relevant when values to
#' \code{cut} are provided. 
#' @param ... Additional arguments passed to \link[graphics]{lines}.
#' @importFrom graphics plot lines axis

create_legend <- function(n, leg_labels, left_mar = 0, height = NULL, 
	main_cols = NULL, cut = NULL, cut_cols = NULL, n_1 = FALSE, ...) {
	
	op <- par(mar = c(5.1, left_mar, 4.1, 0))
	on.exit(par(op))
	

	if(is.null(height)) {
		if(n < 15) {
			height <-  20
		}
		else {
			height <- n * 1.5
		}
	}

	plot(seq(0, 1, length = height), 
		 seq_len(height),
		type = "n",
		bty = "n", 
		xaxt = "n",
		xlab = "", 
		yaxt = "n",
		ylab = "")	

	if(n > 1 | n_1) {
		axes <- cbind(c(0, 1), rep(seq_len(n), each = 2))
		Map(lines, 
			split(axes[ ,1], axes[ ,2]), 
			split(axes[ ,2], axes[ ,2]),
			col = main_cols,
			...)

		locs <- unique(axes[ ,2])
	}
	if(!is.null(cut)) {
		axes_cut <- cbind(c(0, 1), 
					  rep(max(height):(max(height) - (length(cut) - 1)), 
					  	each = 2))	
		Map(lines, 
			split(axes_cut[ ,1], axes_cut[ ,2]), 
			split(axes_cut[ ,2], axes_cut[ ,2]),
			col = cut_cols,
			lty = 3,
			...)
		points(rep(0.5, length(cut)), rev(unique(axes_cut[ ,2])),
			col = cut_cols,
			bg = cut_cols,
			pch = 21)
		
		if(n > 1 | n_1) {
			leg_labels <- c(leg_labels, paste0("Score: ", rev(cut)))
		}
		else {
			leg_labels <- paste0("Score: ", rev(cut))
		}
		
		if(exists("locs")) {
			locs <- c(locs, unique(axes_cut[ ,2]))
		}
		else {
			locs <- unique(axes_cut[ ,2])
		}
	}
	axis(2, 
		lwd = 0, 
		at = locs,
		labels = leg_labels, 
		las = 2)
}

#' Create a base legend for a plot
#' 
#' This function creates a legend using the base \link[graphics]{legend}
#' function, but with more abbreviated syntax.
#' 
#' @param labels Labels for the legend (line labels).
#' @param position Where the legend should be positioned. Defaults to
#'  \code{"bottomright"}.
#' @param ... Additional arguments passed to \link[graphics]{legend} 
#' (typically colors, line width, etc).
#' @importFrom graphics legend

create_base_legend <- function(labels, position = "bottomright", ...) {

	legend(position, 
			legend = labels, 
			box.lwd = 0,
			...)
}

#' Create an empty plot
#' 
#' This function creates an empty plot for further plotting (e.g., via 
#' \link[graphics]{lines}). What makes the function unique is that it allows
#' for specification of default \code{xlab}, \code{ylab}, and \code{main}
#' arguments, while allowing the user to override those arguments. Only really
#' useful when used within other functions (e.g., \link{pp_plot}).
#' 
#' @param x The x variable to be plotted.
#' @param y The y variable to be plotted.
#' @param default_xlab The default x-label, which can be overridden by the 
#' user. Defaults to \code{NULL}, in which case the label is defined by
#' the default \link[graphics]{plot} function.
#' @param default_ylab The default y-label, which can be overridden by the 
#' user. Defaults to \code{NULL}, in which case the label is defined by
#' the default \link[graphics]{plot} function.
#' @param default_main The default main title, which can be overridden by the 
#' user. Defaults to \code{NULL}, in which case the title is defined by
#' the default \link[graphics]{plot} function.
#' @param default_bty The default background type, which can be overridden by 
#' the user. Defaults to \code{"n"}.
#' @param default_xlim The default x-axis limits, which can be overridden by 
#' the user. Defaults to \code{NULL}, in which case the limits are defined by
#' the default \link[graphics]{plot} function.
#' @param default_ylim The default y-axis limits, which can be overridden by 
#' the user. Defaults to \code{NULL}, in which case the limits are defined by
#' the default \link[graphics]{plot} function.
#' @param default_xaxt The default x-axis type, which can be overridden by the 
#' user. Defaults to \code{NULL}, in which case the type is defined by
#' the default \link[graphics]{plot} function.
#' @param default_yaxt The default y-axis type, which can be overridden by the 
#' user. Defaults to \code{NULL}, in which case the type is defined by
#' the default \link[graphics]{plot} function.
#' @param theme The theme to be applied.
#' @param las The axis option. Defaults to \code{c(1, 2)} which makes the 
#' labels all horizontal.
#' @param ... Additional arguments supplied to \link[graphics]{plot} (e.g., 
#' \code{xlim}, \code{ylim}, \code{cex}, etc.)
#' @importFrom graphics plot

empty_plot <- function(x, y, default_xlab = NULL, default_ylab = NULL, 
	default_main = NULL, default_xlim = NULL, default_ylim = NULL, 
	default_xaxt = NULL, default_yaxt = NULL, default_bty = "n",
	theme = "standard", las = c(1, 2), ...) {

	pargs <- list(x = quote(x), 
				  y = quote(y),
				  type = "n",
				  ...)

	if(is.null(pargs$xlab)) {
		pargs$xlab <- default_xlab
		
		# check for partial matching
		if(!is.null(pargs$xla)) {
			pargs$xlab <- pargs$xla
			pargs$xla <- NULL
		}
	}
	if(is.null(pargs$ylab)) {
		pargs$ylab <- default_ylab
		
		# check for partial matching
		if(!is.null(pargs$yla)) {
			pargs$ylab <- pargs$yla
			pargs$yla <- NULL
		}
	}
	if(is.null(pargs$xlim)) {
		pargs$xlim <- default_xlim
		
		if(!is.null(pargs$xli)) {
			pargs$xlim <- pargs$xli
			pargs$xli <- NULL
		}
	}
	if(is.null(pargs$ylim)) {
		pargs$ylim <- default_ylim
		
		if(!is.null(pargs$yli)) {
			pargs$ylim <- pargs$yli
			pargs$yli <- NULL
		}
	}
	if(!is.null(pargs$xaxt)) user_xaxt <- pargs$xaxt 
	if(is.null(pargs$xaxt)) pargs$xaxt <- "n"
	
	if(!is.null(pargs$yaxt)) user_yaxt <- pargs$yaxt 
	if(is.null(pargs$yaxt)) pargs$yaxt <- "n"
		
	
	if(is.null(pargs$main)) {
		
		# check for partial matching
		if(length(grep("^m", names(pargs))) > 0) {
			pargs$main <- pargs[[grep("^m", names(pargs))]]
			pargs[grep("^m", names(pargs))[1]] <- NULL
		}
		else { 
			pargs$main <- default_main
		}
	}
	if(is.null(pargs$bty)) pargs$bty <- default_bty

	do.call("plot", pargs)

	if(!exists("user_xaxt")) {
		axis(1, col = themes(theme)$line_col, las = las[1])	
	}
	if(!exists("user_yaxt")) {
		axis(2, col = themes(theme)$line_col, las = las[2])	
	}
	
invisible(pargs)
}
