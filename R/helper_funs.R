#' Parse formula
#' @param formula A formula of the type \code{out ~ group} where \code{out} is
#' the outcome variable and \code{group} is the grouping variable. Note this
#' variable can include any arbitrary number of groups.
#' @param data The data frame that the data in the formula come from.
#' @param order Logical. Defaults to \code{TRUE}. Should the groups be ordered 
#' according to their mean?
#' @return A list of data split by the grouping factor.
parse_form <- function(formula, data, order = TRUE) {
	vars <- all.vars(formula)

	if(length(vars) == 2) {
		splt <- split(data[[ vars[1] ]], data[[ vars[2] ]]	)
	}
	if(length(vars) == 3) {
		splt <- split(data, data[[ vars[2] ]]	)
		splt <- lapply(splt, function(x) split(x, x[[ vars[3] ]]))
		splt <- lapply(splt, function(x) lapply(x, "[[", vars[1]))
	}
	if(length(vars) > 3) {
		stop("Only two grouping factors supported. Make sure your data are tidy.")
	}

	if(order == TRUE) {
		if(length(vars) == 2) {
			means <- sapply(splt, mean, na.rm = TRUE)
			splt <- splt[order(means, decreasing = TRUE)]
		}
		if(length(vars) == 3) {
			ordered <- lapply(splt, function(x) {
				order(sapply(x, mean, na.rm = TRUE), decreasing = TRUE)
			})
			splt <- Map(function(x, y) x[y], splt, ordered)
		}
	}
splt
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

	diff_1 <- mapply(fun, split(combos_1, 1:nrow(combos_1)))
	diff_2 <- mapply(fun, split(combos_2, 1:nrow(combos_2)))

	set1 <- sapply(split(combos_1, 1:nrow(combos_1)), 
				paste0, collapse = "-")
	set2 <- sapply(split(combos_2, 1:nrow(combos_2)), 
				paste0, collapse = "-")

	vec <- c(mapply(fun, split(combos_1, 1:nrow(combos_1))),
			 mapply(fun, split(combos_2, 1:nrow(combos_2))))
	names(vec) <- c(set1, set2)
vec
}

tidy_out <- function(levs, fun) {
	combos_1 <- t(utils::combn(levs, 2))
	combos_2 <- t(utils::combn(rev(levs), 2))

	diff_1 <- mapply(fun, split(combos_1, 1:nrow(combos_1)))
	diff_2 <- mapply(fun, split(combos_2, 1:nrow(combos_2)))

	if(is.null(dim(diff_1))) {
		td <- as.data.frame(rbind(combos_1, combos_2))
		names(td) <- c("ref_group", "foc_group")
		td$estimate <- c(diff_1, diff_2)
	}
	else{
		g1 <- as.data.frame(split(rep(combos_1, nrow(diff_1)), rep(1:2, each = nrow(combos_1))))
		g2 <- as.data.frame(split(rep(combos_2, nrow(diff_2)), rep(1:2, each = nrow(combos_2))))
		
		td <- as.data.frame(rbind(g1, g2))
		names(td) <- c("ref_group", "foc_group")
		
		if(is.null(rownames(diff_1))) warning("Cut score not specified as row names")
		
		td$cut <- rep(rep(rownames(diff_1), each = nrow(combos_1)), 2)

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
#' @return A list with one function per group (level in the grouping factor).
#' @export

cdfs <- function(formula, data) {
	splt <- parse_form(formula, data)
	
	if(length(all.vars(formula)) == 2) {
		return(lapply(splt, stats::ecdf))
	}
	else {
		lapply(splt, function(x) lapply(x, stats::ecdf))
	}		
}

#' Compute probabilities from the empirical CDFs of a grouping variable for
#' each group.
#' 
#' This formula returns the paired probabilities for any 
#' @param formula A formula of the type \code{out ~ group} where \code{out} is
#' the outcome variable and \code{group} is the grouping variable. Note this
#' variable can include any arbitrary number of groups.
#' @param data The data frame that the data in the formula come from.
#' @return A matrix of probabilities with separate columns for each group and
#' \code{rownames} corresponding to the value the paired probabilities are 
#' calculated from.
#' @examples
#' 
#' free <- rnorm(300, 80, 15)
#' reduced <- rnorm(100, 90, 12)
#' pay <- rnorm(500, 100, 10)
#' d <- data.frame(score = c(free, reduced, pay), 
#' 				frl = c(rep("free", 300), 
#' 						rep("reduced", 100), 
#' 						rep("pay", 500)))
#' 
#' probs(score ~ frl, d)
#' @export

probs <- function(formula, data) {
	ecdfs <- cdfs(formula, data)
	out <- data[[ all.vars(formula)[1] ]]
	
	prob_vals <- seq(min(out, na.rm = TRUE) - sd(out, na.rm = TRUE), 
					  max(out, na.rm = TRUE) + sd(out, na.rm = TRUE),
					  .1)

	if(length(all.vars(formula)) == 2) {
		ps <- sapply(ecdfs, function(x) x(prob_vals) )
		colnames(ps) <- names(ecdfs)
		rownames(ps) <- prob_vals
	}
	else {
		ps <- lapply(ecdfs, function(x) sapply(x, function(x) x(prob_vals) ))
		ps <- lapply(ps, function(x) {
				rownames(x) <- prob_vals
			x
		})
	}
ps
}

#' Color hues
#'
#' Emulates ggplot's default colors. Evenly spaced hues around the color wheel.
#' 
#' @param n The number of colors to be produced
#' @export

col_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  grDevices::hcl(h = hues, c = 100, l = 65)[1:n]
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
#' @param ... Additional arguments passed to \link[graphics]{lines}.
#' 

create_legend <- function(n, leg_labels, ...) {
	par(mar = c(5.1, 0, 4.1, 0))
	if(n < 8) {
		plot(seq(0, 1, length = 12), 
			 1:12,
			type = "n",
			bty = "n", 
			xaxt = "n",
			xlab = "", 
			yaxt = "n",
			ylab = "")
	}
	else {
		plot(seq(0, 1, length = n * 1.5), 
			 seq(1, n * 1.5, 
			 	length = n * 1.5),
			type = "n",
			bty = "n", 
			xaxt = "n",
			xlab = "", 
			yaxt = "n",
			ylab = "")
	}

	axes <- cbind(c(0, 1), rep(1:n, each = 2))	
	
	Map(lines, 
		split(axes[ ,1], axes[ ,2]), 
		split(axes[ ,2], axes[ ,2]),
		...)
	axis(2, 
		lwd = 0, 
		at = 1:n, 
		labels = leg_labels, 
		las = 2)
}