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
		means <- sapply(splt, mean, na.rm = TRUE)
		splt <- splt[order(means, decreasing = TRUE)]
	}
splt
}

#' Create Matrix of all possible combinations
#' 
#' @param levs The levels of the grouping factor from which to create the
#' matrix
#' @param fun The function to apply.
#' @param diagonal What elements should go on the diagonal of the matrix?
#' Defaults to 0. 
#' @param vec Logical. Should a vector rather than a matrix be returned? 
#' Defaults to FALSE.
#' @return Matrix of values according to the function supplied.

create_mat <- function(levs, fun, diagonal = 0, vec = FALSE) {
	combos_1 <- t(utils::combn(levs, 2))
	combos_2 <- t(utils::combn(rev(levs), 2))

	diff_1 <- mapply(fun, split(combos_1, 1:nrow(combos_1)))
	diff_2 <- mapply(fun, split(combos_2, 1:nrow(combos_2)))

	mat <- matrix(rep(NA, length(levs)^2), ncol = length(levs))
	diag(mat) <- diagonal
	mat[lower.tri(mat)] <- diff_1
	mat[upper.tri(mat)] <- rev(diff_2)
	rownames(mat) <- levs
	colnames(mat) <- levs
	if(vec == TRUE) {
		set1 <- sapply(split(combos_1, 1:nrow(combos_1)), 
					paste0, collapse = "-")
		set2 <- sapply(split(combos_2, 1:nrow(combos_2)), 
					paste0, collapse = "-")

		vec <- c(mapply(fun, split(combos_1, 1:nrow(combos_1))),
				 mapply(fun, split(combos_2, 1:nrow(combos_2))))
		names(vec) <- c(set1, set2)
	return(vec)
	}
t(mat)
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
	
	ps <- sapply(ecdfs, function(x) {
			x(seq(min(out, na.rm = TRUE) - sd(out, na.rm = TRUE), 
				  max(out, na.rm = TRUE) + sd(out, na.rm = TRUE),
				  .1))
		})
	colnames(ps) <- names(ecdfs)
	rownames(ps) <- seq(min(out, na.rm = TRUE) - sd(out, na.rm = TRUE), 
				  max(out, na.rm = TRUE) + sd(out, na.rm = TRUE),
				  .1)
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
