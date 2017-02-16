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

#' Compute Cohen's \emph{d}
#' 
#' This function calculates effect sizes in terms of Cohen's \emph{d}, also
#' called the uncorrected effect size. See \code{\link{hedg_g}} for the sample
#' size corrected version. Also see 
#' \href{https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3840331/}{Lakens (2013)}
#' for a discussion on different types of effect sizes and their
#' interpretation. Note that missing data are removed from the calculations of 
#' the means and standard deviations.
#' @param formula A formula of the type \code{out ~ group} where \code{out} is
#' the outcome variable and \code{group} is the grouping variable. Note this
#' variable can include any arbitrary number of groups.
#' @param data The data frame that the data in the formula come from.
#' @param matrix Logical. If only two groups are being compared, should the
#' full matrix of all possible comparisons be returned? If \code{FALSE} only
#' the positive effect size is returned.
#' @return By default the Cohen's \emph{d} for all possible pairings of
#'  the grouping factor are returned as a matrix, with the reference group 
#'  reported by rows and the focal group reported by columns.
#' @import stats
#' @export
coh_d <- function(formula, data, matrix = TRUE) {
	splt <- parse_form(formula, data)

	means <- sapply(splt, mean, na.rm = TRUE)
	vars <- sapply(splt, var, na.rm = TRUE)
	ns <- sapply(splt, length)

	# vec is a vector to subset means/vars/ns for the appropriate comparison
	es_d <- function(vec) {
		(means[ vec[1] ] - means[ vec[2] ]) / 
		sqrt((((ns[1] - 1)*vars[1]) + ((ns[2] - 1)*vars[2])) / 
			(sum(ns[vec]) - 2))
	}

	mat <- create_mat(names(splt), es_d)
	
	if(length(splt) == 2 & matrix == FALSE) {
		return(mat[2, 1])
	}
	if(length(splt) > 2 & matrix == FALSE) {
		warning("Single value cannot be returned when the number of groups > 2. Returning entire matrix. Please subset the matrix manually to select the specific value of interest.")
	}
mat
}


#' Compute Hedges' \emph{g}
#' This function calculates effect sizes in terms of Hedges' \emph{g}, also
#' called the corrected (for sample size) effect size. See
#' \code{\link{coh_d}} for the uncorrected version. Also see 
#' \href{https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3840331/}{Lakens (2013)}
#' for a discussion on different types of effect sizes and their
#' interpretation. Note that missing data are removed from the calculations of 
#' the means and standard deviations.
#' @param formula A formula of the type \code{out ~ group} where \code{out} is
#' the outcome variable and \code{group} is the grouping variable. Note this
#' variable can include any arbitrary number of groups.
#' @param data The data frame that the data in the formula come from.
#' @param matrix Logical. If only two groups are being compared, should the
#' full matrix of all possible comparisons be returned? If \code{FALSE} only
#' the positive effect size is returned.
#' @return By default the Hedges' \emph{d} for all possible pairings of
#'  the grouping factor are returned as a matrix, with the reference group 
#'  reported by rows and the focal group reported by columns.
#' @import stats
#' @export

hedg_g <- function(formula, data, matrix = TRUE) {
	splt <- parse_form(formula, data)

	ns <- sapply(splt, length)
	ns <- outer(ns, ns, "+")
	
	ds <- coh_d(formula, data)

	mat <- ds * (1 - ( 3 /( (4*ns) - 9) ) )
	if(length(splt) == 2 & matrix == FALSE) {
		return(mat[1, 2])
	}
	if(length(splt) > 2 & matrix == FALSE) {
		warning("Single value cannot be returned when the number of groups > 2. Returning entire matrix. Please subset the matrix manually to select the specific value of interest.")
	}
mat
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

#' Compute the proportion above a specific cut location
#' 
#' This rather simple function  calls \link{cdfs}, to compute the
#' empirical cumulative distribution function for all levels of the grouping 
#' factor, and then calculates the proportion of the sample above any generic
#' point on the scale for all groups. Alternatively only specific proportions
#' can be returned.
#' @param formula A formula of the type \code{out ~ group} where \code{out} is
#' the outcome variable and \code{group} is the grouping variable. Note this
#' variable can include any arbitrary number of groups.
#' @param data The data frame that the data in the formula come from.
#' @param cut The point at the scale from which the proportion above should
#' be calculated from.
#' @param groups The groups to return the proportion above the cut. Defaults
#'  to \code{"all"}. Argument takes a character or character vector as its
#'  argument corresponding to the levels of the grouping factor.
#' @param diff Logical, defaults to \code{TRUE}. Should the difference between
#' the groups be returned? If \code{FALSE} the raw proportion above
#' the cut is returned for each group.
#' @param matrix Logical, defaults to \code{TRUE}. Should the results be
#' returned as a matrix? Only relevant when \code{diff == TRUE}.
#' @return Matrix (or vector) of the proportion above the cutoff.
#' @export 

pac <- function(formula, data, cut, groups = "all", diff = TRUE, 
			matrix = TRUE) {
	if(is.numeric(groups)) {
		groups <- as.character(groups)
		warning("Numeric input for `groups` coerced to character.")
	}

	ecdfs <- cdfs(formula, data)
	pacs <- sapply(ecdfs, function(f) 1 - f(cut))
	if(diff == FALSE) {
		if(length(groups) > 1|(length(groups) == 1 & groups != "all")) {
			return(pacs[groups])
		}
		else {
			return(pacs)
		}

	}
	
	if(diff == TRUE) {
		diff_pac <- function(vec) pacs[[ vec[1] ]] - pacs[[ vec[2] ]]
		if(matrix == TRUE) {
			if(length(groups) > 1) {
				pacs <- pacs[groups]
				mat <- create_mat(names(pacs), diff_pac)
			}
			if(groups == "all") {
				mat <- create_mat(names(pacs), diff_pac)
			}
		}

		if(matrix == FALSE) {
			if(length(groups) > 1) {
				pacs <- pacs[groups]
				vec <- create_mat(names(pacs), diff_pac, vec = TRUE)
			return(vec)
			}
			if(groups == "all") {
				vec <- create_mat(names(pacs), diff_pac, vec = TRUE)
			return(vec)
			}
		}
	}
mat
}

#' Transformed proportion above the cut
#' 
#' This function transforms calls to \link{pac} into standard deviation units.
#' Function assumes that each distribution is distributed normally with 
#' common variances. See 
#' \href{http://journals.sagepub.com/doi/abs/10.3102/1076998611411918}{Ho &
#'  Reardon, 2012}
#' @param formula A formula of the type \code{out ~ group} where \code{out} is
#' the outcome variable and \code{group} is the grouping variable. Note this
#' variable can include any arbitrary number of groups.
#' @param data The data frame that the data in the formula come from.
#' @param cut The point at the scale from which the proportion above should
#' be calculated from.
#' @param groups The groups to return the proportion above the cut. Defaults
#'  to \code{"all"}. Argument takes a character or character vector as its
#'  argument corresponding to the levels of the grouping factor.
#' @param diff Logical, defaults to \code{TRUE}. Should the difference between
#' the groups be returned? If \code{FALSE} the raw transformed proportion above
#' the cut is returned for each group, in standard deviation units.
#' @param matrix Logical, defaults to \code{TRUE}. Should the results be
#' returned as a matrix? Only relevant when \code{diff == TRUE}.
#' @return Matrix (or vector) of the transformed proportion above the cutoff.
#' @export 

tpac <- function(formula, data, cut, groups = "all", diff = TRUE, 
			matrix = TRUE) {
	pacs <- pac(formula, data, cut, groups, diff = FALSE, matrix = FALSE)
	tpacs <- qnorm(pacs)
	if(diff == FALSE) {
		return(tpacs)
	}
	if(length(tpacs) == 1 & diff == TRUE) {
		warning("Only one group specified with `dif = TRUE`. Call to `diff` will be ignored")
		return(tpacs)
	}

	
	if(diff == TRUE) {
		diff_tpac <- function(vec) tpacs[[ vec[1] ]] - tpacs[[ vec[2] ]]

		if(matrix == FALSE) {
			vec <- create_mat(names(tpacs), diff_tpac, vec = TRUE)
			if(any(vec == Inf|vec == -Inf)) {
				warning("100% or 0% of the sample (for one or more groups)scored above/below this cut point. Cannot transform to normal scale.")
			}
		return(vec)
		}
		
		mat <- create_mat(names(tpacs), diff_tpac)
		if(any(mat == Inf)) {
			warning("100% or 0% of the sample (for one or more groups) scored above/below this cut point. Cannot transform to normal scale.")
		}
	}
mat
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

#' Calculate the area under the curve
#' 
#' This function is used within \code{\link{pp_plot}} to calculate the area 
#' under the \code{pp} curve. The area under the curve is also a useful 
#' effect-size like statistic, representing the probability that a randomly 
#' selected individual from distribution a will have a higher value than a 
#' randomly selected individual from distribution b.
#' 
#' @param formula A formula of the type \code{out ~ group} where \code{out} is
#' the outcome variable and \code{group} is the grouping variable. Note this
#' variable can include any arbitrary number of groups.
#' @param data The data frame that the data in the formula come from.
#' @param matrix Logical. If only two groups are being compared, should the
#' full matrix of all possible comparisons be returned? If \code{FALSE} the
#' greater of the two aucs is returned. 
#' @return By default the area under the curve for all possible pairings of
#'  the grouping factor are returned as a matrix, with the reference group 
#'  (x-axis of the pp plot) reported by rows and the focal group 
#'  (y-axis) reported by columns.
#' @examples
#' free_reduced <- rnorm(800, 80, 20)
#' pay <- rnorm(500, 100, 10)
#' d <- data.frame(score = c(free_reduced, pay), 
#' 				frl = c(rep("free_reduced", 800),  
#' 						rep("pay", 500)))
#' 
#' auc(score ~ frl, d)
#' @export

auc <- function(formula, data, matrix = TRUE) {
	ps <- probs(formula, data)
	if(ncol(ps) == 2 & matrix == FALSE) { 
		return(sfsmisc::integrate.xy(ps[ ,1], ps[ ,2]))
	}

	auc_fun <- function(x, y) sfsmisc::integrate.xy(ps[ ,x], ps[ ,y])	
	aucs <- mapply(auc_fun, 
				   rep(1:ncol(ps), ncol(ps)),
				   rep(1:ncol(ps), each = ncol(ps)))

	t(matrix(aucs, 
			ncol = ncol(ps),
			byrow = TRUE,
			dimnames = list(colnames(ps), colnames(ps))))
} 

#' Calculate the V effect size statistic
#' 
#' This function calculates the effect size V, as discussed by Ho, 2009. The V
#' statistic is a transformation of \code{\link{auc}}, interpreted as the 
#' average difference between the distributions in standard deviation units.
#' @param formula A formula of the type \code{out ~ group} where \code{out} is
#' the outcome variable and \code{group} is the grouping variable. Note this
#' variable can include any arbitrary number of groups.
#' @param data The data frame that the data in the formula come from.
#' @return By default the V statistic for all possible pairings of
#'  the grouping factor are returned as a matrix, with the reference group 
#'  (x-axis of the pp plot) reported by rows and the focal group 
#'  (y-axis) reported by columns. Note that V cannot be negative (given that
#'  the square root of a negative number is imaginary) and half the values
#'  reported are missing (typically the lower triangle of the matrix). When
#'  only two groups are included in the grouping factor, a single value is
#'  returned.
#' @import stats
#' @examples
#' free_reduced <- rnorm(800, 80, 20)
#' pay <- rnorm(500, 100, 10)
#' d <- data.frame(score = c(free_reduced, pay), 
#' 				frl = c(rep("free_reduced", 800),  
#' 						rep("pay", 500)))
#' 
#' v(score ~ frl, d)
#' @export

v <- function(formula, data) {
	ps <- probs(formula, data)
	if(ncol(ps) == 2 ) { 
		return(sqrt(2*qnorm(sfsmisc::integrate.xy(ps[ ,1], ps[ ,2]))))
	}

	v_fun <- function(x, y) {
		sqrt(2*qnorm(sfsmisc::integrate.xy(ps[ ,x], ps[ ,y])))
	}
	suppressWarnings(
		vs <- mapply(v_fun, 
					   rep(1:ncol(ps), ncol(ps)),
					   rep(1:ncol(ps), each = ncol(ps)))
	)
	t(matrix(vs, 
			ncol = ncol(ps),
			byrow = TRUE,
			dimnames = list(colnames(ps), colnames(ps))))
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
