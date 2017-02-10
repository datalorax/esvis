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

	combos_1 <- t(utils::combn(1:length(splt), 2))
	combos_2 <- t(utils::combn(length(splt):1, 2))

	effects_1 <- mapply(es_d, split(combos_1, 1:nrow(combos_1)))
	effects_2 <- mapply(es_d, split(combos_2, 1:nrow(combos_2)))

	mat <- matrix(rep(NA, length(splt)^2), ncol = length(splt))
	diag(mat) <- 0
	mat[lower.tri(mat)] <- effects_1
	mat[upper.tri(mat)] <- rev(effects_2)
	rownames(mat) <- names(splt)
	colnames(mat) <- names(splt)
	
	if(length(splt) == 2 & matrix == FALSE) {
		return(mat[2, 1])
	}
	if(length(splt) > 2 & matrix == FALSE) {
		warning("Single value cannot be returned when the number of groups > 2. Returning entire matrix. Please subset the matrix manually to select the specific value of interest.")
	}
t(mat)
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
