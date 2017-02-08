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

cdfs <- function(formula, data, order = TRUE) {
	out <- data[[ all.vars(formula)[1] ]]
	group <- data[[ all.vars(formula)[2] ]]	

	splt <- split(out, group)
	if(order == TRUE) {
		means <- sapply(splt, mean, na.rm = TRUE)
		splt <- splt[order(means, decreasing = TRUE)]
	}
lapply(splt, ecdf)
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
#'  (x-axis of the pp plot) reported by columns and the focal group 
#'  (y-axis) reported by rows.
#' @examples
#' free_reduced <- rnorm(800, 80, 20)
#' pay <- rnorm(500, 100, 10)
#' d <- data.frame(score = c(free_reduced, pay), 
#' 				frl = c(rep("free_reduced", 800),  
#' 						rep("pay", 500)))
#' 
#' auc(probs(score ~ frl, d))
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

	matrix(aucs, 
		ncol = ncol(ps),
		byrow = TRUE,
		dimnames = list(colnames(ps), colnames(ps)))
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
#'  (x-axis of the pp plot) reported by columns and the focal group 
#'  (y-axis) reported by rows. Note that V cannot be negative (given that the
#'  square root of a negative number is imaginary) and half the values reported
#'  are missing (typically the upper triangle of the matrix). When only two 
#'  groups are included in the grouping factor, a single value is returned.
#' @examples
#' free_reduced <- rnorm(800, 80, 20)
#' pay <- rnorm(500, 100, 10)
#' d <- data.frame(score = c(free_reduced, pay), 
#' 				frl = c(rep("free_reduced", 800),  
#' 						rep("pay", 500)))
#' 
#' v(probs(score ~ frl, d))
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
	matrix(vs, 
		ncol = ncol(ps),
		byrow = TRUE,
		dimnames = list(colnames(ps), colnames(ps)))
} 

#' Color hues
#'
#' Emulates ggplot's default colors. Evenly spaced hues around the color wheel.
#' 
#' @param n The number of colors to be produced
#' @export

col_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, c = 100, l = 65)[1:n]
}
