#' Compute empirical CDFs by grouping variable and return the probabilities
#' for each group.
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


probs <- function(formula, data, ...) {
	form_args <- all.vars(formula)
	out <- data[[ form_args[1] ]]
	group <- data[[ form_args[2] ]]	

	splt <- split(out, group)

	means <- sapply(splt, mean, na.rm = TRUE)

	splt <- splt[order(means, decreasing = TRUE)]

	ecdfs <- lapply(splt, ecdf)
	ps <- sapply(ecdfs, function(x) {
			x(seq(min(out, na.rm = TRUE) - sd(out, na.rm = TRUE), 
				  max(out, na.rm = TRUE) + sd(out, na.rm = TRUE),
				  .1))
		})
	colnames(ps) <- names(splt)
	rownames(ps) <- seq(min(out, na.rm = TRUE) - sd(out, na.rm = TRUE), 
				  max(out, na.rm = TRUE) + sd(out, na.rm = TRUE),
				  .1)
ps
}

#' Calculate the area under the curve
#' 
#' This function is used within \code{\link{pp_plot}} to calculate the area 
#' under the \code{pp} curve. 
#' @param ps The paired probabilities for two groups, calculated from 
#' \code{\link{probs}}. Note that the probabilities must only be from two
#' groups.
#' @examples
#' free_reduced <- rnorm(800, 80, 20)
#' pay <- rnorm(500, 100, 10)
#' d <- data.frame(score = c(free_reduced, pay), 
#' 				frl = c(rep("free_reduced", 800),  
#' 						rep("pay", 500)))
#' 
#' auc(probs(score ~ frl, d))
#' @export

auc <- function(ps) sfsmisc::integrate.xy(ps[ ,1], ps[ ,2])


#' Calculate the V effect size statistic
#' 
#' This function calculates the effect size V, as discussed by Ho.
#' @param ps The paired probabilities for two groups, calculated from 
#' \code{\link{probs}}. Note that the probabilities must only be from two
#' groups.
#' @examples
#' free_reduced <- rnorm(800, 80, 20)
#' pay <- rnorm(500, 100, 10)
#' d <- data.frame(score = c(free_reduced, pay), 
#' 				frl = c(rep("free_reduced", 800),  
#' 						rep("pay", 500)))
#' 
#' v(probs(score ~ frl, d))
#' @export

v <- function(ps) sqrt(2*qnorm(sfsmisc::integrate.xy(ps[ ,1], ps[ ,2])))


#' Color hues
#'
#' Emulates ggplot's default colors. Evenly spaced hues around the color wheel.
#' 
#' @param n The number of colors to be produced

col_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, c = 100, l = 65)[1:n]
}
