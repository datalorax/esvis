
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
#' @param ref_group Optional. If the name of the reference group is provided
#' (must be character and match the grouping level exactly), only the
#' estimates corresponding to the given reference group will be returned.
#' @param tidy Logical. Should the data be returned in a tidy data frame? (see
#' \href{http://journals.sagepub.com/doi/abs/10.3102/1076998611411918}{Wickham, 2014}). If false, effect sizes returned
#'  as a vector.
#' @return By default the Cohen's \emph{d} for all possible pairings of
#'  the grouping factor are returned as a tidy data frame.
#' @import stats
#' @export
coh_d <- function(formula, data, ref_group = NULL, tidy = TRUE) {
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

	td <- tidy_out(names(splt), es_d)
	if(!is.null(ref_group)) {
		td <- td[td$ref_group == ref_group, ]
	}

	if(tidy == FALSE) {
		vec <- td$estimate
		names(vec) <- paste(td$ref_group, td$foc_group, sep = "-")
		return(vec)
	}
td
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
#' @param ref_group Optional. If the name of the reference group is provided
#' (must be character and match the grouping level exactly), only the
#' estimates corresponding to the given reference group will be returned.
#' @param tidy Logical. Should the data be returned in a tidy data frame? (see
#' \href{http://journals.sagepub.com/doi/abs/10.3102/1076998611411918}{Wickham, 2014}). If false, effect sizes returned
#'  as a vector.
#' @return By default the Hedges' \emph{d} for all possible pairings of
#'  the grouping factor are returned as a tidy data frame.
#' @import stats
#' @export

hedg_g <- function(formula, data, ref_group = NULL, tidy = TRUE) {
	splt <- parse_form(formula, data)

	ns <- sapply(splt, length)
	ns <- outer(ns, ns, "+")
	diag(ns) <- 0
	v <- as.vector(ns)

	td <- coh_d(formula, data)
	
	td$estimate <- td$estimate * (1 - ( 3 /( (4*v[v!=0]) - 9) ) )
	if(!is.null(ref_group)) {
		td <- td[td$ref_group == ref_group, ]
	}

	if(tidy == FALSE) {
		vec <- td$estimate
		names(vec) <- paste(td$ref_group, td$foc_group, sep = "-")
		return(vec)
	}
td
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
		return(sqrt(2)*qnorm(sfsmisc::integrate.xy(ps[ ,1], ps[ ,2])))
	}

	v_fun <- function(x, y) {
		sqrt(2)*qnorm(sfsmisc::integrate.xy(ps[ ,x], ps[ ,y]))
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