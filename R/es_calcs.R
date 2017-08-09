
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
#' @importFrom stats var qnorm
#' @export
coh_d <- function(formula, data, ref_group = NULL, tidy = TRUE) {
	splt <- parse_form(formula, data)

	means <- vapply(splt, mean, na.rm = TRUE, numeric(1))
	vars <- vapply(splt, var, na.rm = TRUE, numeric(1))
	ns <- vapply(splt, length, numeric(1))

	# vec is a vector to subset means/vars/ns for the appropriate comparison
	es_d <- function(v) {
		(means[ v[1] ] - means[ v[2] ]) / 
		sqrt((((ns[1] - 1)*vars[1]) + ((ns[2] - 1)*vars[2])) / 
			(sum(ns[v]) - 2))
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
#' @export

hedg_g <- function(formula, data, ref_group = NULL, tidy = TRUE) {
	splt <- parse_form(formula, data)

	ns <- vapply(splt, length, numeric(1))
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
#' @param ref_group Optional. If the name of the reference group is provided
#' (must be character and match the grouping level exactly), only the
#' estimates corresponding to the given reference group will be returned.
#' @param diff Logical, defaults to \code{TRUE}. Should the difference between
#' the groups be returned? If \code{FALSE} the raw proportion above
#' the cut is returned for each group.
#' @param tidy Logical. Should the data be returned in a tidy data frame? (see
#' \href{http://journals.sagepub.com/doi/abs/10.3102/1076998611411918}{Wickham, 2014}). If false, effect sizes returned
#'  as a vector.
#' @return Tidy data frame (or vector) of the proportion above the cutoff for 
#' each (or selected) groups.
#' @export 

pac <- function(formula, data, cut, ref_group = NULL, diff = TRUE, 
			tidy = TRUE) {

	ecdfs <- cdfs(formula, data)
	pacs <- vapply(ecdfs, function(f) 1 - f(cut), numeric(length(cut)))

	if(diff == FALSE) {
		if(is.null(dim(pacs))) {
			if(tidy == TRUE) {
				td <- data.frame(group = names(pacs), 
								 estimate = pacs)
				rownames(td) <- NULL
			}
			if(tidy == FALSE) {
				if(!is.null(ref_group)) {
					pacs <- pacs[as.character(ref_group)]
				}
				return(pacs)
			}
		}
		else {
			if(tidy == TRUE) {
				td <- data.frame(group = rep(colnames(pacs), 
												each = nrow(pacs)),
								cut = rep(cut, ncol(pacs)))
				dim(pacs) <- NULL
				td$estimate <- pacs
			}
			if(tidy == FALSE) {
				rownames(pacs) <- cut
			return(pacs)
			}	
		}
	}
	if(diff == TRUE) {
		if(is.null(dim(pacs))) {
			diff_pac <- function(v) pacs[[ v[1] ]] - pacs[[ v[2] ]]	
			if(tidy == TRUE) {
				td <- tidy_out(names(pacs), diff_pac)
			}

			if(tidy == FALSE) {
				vec <- create_vec(names(pacs), diff_pac)
				if(!is.null(ref_group)) {
					vec <- vec[grep(paste0("^", ref_group), names(vec))]
				}
				return(vec)
			}
		}
		else {
			diff_pac <- function(v) pacs[ ,v[1]] - pacs[ ,v[2]]
			rownames(pacs) <- cut
			td <- tidy_out(colnames(pacs), diff_pac)
			
			if(tidy == FALSE) {
				splt_td <- split(td$estimate, td$cut)
				
				nms <- subset(td, cut == cut[1], select = c(1, 2))
				nms <- apply(nms, 1, paste, collapse = "-")

				splt_td <- lapply(splt_td, function(x) {
					names(x) <- nms
					return(x)
				})
			return(splt_td)
			}			
		}
	}
	if(!is.null(ref_group)) {
		td <- td[td$ref_group == ref_group, ]
	}
td
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
#' @param ref_group Optional. If the name of the reference group is provided
#' (must be character and match the grouping level exactly), only the
#' estimates corresponding to the given reference group will be returned.
#' @param diff Logical, defaults to \code{TRUE}. Should the difference between
#' the groups be returned? If \code{FALSE} the raw proportion above
#' the cut is returned for each group.
#' @param tidy Logical. Should the data be returned in a tidy data frame? (see
#' \href{http://journals.sagepub.com/doi/abs/10.3102/1076998611411918}{Wickham, 2014}). If false, effect sizes returned
#'  as a vector.
#' @return A tidy data frame (or vector) of the transformed proportion above
#' the cutoff. Optionally (and by default) all pairwise comparisons are 
#' calculated and returned.
#' @importFrom stats qnorm
#' @export 

tpac <- function(formula, data, cut, ref_group = NULL, diff = TRUE, 
			tidy = TRUE) {
	pacs <- pac(formula, data, cut, diff = FALSE, tidy = FALSE)
	tpacs <- qnorm(pacs)
	
	if(any(tpacs == Inf|tpacs == -Inf)) {
				warning(
					paste("100% or 0% of the sample (for one or more groups)",
						"scored above/below this cut point. Cannot transform",
						"to normal scale.")
					)
	}
	
	if(diff == FALSE) {
		if(tidy) {
			if(is.null(dim(tpacs))) {
				td <- data.frame(group = names(tpacs), 
							 estimate = tpacs)
				rownames(td) <- NULL
			}
			else {
				td <- data.frame(group = rep(colnames(tpacs), 
												each = nrow(tpacs)),
								cut = rep(cut, ncol(tpacs)))
				dim(tpacs) <- NULL
				td$estimate <- tpacs
			}
		}
		if(tidy == FALSE) {
			return(tpacs)	
		}
	}
	
	if(length(tpacs) == 1 & diff == TRUE) {
		warning(
			paste("Only one group specified with `diff = TRUE`.", 
				  "Call to `diff` will be ignored")
			)
		return(tpacs)
	}

	if(diff == TRUE) {
		if(is.null(dim(tpacs))) {
			diff_tpac <- function(v) tpacs[[ v[1] ]] - tpacs[[ v[2] ]]

			if(tidy == FALSE) {
				vec <- create_vec(names(tpacs), diff_tpac)
				
				if(!is.null(ref_group)) {
					vec <- vec[grep(paste0("^", ref_group), names(vec))]
				}
			return(vec)
			}
			if(tidy == TRUE) {
				td <- tidy_out(names(tpacs), diff_tpac)
				if(!is.null(ref_group)) {
					td <- td[td$ref_group == ref_group, ]
				}
			}
		}
		else {
			diffs_1 <- Map(function(i) 
								apply(combn(tpacs[i, ], 2), 2, diff), 
					   	   seq_len(nrow(tpacs)))

			diffs_2 <- Map(function(i) 
								apply(combn(rev(tpacs[i, ]), 2), 2, diff), 
							seq_len(nrow(tpacs)))

			vec <- c(unlist(diffs_1), unlist(diffs_2)) * -1

			comparison <- c(
				rep(
					apply(combn(colnames(tpacs), 2), 2, 
						paste, 
						collapse = "-"),
					ncol(tpacs)),
				rep(
					apply(combn(rev(colnames(tpacs)), 2), 2, 
						paste, 
						collapse = "-"),
					ncol(tpacs))
			)
			if(tidy == FALSE) {
				names(vec) <- paste(comparison, 
							  	rep(rownames(tpacs), each = ncol(tpacs)),
							  	sep = "_")
				if(!is.null(ref_group)) {
					vec <- vec[grep(paste0("^", ref_group), names(vec))]
				}
				return(vec)
			}
			else {
				ref <- vapply(strsplit(comparison, "-"), 
						  function(x) x[[1]], 
						  character(1))
				foc <- vapply(strsplit(comparison, "-"), 
						  function(x) x[[2]], 
						  character(1))

				td <- data.frame(ref_group = ref,
						   foc_group = foc,
						   cut = rep(rownames(tpacs), each = ncol(tpacs)),
						   estimate = vec)
				if(!is.null(ref_group)) {
					td <- td[td$ref_group == ref_group, ]
				}
			}
		}
	}
td
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
#' @param ref_group Optional. If the name of the reference group is provided
#' (must be character and match the grouping level exactly), only the
#' estimates corresponding to the given reference group will be returned.
#' @param tidy Logical. Should the data be returned in a tidy data frame? (see
#' \href{http://journals.sagepub.com/doi/abs/10.3102/1076998611411918}{Wickham, 2014}). If false, effect sizes returned
#'  as a vector.
#' @return By default the area under the curve for all possible pairings of
#' the grouping factor are returned as a tidy data frame. Alternatively, a 
#' vector can be returned, and/or only the auc corresponding to a specific
#' reference group can be returned.
#' @examples
#' free_reduced <- rnorm(800, 80, 20)
#' pay <- rnorm(500, 100, 10)
#' d <- data.frame(score = c(free_reduced, pay), 
#' 				frl = c(rep("free_reduced", 800),  
#' 						rep("pay", 500)))
#' 
#' auc(score ~ frl, d)
#' @export

auc <- function(formula, data, ref_group = NULL, tidy = TRUE) {
	ps <- probs(formula, data)
	
	auc_fun <- function(v) sfsmisc::integrate.xy(ps[ ,v[1]], ps[ ,v[2]])	
	
	if(tidy == FALSE) {
		vec <- create_vec(colnames(ps), auc_fun)
		if(!is.null(ref_group)) {
			vec <- vec[grep(paste0("^", ref_group), names(vec))]
		}
	return(vec)
	}
	
	if(tidy == TRUE) {
		td <- tidy_out(colnames(ps), auc_fun)
		if(!is.null(ref_group)) {
			td <- td[td$ref_group == ref_group, ]
		}	
	}
td
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
#' @param ref_group Optional. If the name of the reference group is provided
#' (must be character and match the grouping level exactly), only the
#' estimates corresponding to the given reference group will be returned.
#' @param tidy Logical. Should the data be returned in a tidy data frame? (see
#' \href{http://journals.sagepub.com/doi/abs/10.3102/1076998611411918}{Wickham, 2014}). If false, effect sizes returned
#'  as a vector.
#' @return By default the V statistic for all possible pairings of
#'  the grouping factor are returned as a tidy data frame. Alternatively, a 
#' vector can be returned, and/or only the V corresponding to a specific
#' reference group can be returned.
#' @importFrom stats qnorm
#' @examples
#' free_reduced <- rnorm(800, 80, 20)
#' pay <- rnorm(500, 100, 10)
#' d <- data.frame(score = c(free_reduced, pay), 
#' 				frl = c(rep("free_reduced", 800),  
#' 						rep("pay", 500)))
#' 
#' v(score ~ frl, d)
#' @export

v <- function(formula, data, ref_group = NULL, tidy = TRUE) {
	ps <- probs(formula, data)

	v_fun <- function(v) {
		sqrt(2)*qnorm(sfsmisc::integrate.xy(ps[ ,v[1]], ps[ ,v[2]]))
	}
	
	if(tidy == FALSE) {
		vec <- create_vec(colnames(ps), v_fun)
		if(!is.null(ref_group)) {
			vec <- vec[grep(paste0("^", ref_group), names(vec))]
		}
	return(vec)
	}
	
	if(tidy == TRUE) {
		td <- tidy_out(colnames(ps), v_fun)
		if(!is.null(ref_group)) {
			td <- td[td$ref_group == ref_group, ]
		}	
	}
td	
} 