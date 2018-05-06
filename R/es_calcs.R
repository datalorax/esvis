#' Cohen's d 
#' 
#' Wraps the equation into a function
#' @keywords internal
#' @param n1 The sample size for group 1
#' @param n2 The sample size for group 2
#' @param mn1 The mean for group 1
#' @param mn2 The mean for group 2
#' @param vr1 The variance for group 1
#' @param vr2 The variance for group 2

coh <- function(n1, n2, mn1, mn2, vr1, vr2) {
		num   <- mn1 - mn2 
    
		dnum1 <- (n1 - 1)*vr1
    dnum2 <- (n2 - 1)*vr2
    ddnom  <- n1 + n2 - 2
    
    denom <- sqrt((dnum1 + dnum2) / ddnom)
    
    num / denom
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
#' @inheritParams pp_plot 
#' @param ref_group Optional. A character vector or forumla listing the 
#' reference group levels for each variable on the right hand side of the 
#' formula, supplied in the same order as the formula. Note that if using the
#' formula version, levels that are numbers, or include hyphens, spaces, etc., 
#' should be wrapped in back ticks (e.g., 
#' \code{ref_group = ~ Active + `Non-FRL`}, or \code{ref_group = ~`8`}). When 
#' in doubt, it is safest to use the back ticks, as they will not interfere 
#' with anything if they are not needed. See examples below for more details.
#' @return By default the Cohen's \emph{d} for all possible pairings of
#'  the grouping factor(s) are returned.
#' @export
#' @examples
#' 
#' # Calculate Cohen's d for all pairwise comparisons
#' coh_d(star, reading ~ condition) 
#' 
#' # Report only relative to regular-sized classrooms
#' coh_d(star,
#'       reading ~ condition, 
#' 		   ref_group = "reg")
#' 
#' # Report by ELL and FRL groups for each season, compare to non-ELL students
#' # who were not eligible for free or reduced price lunch in the fall (using
#' # the formula interface for reference group referencing).
#' 
#' coh_d(benchmarks, 
#'       math ~ ell + frl + season,
#'       ref_group = ~`Non-ELL` + `Non-FRL` + Fall)
#' 
#' # Same thing but with character vector supplied, rather than a formula
#' coh_d(benchmarks, 
#'       math ~ ell + frl + season,
#'       ref_group = c("Non-ELL", "Non-FRL", "Fall"))

coh_d <- function(data, formula, ref_group = NULL) {
  rhs  <- labels(terms(formula))

  stats <- descrip_stats(data, formula)
  
  d <- stats %>% 
    mutate(coh_d = coh(.data$n, 
                       .data$n1, 
                       .data$mn, 
                       .data$mn1, 
                       .data$vr, 
                       .data$vr1)) %>% 
    select(-.data$n, 
           -.data$n1, 
           -.data$mn, 
           -.data$mn1, 
           -.data$vr, 
           -.data$vr1) 
  
  if(!is.null(ref_group)) {
    d <- ref_subset(d, formula, ref_group)
  }
  rename_ref_foc(d, formula)
}

#' Hedge's d 
#' 
#' Wraps the equation into a function
#' @keywords internal
#' @param n1 The sample size for group 1
#' @param n2 The sample size for group 2
#' @param d The value of Cohen's d
#' 
hedg <- function(n1, n2, d) {
  d * (1 - (3 / (4*(n1 + n2) - 9)))
}

#' Compute Hedges' \emph{g}
#' This function calculates effect sizes in terms of Hedges' \emph{g}, also
#' called the corrected (for sample size) effect size. See
#' \code{\link{coh_d}} for the uncorrected version. Also see 
#' \href{https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3840331/}{Lakens (2013)}
#' for a discussion on different types of effect sizes and their
#' interpretation. Note that missing data are removed from the calculations of 
#' the means and standard deviations.
#' @inheritParams coh_d 
#' @return By default the Hedges' \emph{g} for all possible pairings of
#'  the grouping factor are returned as a tidy data frame.
#' @export
#' @examples
#' 
#' # Calculate Hedges' g for all pairwise comparisons
#' hedg_g(star, reading ~ condition) 
#' 
#' # Report only relative to regular-sized classrooms
#' hedg_g(star, 
#'        reading ~ condition, 
#'        ref_group = "reg")
#' 
#' # Report by ELL and FRL groups for each season, compare to non-ELL students
#' # who were not eligible for free or reduced price lunch in the fall (using
#' # the formula interface for reference group referencing).
#' 
#' hedg_g(benchmarks, 
#'       math ~ ell + frl + season,
#'       ref_group = ~`Non-ELL` + `Non-FRL` + Fall)
#' 
#' # Same thing but with character vector supplied, rather than a formula
#' hedg_g(benchmarks, 
#'       math ~ ell + frl + season,
#'       ref_group = c("Non-ELL", "Non-FRL", "Fall"))

hedg_g <- function(data, formula, ref_group = NULL) {
  stats <- descrip_stats(data, formula)
  
  g <- stats %>% 
    mutate(coh_d  = coh(.data$n, .data$n1, 
                        .data$mn, .data$mn1, 
                        .data$vr, .data$vr1),
           hedg_g = hedg(.data$n, .data$n1, .data$coh_d)) %>% 
    select(-.data$n, -.data$n1, 
           -.data$mn, -.data$mn1, 
           -.data$vr, -.data$vr1, 
           -.data$coh_d)
  
  if(!is.null(ref_group)) {
    g <- ref_subset(g, formula, ref_group)
  }
  rename_ref_foc(g, formula)
}

#' Computes the empirical cummulative distribution function for all groups
#' supplied by the formula.
#' @inheritParams coh_d
#' @param cuts Optional vector of cut scores. If supplied, the ECDF will be
#' guaranteed to include these points. Otherwise, there could be gaps in the 
#' ECDF at those particular points (used in plotting the cut scores).
#' @keywords internal

ecdf_fun <- function(data, formula, cuts = NULL) {
  if(is.null(cuts)) cuts <- 0
  rhs  <- labels(terms(formula))
  lhs  <- all.vars(formula)[1]
  
  data %>% 
    mutate_at(vars(!!!syms(rhs)), funs(as.character)) %>% 
    group_by(!!!syms(rhs)) %>% 
    nest() %>% 
    mutate(ecdf = map(.data$data, ~ecdf(.[[lhs]])),
           nd   = map(.data$data, ~c(-Inf, sort(c(unique(.[[lhs]]), cuts)), Inf)),
           ecdf = map2(.data$ecdf,.data$ nd, ~.x(.y))) %>% 
    select(-.data$data) 
}


#' Pairs empirical cummulative distribution functions for all groups
#' supplied by the formula.
#' @inheritParams ecdf_fun
#' @keywords internal

paired_ecdf <- function(data, formula, cuts = NULL) {
  ecdf_fun(data, formula, cuts) %>% 
    mutate(nd = map2(.data$nd, .data$ecdf, ~data.frame(x = .x, y = .y))) %>% 
    select(-.data$ecdf) %>% 
    crossing(., .) %>% 
    filter(!map2_lgl(.data$nd, .data$nd1, ~identical(.x, .y))) %>% 
    mutate(matched = map2(.data$nd, .data$nd1,
                          ~data.frame(x = sort(unique(.x$x, .y$x))) %>% 
                            left_join(.x, by = "x") %>% 
                            left_join(.y, 
                                      by = "x",
                                      suffix = c("_ref", "_foc")) %>% 
                            fill(names(.)))) %>% 
    select(-.data$nd, -.data$nd1)
}

#' Compute the Area Under the \link{pp_plot} Curve
#' Calculates the area under the \code{pp} curve. The area under the curve is 
#' also a useful effect-size like statistic, representing the probability that 
#' a randomly selected individual from the \code{x} distribution will have a 
#' higher value than a randomly selected individual from the \code{y} 
#' distribution.
#' @inheritParams coh_d
#' @param rename Used primarily for internal purposes. Should the column 
#' names be renamed to reference the focal and reference groups? Defaults to
#' \code{TRUE}.
#' @return By default the area under the curve for all possible pairings of
#' the grouping factor are returned. 
#' @export
#' @examples
#' 
#' # Calculate AUC for all pairwise comparisons
#' auc(star, reading ~ condition) 
#' 
#' # Report only relative to regular-sized classrooms
#' auc(star, 
#'     reading ~ condition, 
#'     ref_group = "reg")
#' 
#' # Report by ELL and FRL groups for each season, compare to non-ELL students
#' # who were not eligible for free or reduced price lunch in the fall (using
#' # the formula interface for reference group referencing).
#' 
#' auc(benchmarks, 
#'       math ~ ell + frl + season,
#'       ref_group = ~`Non-ELL` + `Non-FRL` + Fall)
#' 
#' # Same thing but with character vector supplied, rather than a formula
#' auc(benchmarks, 
#'       math ~ ell + frl + season,
#'       ref_group = c("Non-ELL", "Non-FRL", "Fall"))


auc <- function(data, formula, ref_group = NULL, rename = TRUE) {
  rhs <- labels(terms(formula))
  
  d <- paired_ecdf(data, formula) %>% 
    mutate(auc = map_dbl(.data$matched, ~integrate.xy(.$y_ref, .$y_foc))) %>% 
    select(-.data$matched)
  
  if(!is.null(ref_group)) {
    d <- ref_subset(d, formula, ref_group)
  }
  if(rename) d <- rename_ref_foc(d, formula)
  d
}

#' Calculate the V effect size statistic
#' 
#' This function calculates the effect size V, as discussed by 
#' \href{http://www.jstor.org/stable/40263526}{Ho, 2009}. The V
#' statistic is a transformation of \code{\link{auc}}, interpreted as the 
#' average difference between the distributions in standard deviation units.
#' @inheritParams coh_d
#' @return By default the V statistic for all possible pairings of
#'  the grouping factor are returned as a tidy data frame. Alternatively, a 
#' vector can be returned, and/or only the V corresponding to a specific
#' reference group can be returned.
#' @export
#' @examples 
#' 
#' # Calculate V for all pairwise comparisons
#' v(star, reading ~ condition) 
#' 
#' # Report only relative to regular-sized classrooms
#' v(star, 
#'     reading ~ condition, 
#'     ref_group = "reg")
#' 
#' # Report by ELL and FRL groups for each season, compare to non-ELL students
#' # who were not eligible for free or reduced price lunch in the fall (using
#' # the formula interface for reference group referencing).
#' 
#' v(benchmarks, 
#'       math ~ ell + frl + season,
#'       ref_group = ~`Non-ELL` + `Non-FRL` + Fall)
#' 
#' # Same thing but with character vector supplied, rather than a formula
#' v(benchmarks, 
#'       math ~ ell + frl + season,
#'       ref_group = c("Non-ELL", "Non-FRL", "Fall"))

v <- function(data, formula, ref_group = NULL) {
  d <- auc(data, formula, rename = FALSE) %>% 
    mutate(v = sqrt(2)*qnorm(auc)) %>% 
    select(-auc)
  
  if(!is.null(ref_group)) {
    d <- ref_subset(d, formula, ref_group)
  }
  rename_ref_foc(d, formula)
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
#' @param cut The point(s) at the scale from which the proportion above should
#' be calculated from.
#' @param ref_group Optional. If the name of the reference group is provided
#' (must be character and match the grouping level exactly), only the
#' estimates corresponding to the given reference group will be returned.
#' @param diff Logical, defaults to \code{TRUE}. Should the difference between
#' the groups be returned? If \code{FALSE} the raw proportion above
#' the cut is returned for each group.
#' @param tidy Logical. Should the data be returned in a tidy data frame? (see
#' \href{https://www.jstatsoft.org/article/view/v059i10}{Wickham, 2014}). If
#'  false, effect sizes returned as a vector.
#' @return Tidy data frame (or vector) of the proportion above the cutoff for 
#' each (or selected) groups.
#' @export 
#' @examples
#' # Compute differences for all pairwise comparisons for each of three cuts
#' pac(reading ~ condition, 
#' 		star, 
#' 		cut = c(450, 500, 550)) 
#' 
#' # Report raw PAC, instead of differences in PAC
#' pac(reading ~ condition, 
#' 		star, 
#' 		cut = c(450, 500, 550), 
#' 		diff = FALSE) 
#' 
#' # Report differences with regular-sized classrooms as the reference group
#' pac(reading ~ condition, 
#' 		star, 
#' 		cut = c(450, 500, 550), 
#' 		ref_group = "reg") 
#' 
#' # Return a matrix instead of a data frame 
#' # (returns a vector if only one cut is provided)
#' pac(reading ~ condition, 
#' 		star, 
#' 		cut = c(450, 500, 550), 
#' 		ref_group = "reg",
#' 		tidy = FALSE) 

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
				
				if(!is.null(ref_group)) {
					td <- td[td$group == ref_group, ]
				}
			return(td)
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
				mat <- do.call("rbind", splt_td)
			return(mat)
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
#' \href{https://www.jstatsoft.org/article/view/v059i10}{Wickham, 2014}). If
#' false, effect sizes returned as a matrix or vector (depending on other
#' arguments passed).
#' @return A tidy data frame (or vector) of the transformed proportion above
#' the cutoff. Optionally (and by default) all pairwise comparisons are 
#' calculated and returned.
#' @importFrom stats qnorm
#' @export 
#' @examples
#' # Compute transformed PAC differences for all pairwise comparisons 
#' # for each of three cuts
#' tpac(reading ~ condition, 
#' 		star, 
#' 		cut = c(450, 500, 550)) 
#' 
#' # Report raw transformed PAC, instead of differences in transformed PAC
#' tpac(reading ~ condition, 
#' 		star, 
#' 		cut = c(450, 500, 550), 
#' 		diff = FALSE) 
#' 
#' # Report transformed differences with regular-sized classrooms as the
#' # reference group
#' tpac(reading ~ condition, 
#' 		star, 
#' 		cut = c(450, 500, 550), 
#' 		ref_group = "reg") 
#' 
#' # Return a matrix instead of a data frame 
#' # (returns a vector if only one cut is provided)
#' tpac(reading ~ condition, 
#' 		star, 
#' 		cut = c(450, 500, 550), 
#' 		ref_group = "reg",
#' 		tidy = FALSE) 


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
				if(!is.null(ref_group)) {
					td <- td[td$group == ref_group, ]
				}
			return(td)
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

			diffs_1 <- lapply(diffs_1, function(x) {
				names(x) <- apply(
					combn(colnames(tpacs), 2), 
					2, 
					paste, collapse = "-")
				x
			})
			names(diffs_1) <- rownames(tpacs)
			diffs_1 <- do.call(rbind, diffs_1)
	
			diffs_2 <- Map(function(i) 
								apply(combn(rev(tpacs[i, ]), 2), 2, diff), 
							seq_len(nrow(tpacs)))
			diffs_2 <- lapply(diffs_2, function(x) {
				names(x) <- apply(
					combn(rev(colnames(tpacs)), 2), 
					2, 
					paste, collapse = "-")
				x
			})
			names(diffs_2) <- rownames(tpacs)
			diffs_2 <- do.call(rbind, diffs_2)
			
			if(tidy == FALSE) {
				m <- cbind(diffs_1, diffs_2)
			return(m)
			}
			else {
				one <- as.data.frame.table(diffs_1, responseName = "estimate")
				two <- as.data.frame.table(diffs_2, responseName = "estimate")	
				td <- rbind(one, two)
				vars <- strsplit(as.character(td$Var2), "-")
				td$ref_group <- vapply(vars, function(x) x[1], character(1))
				td$foc_group <- vapply(vars, function(x) x[2], character(1))
				names(td)[1] <- "cut"
				td <- td[order(td$ref_group), c(4:5, 1, 3)]
				rownames(td) <- NULL
				if(!is.null(ref_group)) {
					td <- td[td$ref_group == ref_group, ]
				}
			}
		}
	}
td
}
