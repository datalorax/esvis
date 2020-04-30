#' Pooled Standard Deviation
#' 
#' The denominator for Cohen's d
#' @keywords internal
#' @param n1 The sample size for group 1
#' @param n2 The sample size for group 2
#' @param vr1 The variance for group 1
#' @param vr2 The variance for group 2
#' 
psd <- function(n1, n2, vr1, vr2) {
		dnum1 <- (n1 - 1)*vr1
    dnum2 <- (n2 - 1)*vr2
    ddnom  <- n1 + n2 - 2
    
    sqrt((dnum1 + dnum2) / ddnom)
}

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
	(mn1 - mn2) / psd(n1, n2, vr1, vr2)
}

coh_se <- function(n1, n2, d) {
  sqrt((n1 + n2)/(n1*n2) + d^2/(2*((n1 + n2))))
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
#' @param data The data frame used for estimation - ideally structured in a tidy 
#' format.
#' @param formula A formula of the type \code{out ~ group} where \code{out} is
#' the outcome variable and \code{group} is the grouping variable. Note this
#' variable can include any arbitrary number of groups. Additional variables 
#' can be included with \code{+} to produce separate estimates by the secondary 
#' or tertiary variables of interest (e.g., \code{out ~ group + characteristic1 
#' + characteristic2}). 
#' @param ref_group Optional. A character vector or forumla listing the 
#' reference group levels for each variable on the right hand side of the 
#' formula, supplied in the same order as the formula. Note that if using the
#' formula version, levels that are numbers, or include hyphens, spaces, etc., 
#' should be wrapped in back ticks (e.g., 
#' \code{ref_group = ~ Active + `Non-FRL`}, or \code{ref_group = ~`8`}). When 
#' in doubt, it is safest to use the back ticks, as they will not interfere 
#' with anything if they are not needed. See examples below for more details.
#' @param se Logical. Should the standard error of the effect size be 
#'   estimated and returned in the resulting data frame? Defaults to 
#'   \code{TRUE}.
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

coh_d <- function(data, formula, ref_group = NULL, se = TRUE) {
  rhs  <- labels(terms(formula))

  stats <- descrip_cross(data, formula, length = length, mean = mean, var = var) %>% 
    mutate_if(is.integer, as.double)
  
  d <- stats %>% 
    mutate(coh_d = coh(.data$length1, 
                       .data$length, 
                       .data$mean1, 
                       .data$mean, 
                       .data$var1, 
                       .data$var),
           coh_se = coh_se(.data$length1,
                           .data$length,
                           .data$coh_d)) %>% 
    select(-.data$length, 
           -.data$length1, 
           -.data$mean, 
           -.data$mean1, 
           -.data$var, 
           -.data$var1) %>% 
    ungroup()
  
  if(!is.null(ref_group)) {
    d <- ref_subset(d, formula, ref_group)
  }
  rename_ref_foc(d, formula)
}
 
#' Hedge's g 
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
#' @param keep_d Logical. Should Cohen's \emph{d} be reported along with 
#'   Hedge's \code{g}? Defaults to \code{TRUE}.
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

hedg_g <- function(data, formula, ref_group = NULL,
                   keep_d = TRUE) {
  stats <- descrip_cross(data, formula,
                         length = length, mean = mean, var = var)
  
  g <- stats %>% 
    mutate(coh_d  = coh(.data$length, .data$length1, 
                        .data$mean, .data$mean1, 
                        .data$var, .data$var1),
           hedg_g = hedg(.data$length, .data$length1, .data$coh_d)) %>% 
    select(-.data$length, -.data$length1, 
           -.data$mean, -.data$mean1, 
           -.data$var, -.data$var1)
  
  if(!keep_d) g <- select(g, -.data$coh_d)
  
  if(!is.null(ref_group)) {
    g <- ref_subset(g, formula, ref_group)
  }
  rename_ref_foc(g, formula)
}

mean_diff <- function(data, formula, ref_group, qtile_groups = NULL) {
  descrip_cross(data, formula, mean = mean, qtile_groups = qtile_groups) %>% 
    mutate(mean_diff = .data$mean1 - .data$mean) %>% 
    select(-.data$mean, -.data$mean1)
}

pooled_sd <- function(data, formula, ref_group, keep_n = FALSE) {
  out <- descrip_cross(data, formula, length = length, var = var) %>% 
    mutate(psd = psd(.data$length, .data$length1, .data$var, .data$var1)) %>% 
    select(-.data$var, -.data$var1)
  
  if(!keep_n) {
    out <- select(out, -.data$length, -.data$length1)
  }
  out
}

#' Calculate binned effect sizes
#' @inheritParams coh_d
#' @param qtile_groups The number of quantile bins to split the data by and 
#'   calculate effect sizes. Defaults to 3 bins (lower, middle, upper).
#' @param es The effect size to calculate. Currently the only options are 
#'   "d" or "g".
#' @param rename Logical. Should the column names be relabeled according to
#'   the reference and focal groups. Defaults to \code{TRUE}.
#' @return A data frame with the corresponding effect sizes.
#' @export

binned_es <- function(data, formula, ref_group = NULL, qtile_groups = 3,
                      es = "g", rename = TRUE) {
  mn_diff <- mean_diff(data, formula, qtile_groups = qtile_groups)  
  p_sd <- pooled_sd(data, formula, keep_n = TRUE)
  
  if(es != "g" & es != "d") stop("es must be one of `'g'` or `'d'`.")

  d <- suppressMessages(left_join(mn_diff, p_sd)) %>% 
    mutate(es    = .data$mean_diff/.data$psd,
           es_se = coh_se(.data$length, .data$length1, .data$es))
    
  if(es == "g") {
    d <- mutate(d, es = hedg(.data$length, .data$length1, .data$es)) 
  }
  
  if(!is.null(ref_group)) {
    d <- ref_subset(d, formula, ref_group)
  }
  if(rename) d <- rename_ref_foc(d, formula)
  d
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
    mutate_at(vars(!!!syms(rhs)), list(as.character)) %>% 
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
  ecdf <- ecdf2 <- ecdf_fun(data, formula, cuts) %>% 
    mutate(nd = map2(.data$nd, .data$ecdf, ~data.frame(x = .x, y = .y))) %>% 
    select(-.data$ecdf)
  names(ecdf2) <- paste0(names(ecdf), "1")
  
  cross(ecdf, ecdf2) %>% 
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
#' \dontrun{
#' auc(benchmarks, 
#'       math ~ ell + frl + season,
#'       ref_group = ~`Non-ELL` + `Non-FRL` + Fall)
#' 
#' # Same thing but with character vector supplied, rather than a formula
#' auc(benchmarks, 
#'       math ~ ell + frl + season,
#'       ref_group = c("Non-ELL", "Non-FRL", "Fall"))
#' }
#' 

auc <- function(data, formula, ref_group = NULL, rename = TRUE) {
  rhs <- labels(terms(formula))
  
  d <- paired_ecdf(data, formula) %>% 
    mutate(auc = map_dbl(.data$matched, ~integrate.xy(.$y_foc, .$y_ref, 
                                                      use.spline = FALSE))) %>% 
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
#' \href{https://journals.sagepub.com/doi/abs/10.3102/1076998609332755}{Ho, 2009}. The V
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
#' \dontrun{
#' v(benchmarks, 
#'       math ~ ell + frl + season,
#'       ref_group = ~`Non-ELL` + `Non-FRL` + Fall)
#' 
#' # Same thing but with character vector supplied, rather than a formula
#' v(benchmarks, 
#'       math ~ ell + frl + season,
#'       ref_group = c("Non-ELL", "Non-FRL", "Fall"))
#' }
#' 

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
#' Computes the proportion of the corresponding group, as specified by the
#' \code{formula}, scoring above the specified \code{cuts}.
#' @inheritParams ecdf_fun
#' @inheritParams coh_d
#' @return Tidy data frame of the proportion above the cutoff for 
#' each (or selected) groups.
#' @seealso [esvis::pac_compare(), esvis::tpac(), esvis::tpac_diff()]
#' @export 
#' @examples
#' # Compute differences for all pairwise comparisons for each of three cuts
#' pac(star,
#'     reading ~ condition,
#' 		 cuts = c(450, 500, 550))
#' 		 
#' pac(star,
#'     reading ~ condition + freelunch + race, 
#' 		 cuts = c(450, 500))
#' 
#' pac(star,
#'     reading ~ condition + freelunch + race, 
#' 		 cuts = c(450, 500),
#' 		 ref_group = ~small + no + white) 

pac <- function(data, formula, cuts, ref_group = NULL) {
  rhs <- labels(terms(formula))
  d <- ecdf_fun(data, formula, cuts) 
  
  cut_tbl <- data.frame(matrix(rep(cuts, each = nrow(d)), nrow = nrow(d)))
  
  if(length(cuts) == 1) {
    names(cut_tbl) <- "cut"
  }
  
  d <- dplyr::bind_cols(d, cut_tbl) 
  
  if(length(cuts) == 1) {
    d <- unnest(d, cols = c(.data$ecdf, .data$nd))
  }
  
  if(length(cuts) > 1) {
    d <- d %>% 
      gather("dis", "cut", matches("^X\\d")) %>% 
      unnest(cols = c(.data$ecdf, .data$nd)) %>% 
      filter(.data$nd == .data$cut)
  }  
  if(!is.null(ref_group)) {
    d <- ref_subset(d, formula, ref_group)
  }
  d %>% 
    mutate(pac = 1 - .data$ecdf) %>% 
    distinct() %>% 
    select(rhs, cut, pac)
}

#' Compute the difference in the proportion above a specific cut location
#' 
#' Computes the difference in the proportion above the specified \code{cuts} 
#' for all possible pairwise comparisons of the groups specified by the 
#' \code{formula}.
#' @inheritParams ecdf_fun
#' @inheritParams coh_d
#' @return Tidy data frame of the proportion above the cutoff for 
#' each (or selected) groups.
#' @seealso [esvis::pac(), esvis::tpac(), esvis::tpac_diff()]
#' @export 
#' @examples
#' # Compute differences for all pairwise comparisons for each of three cuts
#' pac_compare(star,
#'     reading ~ condition, 
#' 		 cuts = c(450, 500, 550)) 
#' 		 
#' pac_compare(star,
#'     reading ~ condition + freelunch + race, 
#' 		 cuts = c(450, 500))
#' 
#' pac_compare(star,
#'     reading ~ condition + freelunch + race, 
#' 		 cuts = c(450, 500),
#' 		 ref_group = ~small + no + white) 

pac_compare <- function(data, formula, cuts, ref_group = NULL) {
  rhs <- labels(terms(formula))
  d1 <- d2 <- pac(data, formula, cuts)
  names(d2) <- paste0(names(d1), "1")
  
  d <- cross(d1, d2) %>% 
    filter(cut == .data$cut1) %>% 
    mutate(pac_diff = .data$pac - .data$pac1) 
  
  d <- map2_df(rhs, 
          paste0(rhs, 1), 
          ~filter(d, !!sym(.x) != !!sym(.y)))
  
  if(!is.null(ref_group)) {
    d <- ref_subset(d, formula, ref_group)
  }
  d <- rename_ref_foc(d, formula)
  
  d %>%
    ungroup() %>% 
    rename("pac_ref" = "pac",
           "pac_foc" = "pac1") %>%
    select(.data$cut,
           ends_with("_ref"),
           ends_with("_foc"),
           .data$pac_diff, 
           -.data$cut1)
    
}

#' Transformed proportion above the cut
#' 
#' This function transforms calls to \link{pac} into standard deviation units.
#' Function assumes that each distribution is distributed normally with 
#' common variances. See 
#' \href{http://journals.sagepub.com/doi/abs/10.3102/1076998611411918}{Ho &
#'  Reardon, 2012}
#' @inheritParams ecdf_fun
#' @inheritParams coh_d
#' @return Tidy data frame of the proportion above the cutoff for 
#' each (or selected) groups.
#' @seealso [esvis::pac(), esvis::pac_diff(), esvis::tpac_compare()]
#' @export 
#' @examples
#' # Compute differences for all pairwise comparisons for each of three cuts
#' tpac(star,
#'     reading ~ condition, 
#' 		 cut = c(450, 500, 550)) 
#' 		 
#' tpac(star,
#'     reading ~ condition + freelunch + race, 
#' 		 cut = c(450, 500))
#' 
#' tpac(star,
#'     reading ~ condition + freelunch + race, 
#' 		 cut = c(450, 500),
#' 		 ref_group = ~small + no + white)  

tpac <- function(data, formula, cuts, ref_group = NULL) {
  pac(data, formula, cuts, ref_group) %>% 
    mutate(pac = qnorm(.data$pac)) %>% 
    rename("tpac" = "pac")
}

#' Compare Transformed Proportion Above the Cut
#' 
#' This function compares all possible pairwise comparisons, as supplied by 
#' \code{formula}, in terms of the transformed proportion above the cut. This
#' is an effect-size like measure of the differences between two groups as the
#' cut point(s) in the distribution. See 
#' \href{http://journals.sagepub.com/doi/abs/10.3102/1076998611411918}{Ho &
#'  Reardon, 2012}
#' @inheritParams ecdf_fun
#' @inheritParams coh_d
#' @return Tidy data frame of the proportion above the cutoff for 
#' each (or selected) groups.
#' @seealso [esvis::pac(), esvis::pac_diff(), esvis::tpac()]
#' @export 
#' @examples
#' # Compute differences for all pairwise comparisons for each of three cuts
#' tpac_compare(star,
#'     reading ~ condition, 
#' 		 cut = c(450, 500, 550)) 
#' 		 
#' tpac_compare(star,
#'     reading ~ condition + freelunch + race, 
#' 		 cut = c(450, 500))
#' 
#' tpac_compare(star,
#'     reading ~ condition + freelunch + race, 
#' 		 cut = c(450, 500),
#' 		 ref_group = ~small + no + white)  

tpac_compare <- function(data, formula, cuts, ref_group = NULL) {
  pac_compare(data, formula, cuts, ref_group) %>% 
    mutate(pac_ref = qnorm(.data$pac_ref),
           pac_foc = qnorm(.data$pac_foc),
           tpac_diff = .data$pac_ref - .data$pac_foc) %>% 
    rename("tpac_ref" = "pac_ref",
           "tpac_foc" = "pac_foc") %>% 
    select(-.data$pac_diff)
}
