#' Compute pooled standard deviation
#' 
#' @param formula  A formula of the type \code{out ~ group} where \code{out} is
#'   the outcome variable and \code{group} is the grouping variable. Note the
#'   grouping variable must only include only two groups.
#' @param data The data frame that the data in the formula come from.
#' @importFrom stats var
#' @export
#' @examples
#' pooled_sd(math ~ condition, star)
#' pooled_sd(reading ~ sex, star)

pooled_sd <- function(formula, data) {
	splt <- parse_form(formula, data)

	vars <- vapply(splt, var, na.rm = TRUE, numeric(1))
	ns <- vapply(splt, length, numeric(1))

	pooled <- function(v) {
		sqrt((((ns[v[1]] - 1)*vars[v[1]]) + ((ns[v[2]] - 1)*vars[v[2]])) / 
			(sum(ns[v]) - 2))
	}
tidy_out(names(splt), pooled)
}

#' Compute mean differences by various quantiles
#' 
#' @param formula  A formula of the type \code{out ~ group} where \code{out} is
#'   the outcome variable and \code{group} is the grouping variable. Note the
#'   grouping variable must only include only two groups.
#' @param data The data frame that the data in the formula come from.
#' @param qtiles Quantile bins for calculating mean differences
#' @importFrom stats quantile
#' @importFrom utils combn
#' @export
#' @examples
#' qtile_mean_diffs(reading ~ condition, star)
#' 
#' qtile_mean_diffs(reading ~ condition, 
#' 		star, 
#' 		qtiles = seq(0, 1, .2))


qtile_mean_diffs <- function(formula, data, qtiles = seq(0, 1, .33)) {
	splt <- parse_form(formula, data)
	qtile_l <- lapply(splt, function(x) {
		split(x, cut(x, quantile(x, qtiles, na.rm = TRUE)))
	})

	mean_diffs <- function(v) {
		Map(function(x, y) mean(y, na.rm = TRUE) - mean(x, na.rm = TRUE),
			qtile_l[[ v[1] ]], 
			qtile_l[[ v[2] ]])
	}
	td <- tidy_out(names(qtile_l), mean_diffs)	
	td$estimate <- unlist(td$estimate)
	
	low_qtiles <- qtiles[-length(qtiles)]
	high_qtiles <- qtiles[-1]

	td$cut <- rep(rep(low_qtiles, each = length(combn(names(splt), 2)) / 2), 2)
	td$high_qtile <- rep(rep(high_qtiles, 
						each = length(combn(names(splt), 2)) / 2), 2)
	names(td)[3] <- "low_qtile"

td[ ,c(1:3, 5, 4)]
}

#' Compute sample size for each quantile bin for each group
#' 
#' @param formula  A formula of the type \code{out ~ group} where \code{out} is
#'   the outcome variable and \code{group} is the grouping variable. Note the
#'   grouping variable must only include only two groups.
#' @param data The data frame that the data in the formula come from.
#' @param qtiles Quantile bins for calculating mean differences
#' @importFrom stats quantile
#' @export
#' @examples
#' qtile_n(reading ~ condition, star)
#' 
#' qtile_n(reading ~ condition, 
#' 		star, 
#' 		qtiles = seq(0, 1, .2))

qtile_n <- function(formula, data, qtiles = seq(0, 1, .33)) {
	splt <- parse_form(formula, data)
	qtile_l <- lapply(splt, function(x) {
		split(x, cut(x, quantile(x, qtiles, na.rm = TRUE)))
	})
	ns <- lapply(qtile_l, function(x) vapply(x, length, numeric(1)))
	ns <- data.frame(group = rep(names(ns), each = length(ns[[1]])),
			   low_qtile = qtiles[-length(qtiles)],
			   high_qtile = qtiles[-1],		   
			   n = unlist(ns))
ns
}

se_es <- function(n1, n2, d) {
		sqrt((n1 + n2)/(n1*n2) + d^2/(2*((n1 + n2))))
}

#' Compute effect sizes by quantile bins
#' 
#' Returns a data frame with the estimated effect size by the provided 
#' percentiles. Currently, the effect size is equivalent to Cohen's d, but 
#' future development will allow this to vary.
#' 
#' @param formula  A formula of the type \code{out ~ group} where \code{out} is
#'   the outcome variable and \code{group} is the grouping variable. Note the
#'   grouping variable must only include only two groups.
#' @param data The data frame that the data in the formula come from.
#' @param ref_group Optional character vector (of length 1) naming the
#'   reference group to be plotted on the x-axis. Defaults to the highest
#'   scoring group.
#' @param qtiles The percentiles to split the data by and calculate effect 
#' sizes. Essentially, this is the binning argument. Defaults to 
#' \code{seq(0, 1, .33)}, which splits the distribution into thirds (lower,
#' middle, upper). Any sequence is valid, but it is recommended the bins be
#' even. For example \code{seq(0, 1, .1)} would split the distributions into
#' deciles.
#' @export
#' @examples
#' 
#' # Compute effect sizes (Cohen's d) by default quantiles
#' qtile_es(reading ~ condition, star)
#' 
#' # Compute Cohen's d by quintile
#' qtile_es(reading ~ condition, 
#' 		star, 
#' 		qtiles = seq(0, 1, .2))
#' 
#' # Report effect sizes only relative to regular-sized classrooms
#' qtile_es(reading ~ condition, 
#' 		star, 
#' 		ref_group = "reg",
#' 		qtiles = seq(0, 1, .2))


qtile_es <- function(formula, data, ref_group = NULL, 
	qtiles = seq(0, 1, .33)) {
	if(is.null(ref_group)) {
		splt <- parse_form(formula, data)
		ref_group <- names(
						which.max(
							vapply(splt, mean, na.rm = TRUE, numeric(1))
							)
						)
	}

	means <- qtile_mean_diffs(formula, data, qtiles)
	means <- means[means$ref_group == ref_group, ]

	sds <- pooled_sd(formula, data)
	names(sds)[3] <- "pooled_sd"

	es <- merge(means, sds, by = c("ref_group", "foc_group"), all.x = TRUE)
	es$es <- es$estimate / es$pooled_sd
	es$midpoint <- (es$low_qtile + es$high_qtile) / 2

	ns <- qtile_n(formula, data, qtiles)
	es <- merge(es, ns, 
					by.x = c("ref_group", "low_qtile", "high_qtile"),
					by.y = c("group", "low_qtile", "high_qtile"),
					all.x = TRUE)
	names(es)[ncol(es)] <- "ref_group_n"
	es <- merge(es, ns, 
					by.x = c("foc_group", "low_qtile", "high_qtile"),
					by.y = c("group", "low_qtile", "high_qtile"),
					all.x = TRUE)
	names(es)[ncol(es)] <- "foc_group_n"


	es$se <- se_es(es$ref_group_n, es$foc_group_n, es$es)


es[order(es$midpoint), c(4, 1:3, 8, 7, 11)]
}

