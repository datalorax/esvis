is.formula <- function(x){
   inherits(x,"formula")
}

rename_ref_foc <- function(out, formula) {
   rhs  <- labels(terms(formula))
   
   ref <- names(out) %in% rhs
   foc <- grepl(paste0(rhs, "\\d$", collapse = "|"), names(out))
   
   nms_ref <- paste0(names(out)[ref],"_ref")
   nms_foc <- gsub("\\d", "_foc", names(out)[foc])
   
   names(out)[ref] <- nms_ref
   names(out)[foc] <- nms_foc
   
   out
}

ref_subset <- function(out, formula, ref_group) {
  rhs  <- labels(terms(formula))
  
  if(is.formula(ref_group)) {
    ref_group <- gsub("`", "", labels(terms(ref_group)))
  }
  ref_join <- data.frame(as.list(as.character(ref_group)), 
                         stringsAsFactors = FALSE)
  names(ref_join) <- rhs[seq_along(ref_group)]
  
  suppressMessages(semi_join(out, ref_join))
} 


#' Report descriptive stats for all possible pairings on the rhs of the formula.
#' @keywords internal
#' @param formula A formula of the type \code{out ~ group} where \code{out} is
#' the outcome variable and \code{group} is the grouping variable. Note this
#' variable can include any arbitrary number of groups. Additional variables 
#' can be included with \code{+} to produce descriptive stats by the secondary 
#' or tertiary variable of interest (e.g., \code{out ~ group + characteristic1 
#' + characteristic2}). 

descrip_stats <- function(data, formula, ..., qtile_groups = NULL) {
  rhs  <- labels(terms(formula))
  lhs  <- all.vars(formula)[1]
  f <- quos(...)
  
  if(length(f) == 0) {
    stop("No function supplied to ...")
  }
  
  d <- data %>%
    select(rhs, lhs) %>% 
    na.omit() %>% 
    mutate_at(vars(!!!syms(rhs)), funs(as.character)) %>%
    group_by(!!!syms(rhs)) 
  
  if(!is.null(qtile_groups)) {
    d <- d %>% 
      group_by(!!!syms(rhs)) %>% 
      nest() %>% 
      mutate(q = map(data, ~as.numeric(cut2(.[[lhs]], g = qtile_groups)))) %>%  
      unnest() %>%
      group_by(!!!syms(rhs), .data$q)
  }
  d <- d %>%
    summarize_at(lhs, funs(!!!f)) 
  
  if(length(f) == 1) {
    names(d)[grep(lhs, names(d))] <- gsub("~", "", as.character(f))
  }
  d
}

descrip_cross <- function(data, formula, ..., qtile_groups = NULL) {
  rhs  <- labels(terms(formula))
  f <- quos(...)
  
  d <- descrip_stats(data, formula, ..., qtile_groups = qtile_groups) %>%
    cross(., .)

  zero_group <- paste(rhs, "==", paste0(rhs, 1), collapse = " & ")
  if(!is.null(qtile_groups)) zero_group <- paste0("q == q1 & ", zero_group)

  test <- filter(d, !!parse_quo(zero_group, env = parent.frame()))
  var <- as.character(quo_get_expr(f[[1]]))

  if(any((test[ ,var] - test[ ,paste0(var, 1)]) != 0)) {
    stop("Reference Group Filtering failed. Use `all == TRUE` and
         filter manually.")
  }
  filt_expr <- parse_quo(paste0("!(", zero_group, ")"),
                         env = parent.frame())
  d <- d %>%
    filter(!!filt_expr)

  if(!is.null(qtile_groups)) {
    d <- d %>%
      filter(.data$q == .data$q1) %>%
      mutate(qtile_ub = .data$q / max(.data$q),
             qtile_lb = .data$qtile_ub - min(.data$qtile_ub)) %>%
      ungroup() %>%
      select(.data$q,
             .data$qtile_lb,
             .data$qtile_ub,
             everything(),
             -.data$q1)
  }
 d
}



#### Old version of tidyr::crossing
drop_empty <- function(x, factor = TRUE) {
  empty <- map_lgl(x, function(x) length(x) == 0 & (!factor | !is.factor(x)))
  x[!empty]
}
seq_nrow <- function(x) seq_len(nrow(x))

cross_df <- function(x, y) {
  x_idx <- rep(seq_nrow(x), each = nrow(y))
  y_idx <- rep(seq_nrow(y), nrow(x))
  bind_cols(x[x_idx, , drop = FALSE], y[y_idx, , drop = FALSE])
}

is_list <- function(x) map_lgl(x, is.list)

ulevels <- function(x) {
  if (is.factor(x)) {
    orig_levs <- levels(x)
    x <- addNA(x, ifany = TRUE)
    levs <- levels(x)
    factor(levs, levels = orig_levs, ordered = is.ordered(x), exclude = NULL)
  } else if (is.list(x)) {
    unique(x)
  } else {
    sort(unique(x), na.last = TRUE)
  }
}

cross <- function(...) {
  x <- lst(...)
  stopifnot(is_list(x))
  
  x <- drop_empty(x)
  if (length(x) == 0) {
    return(data.frame())
  }
  
  is_atomic <- map_lgl(x, is_atomic)
  is_df <- map_lgl(x, is.data.frame)
  
  # turn each atomic vector into single column data frame
  col_df <- map(x[is_atomic], function(x) tibble(x = ulevels(x)))
  col_df <- map2(col_df, names(x)[is_atomic], set_names)
  x[is_atomic] <- col_df
  
  Reduce(cross_df, x)
}