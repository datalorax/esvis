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

descrip_stats <- function(data, formula) {
  rhs  <- labels(terms(formula))
  lhs  <- all.vars(formula)[1]
 
  data %>% 
    mutate_at(vars(!!!syms(rhs)), funs(as.character)) %>% 
    group_by(!!!syms(rhs)) %>% 
    summarize(n    = n(),
              mn = mean(!!sym(lhs), na.rm = TRUE),
              vr = var( !!sym(lhs), na.rm = TRUE)) %>% 
    crossing(., .) %>% 
    filter(!(.data$n == .data$n1 & 
             .data$mn == .data$mn1 & 
             .data$vr == .data$vr1))
}
