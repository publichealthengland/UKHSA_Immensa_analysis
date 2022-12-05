# Negation of "in" function
`%ni%` <- Negate(`%in%`)

#' exp_fun
#'
#' `exp_fun()` Applies the log transform with an offset of 1
#'
#' @param x: int
#'
#' @param log_const: int

adjusted_log <- function(x, log_const) {
  return(log(log_const + x))
}

#' exp_fun
#'
#' `exp_fun()` Inverts the logarithm
#'
#' @param x: int
#'
#' @param log_const: int

exp_fun <- function(x, log_const) {
  if (class(x) == "numeric") {
    return(exp(x) - log_const)
  } else {
    return(x)
  }
}
