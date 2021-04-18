#' Make words to vector
#'
#' This function makes a vector from words list
#'
#' @param vars words to be a vector
#' @return vector
#' @keywords vec
#' @export
#' @examples vec(a, b, c)
#'

vec = function(vars, ...){
  arg <- deparse(substitute(vars))
  dots <- substitute(list(...))[-1]
  vars = c(arg, sapply(dots, deparse))
  return(vars)
}
