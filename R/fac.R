#' Make variables to factor
#'
#' This function transforms variables to the factor class
#'
#' @param data Dataframe to be used
#' @param vars Variables to be transformed to factor
#' @return Converted dataframe
#' @keywords fac
#' @export
#' @examples fac(mtcars, am, cyl)
#'

fac = function(data, vars, ...){
  arg <- deparse(substitute(vars))
  dots <- substitute(list(...))[-1]
  vars = c(arg, sapply(dots, deparse))
  err = c()
  for(var in vars){
    if(var %in% colnames(data)){
      data[[var]] = factor(data[[var]])
    }
    else{
      err = c(err, var)
    }
  }
  if(length(err) != 0){
    cat('Factor transform error:', paste0(err, collapse=', '))
  }
  return(data)
}

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
