#' @title Fit MM-type regression
#' @description Fit MM-type regression with \code{\link[robustbase]{lmrob}}
#' @param formula a symbolic description of the model to be fit. See 
#' \code{\link[stats]{lm}} and \code{\link[stats]{formula}} for more details.
#' @param data an optional data frame, list or environment 
#' (or object coercible by \code{\link[base]{as.data.frame}} to a data frame) 
#' containing the variables in the model. If not found in data, 
#' the variables are taken from environment(formula), 
#' typically the environment from which lmrob is called.
#' @param ... additional arguments can be used to specify control parameters directly instead of (but not in addition to!) via control.
#' @return An object of class \code{\link[robustbase]{lmrob}}
#' @details see \code{\link[robustbase]{lmrob}}
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[robustbase]{lmrob}}
#' @rdname fitSMDM
#' @export 
#' @importFrom robustbase lmrob
fitSMDM <- function(formula, data, ...) {
  roblm <- robustbase::lmrob(formula, data, setting = "KS2011",
             maxit.scale = 1000)
  return(roblm)
}