#' @title Expression linear model equation and r-square.
#' @description Computes and returns (as.character) linear model equation and R-square value.
#' @param df input dataframe including columns "Sample.VWC" \code{y} and "Sensor.VWC"  \code{x}
#' @param method define method, \code{lm} for OLS estimator, \code{rlm} for MM-type estimator (robust).
#' @return character of linear model equation and R-square value
#' @details DETAILS
#' @examples 
#' data("data")
#' lm_eq(df = data, method = "rlm")
#' @rdname lm_eq
#' @export 
lm_eq <- function(df, method){
  
  if (method == "lm") {
    m <- lm(Sample.VWC ~ Sensor.VWC, df)
  }
  
  if (method == "rlm") {
    require(robustbase)
    m <- robustbase::lmrob(Sample.VWC ~ Sensor.VWC, df, setting = "KS2011", 
      maxit.scale = 1000)
  }
  
  if (coef(m)[2] < 0) {
    eq <- substitute(italic(y) == a  -  b %*% italic(x)*","~~italic(r)^2~"="~r2, 
                     list(a = as.numeric(format(coef(m)[1], digits = 2)), 
                          b = as.numeric(format(abs(coef(m))[2], digits = 2)), 
                          r2 = format(summary(m)$r.squared, digits = 3)))
  } else {
    eq <- substitute(italic(y) == a  +  b %*% italic(x)*","~~italic(r)^2~"="~r2, 
                     list(a = as.numeric(format(coef(m)[1], digits = 2)), 
                     b = as.numeric(format(abs(coef(m))[2], digits = 2)), 
    r2 = format(summary(m)$r.squared, digits = 3)))
  }
  
  return(as.character(as.expression(eq)))
  
}
