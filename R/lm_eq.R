lm_eqn <- function(df, method){
  
  if (method == "lm") {
    m <- lm(meansample ~ meanstation, df)
  }
  
  if (method == "rlm") {
    require(robustbase)
    m <- lmrob(meansample ~ meanstation, df, setting = "KS2011", maxit.scale = 1000)
  }
  
  if (coef(m)[2] < 0) {
    eq <- substitute(italic(y) == a  -  b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                     list(a = base::format(stats::coef(as.numeric(m))[1], digits = 2), 
                          b = base::format(base::abs(stats::coef(as.numeric(m))[2]), digits = 2), 
                          r2 = base::format(summary(m)$r.squared, digits = 3)))
  } else {
    eq <- substitute(italic(y) == a  +  b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                     list(a = base::format(stats::coef(as.numeric(m))[1], digits = 2), 
                          b = base::format(base::abs(stats::coef(as.numeric(m))[2]), digits = 2), 
                          r2 = base::format(summary(m)$r.squared, digits = 3)))
  }
  
  return(as.character(as.expression(eq)))   
  
}
