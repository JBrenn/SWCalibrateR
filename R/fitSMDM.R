fitSMDM <- function(formula, data, ...) {
  lmrob(formula, data, setting = "KS2011",
        maxit.scale = 1000)
}