.onAttach <- function(libname, pkgname) {
  # TODO (JBrenn): adjust meaassage, add package explainations and examples
  packageStartupMessage("Welcome to SMCcalibration package")
}

# Global options ----------
.onLoad <- function(libname, pkgname) {
  op <- options()
  op.kostr <- list(
    kostr.path = "~/R-SMCcalibration",
    kostr.install.args = "",
    kostr.desc.suggests = NULL,
    kostr.desc = list()
  )
  toset <- !(names(op.kostr) %in% names(op))
  if(any(toset)) options(op.kostr[toset])
  
  invisible()
}