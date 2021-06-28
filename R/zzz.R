.onAttach = function(libname, pkgname) {
    version = packageDescription(pkgname, fields = "Version")

  	msg = paste0("========================================
", pkgname, " version ", version, "
Github page: https://github.com/jokergoo/spiralize

This message can be suppressed by:
  suppressPackageStartupMessages(library(spiralize))
========================================
")	

    packageStartupMessage(msg)
}
