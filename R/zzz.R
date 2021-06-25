.onAttach = function(libname, pkgname) {
    version = packageDescription(pkgname, fields = "Version")

  	msg = paste0("========================================
", pkgname, " version ", version, "
Github page: https://github.com/jokergoo/spiralize

This message can be suppressed by:
  suppressPackageStartupMessages(library(spiralize))
========================================
")	

  	if("package:pheatmap" %in% search()) {
  		msg = paste0(msg, 
"! pheatmap() has been masked by ComplexHeatmap::pheatmap(). Most of the arguments
   in the original pheatmap() are identically supported in the new function. You 
   can still use the original function by explicitly calling pheatmap::pheatmap().
")
  	}

    packageStartupMessage(msg)
}
