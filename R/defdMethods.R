#' defdMethods enumerates methods defined for a class in a package -- NEEDS WORK
#' @param cl character(1) name of class
#' @param pkg character(1) name of package where class is defined, defaults to 'package:[classname]' which works for major classes like SummarizedExperiment
#' @note This is an early attack on the problem of enumerating
#' defined methods for a class.  As of 5 Jan 2019 this function
#' lists only one signature per method and fails on oddly
#' names signature components like '_data'.
#' @value an S4Vectors::DataFrame instance with columns 'mname' for method name, and 'sigs', a CharacterList of named signature elements (each element is a character vector of method parameter types, named with parameter names)
#' @examples
#' require("SingleCellExperiment")
#' scem = defdMethods("SingleCellExperiment")
#' scem
#' scem[[2]][[1]]
#' @export
defdMethods = function(cl, pkg=paste0("package:", cl)) {
 stopifnot(pkg %in% search())
 ## define helpers
 cleanF = function(st) {
   sub("Function: (.*) \\(package.*", "\\1", st)
 }
 getSig = function(st) {
   eval(parse(text=paste0("c(", st, ")")))
   }
 ranstr = function(x) paste0(sample(letters,5), collapse="")
 ## end helpers
 #zz = ranstr()
 z = textConnection("zz", open="w", local=TRUE) # sets up local variable zz
 showMethods(class=cl, where=pkg, printTo=z) # gets class metadata as defined in package
 nminds = grep("Function", zz) # defined function indices
 mnames = cleanF(zz[nminds]) # defined method(function) names
#
# FIXME: this assumes one signature per method, completely wrong
# Need to find number of sigs per method and operate accordingly
#
 sigs = lapply(zz[nminds+1], function(x) try(getSig(x))) # defined signatures
 #list(mnames, sigs)
 message("there are probably more signatures per method than listed here")
 message("see FIXME in source")
 S4Vectors::DataFrame(mnames=mnames, sigs=S4Vectors::SimpleList(sigs))
}

