#' defdMethods enumerates methods defined for a class in a package -- NEEDS WORK
#' @param cl character(1) name of class
#' @param pkg character(1) name of package where class is defined, defaults to 'package:[classname]' which works for major classes like SummarizedExperiment
#' @param verbose logical(1) if TRUE (default) will issue a message enumerating superclasses.
#' @note Users should consider how methods inherited from superclasses defined in
#' other packages are also relevant to reports of this type.  See the example.
#' @value a data.frame instance with columns 'method', 'cl', 'pkg',
#' 'nmeth4cl', counting the number of method-signature combinations for
#' this method, and 'sigs', a list of lists of named signature elements (each element is a list of character vectors of method parameter types, named with parameter names)
#' @examples
#' # start with simple tabulation of package-specific methods
#' suppressPackageStartupMessages({
#' require("SingleCellExperiment")
#' })
#' scem = defdMethods("SingleCellExperiment")
#' head(scem)
#' scem$method
#' scem["isSpike", "sigs"]
#' # now let's collect some related classes, with the objective of
#' # enumerating relevant coercion methods and their signatures
#' require("SummarizedExperiment")
#' se = defdMethods("SummarizedExperiment", verbose=FALSE)
#' rse = defdMethods("RangedSummarizedExperiment", "package:SummarizedExperiment", verbose=FALSE)
#' cmb = try(rbind(scem,se,rse))
#' require("dplyr")
#' require("magrittr")
#' tmp = (cmb %>% filter(method=="coerce"))
#' sl = (tmp %>% select(sigs))[[1]]
#' names(sl) = tmp$cl
#' sl
#' @export
defdMethods = function(cl, pkg=paste0("package:", cl), verbose=TRUE) {
 stopifnot(pkg %in% search())
 ## define helpers
 cleanF = function(st) {
   sub("Function: (.*) \\(package.*", "\\1", st)
 }
 getSig = function(st) {
   eval(parse(text=paste0("c(", st, ")")))
   }
 #ranstr = function() paste0(sample(letters,5), collapse="")
 ## end helpers
 z = textConnection("zz", open="w", local=TRUE) # sets up local variable zz
 showMethods(class=cl, where=pkg, printTo=z) # gets class metadata as defined in package
 nminds = grep("Function", zz) # defined function indices
 mnames = cleanF(zz[nminds]) # defined method(function) names
 smt = methods:::.showMethodsTable
 allsigs = lapply(mnames, function(x) {
       z = tempfile(); 
       con = file(z, "w")
       smt(getGeneric(x), classes=cl, printTo=con)
       on.exit({close(con); try(unlink(z))}); 
       ans = readLines(z)
       ans[-c(1,length(ans))]
      })
 ans = data.frame(method=mnames, cl=cl, pkg=pkg, nmeth4cl=sapply(allsigs,length), sigs=mnames,
   stringsAsFactors=FALSE)
 ans$sigs = allsigs
 supcl = names(getClass(cl)@contains)
 if (length(supcl)>0 & isTRUE(verbose)) {
    sups = paste0(supcl, collapse=", ")
    message(paste("superclasses", sups, "may provide additional relevant methods/signatures"))
    }
 ans
}

