#' class definition support, automates creation of getters and setters
#' @export
setClass("ClassSupport", slots=c(clname="character", defcode="character",
  getters="list", setters="list", protolist="list"))

setMethod("show", "ClassSupport", function(object) {
 cat(sprintf("ClassSupport instance for %s", slot(object, "clname")), "\n")
 cat(" slots: ", names(slot(object, "protolist")), "\n")
})

#' output definition, getters, setters
#' @export
setGeneric("dumpcs", function(csupp, con=stdout()) standardGeneric("dumpcs"))

#' output definition, getters, setters
#' @export
setMethod("dumpcs", c("ClassSupport"), function(csupp, con=stdout()) {
 code = unlist(c(slot(csupp, "defcode"), slot(csupp, "getters"),
         slot(csupp, "setters")))
 writeLines(code, con)
})

# helper function for building the representation string
repstr = function(LL) paste(names(LL), sQuote(sapply(sapply(LL, class), 
   "[", 1)), sep="=", collapse=", ")

#' write the code for an S4 class with setters and getters
#' @param clname character(1) class name
#' @param protolist named list() of prototypical values for slots
#' @param check_init logical(1) if TRUE, try to construct instance with prototype before building code
#' @note The example is inspired by [Stuart Lee's blog post](https://stuartlee.org/2019/07/09/s4-short-guide/)
#' @examples
#' dm = clgen()
#' dm
#' dumpcs(dm)
#' @export
clgen = function(clname="Turtle", 
    protolist = list(location=c(0,0), orientation=0, path=matrix(c(0,0), ncol=2)),
    check_init = FALSE) {
 rep = do.call("representation", lapply(lapply(protolist, class), "[", 1))
 slo = sapply(lapply(protolist, class), "[", 1)
 if (check_init) {
   tmpe = new.env()
   cl = setClass(clname, slots=slo, environment=tmpe)
   clcode = c(clname, protolist)
   ob = try(do.call("new", clcode)) #list(clname)))
   if (inherits(ob, "try-error")) stop("could not run 'new'")
   removeClass(cl, environment=tmpe) # to proceed
   }
 rrep = paste0("c(", repstr(protolist), ")")
 pro = {
   fn = basename(tempfile())
   tt = textConnection(fn, "w", local=TRUE)
   sink(tt)
   dput(protolist)
   sink(NULL)
   ans = get(fn)
   close(tt)
   rm(tt)
   paste(ans, collapse="")
   }
 cldef = sprintf("setClass(%s, slots= %s, prototype=%s)", sQuote(clname), rrep, pro)
 getter_code = lapply(names(protolist), function(s) make_getter(clname, s))
 setter_code = lapply(names(protolist), function(s) make_setter(clname, s))
 new("ClassSupport", clname=clname, defcode=cldef, getters=getter_code, setters=setter_code,
    protolist=protolist)
}
 
make_getter = function(cl, slot) {
 g = sprintf("setGeneric(name=%s, def=function(x) standardGeneric(%s))", sQuote(slot), sQuote(slot))
 m = sprintf("setMethod(f=%s, signature=%s, function(x) slot(x, %s))", sQuote(slot), 
               sQuote(cl), sQuote(slot))
 list(g=g, m=m)
}

make_setter = function(cl, slot) {
 sarr = paste0(slot, "<-")
 g = sprintf("setGeneric(name=%s, def=function(x, value) standardGeneric(%s))", sQuote(sarr), sQuote(sarr))
 m = sprintf("setMethod(f=%s, signature=%s, function(x, value) {slot(x, %s) = value; x})", sQuote(sarr), 
               sQuote(cl), sQuote(slot))
 list(g=g, m=m)
}

#' support extension of a class support instance to support a new class defined by a name and prototype list
#' @export
setGeneric("extendClassSupport",  function(oldcls, extras, newcl)
   standardGeneric("extendClassSupport"))

#' support extension of a class support instance to support a new class defined by a name and prototype list
#' @export
setMethod("extendClassSupport", c("ClassSupport", "list", "character"),
   function(oldcls, extras, newcl) {
     newsl = sapply(sapply(extras, class), "[", 1) # DEBT - multiple inh not addressed
     newdef = sprintf("setClass(%s, contains=%s, slots=%s)",
               sQuote(newcl), sQuote(slot(oldcls, "clname")), paste0("c(", repstr(extras), ")"))
     getter_code = lapply(names(extras), function(s) make_getter(newcl, s))
     setter_code = lapply(names(extras), function(s) make_setter(newcl, s))
     new("ClassSupport", clname=newcl, defcode=newdef, getters=getter_code, setters=setter_code,
         protolist=extras)
   })

