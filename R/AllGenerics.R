## FIXME: Is there a way to ask for the generic from package graph?  That's
## the one we want, if defined.
setGeneric("label", function(object) standardGeneric("label"))


setGeneric("label<-", function(object, value) standardGeneric("label<-"))


setGeneric("toUndirected", function(.Object) standardGeneric("toUndirected"))


setGeneric("head", function(.Object) standardGeneric("head"))


setGeneric("tail", function(.Object) standardGeneric("tail"))


setGeneric("hyperedges", function(.Object) standardGeneric("hyperedges"))


setGeneric("hyperedgeLabels",
           function(.Object) standardGeneric("hyperedgeLabels"))


setGeneric("inciMat", function(.Object) standardGeneric("inciMat"))


setGeneric("toGraphNEL", function(.Object) standardGeneric("toGraphNEL"))
