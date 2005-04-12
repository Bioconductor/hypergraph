setClass("Hyperedge", representation(head="character", label="character"))


setMethod("initialize", "Hyperedge",
          function(.Object, nodes, label="") {
              .Object@head <- as.character(nodes)
              .Object@label <- label
              .Object
          })
Hyperedge <- function(nodes, label="")
  new("Hyperedge", nodes=nodes, label=label)


if (!isGeneric("nodes"))
  setGeneric("nodes", function(.Object) standardGeneric("nodes"))
setMethod("nodes", signature(.Object="Hyperedge"), function(.Object) .Object@head)


if (!isGeneric("label"))
  setGeneric("label", function(.Object) standardGeneric("label"))
setMethod("label", signature(.Object="Hyperedge"),
          function(.Object) .Object@label)


setMethod("show", signature(object="Hyperedge"),
          function(object) {
              cat(paste("A", class(object)[1]), "containing",
                  length(nodes(object)), "nodes.\n")
          })



setClass("DirectedHyperedge", representation(tail="character"),
         contains="Hyperedge")


setMethod("initialize", "DirectedHyperedge",
          function(.Object, head, tail, label="") {
              .Object@label <- label
              .Object@head <- as.character(head)
              .Object@tail <- as.character(tail)
              .Object
          })
DirectedHyperedge <- function(head, tail, label="")
  new("DirectedHyperedge", head=head, tail=tail, label=label)


setMethod("nodes", signature(.Object="DirectedHyperedge"), function(.Object) {
    c(.Object@tail, .Object@head)
})


if (!isGeneric("toUndirected"))
  setGeneric("toUndirected", function(.Object) standardGeneric("toUndirected"))
setMethod("toUndirected", signature(.Object="DirectedHyperedge"),
          function(.Object) {
              new("Hyperedge", nodes=nodes(.Object), label=label(.Object))
          })
          

## XXX: YUCK! I'm not grocking the generic function thing.  head, tail are
## defined as generics returning the first "chunk" of vectors and data frames.
## So I have to match the arg names and "..." even though I want nothing to do
## with them!
if (!isGeneric("head"))
  setGeneric("head", function(x, ...) standardGeneric("head"))
setMethod("head", signature(x="DirectedHyperedge"),
          function(x, ...) x@head)


if (!isGeneric("tail"))
  setGeneric("tail", function(x, ...) standardGeneric("tail"))
setMethod("tail", signature(x="DirectedHyperedge"),
          function(x, ...) x@tail)


setMethod("show", "DirectedHyperedge", function(object) {
    callNextMethod()
    cat(length(tail(object)), "nodes in the tail and ")
    cat(length(head(object)), "nodes in head.\n")
})
    


setClass("hypergraph", representation(nodes="character", hyperedges="list"))


if (!isGeneric("hyperedges"))
  setGeneric("hyperedges", function(.Object) standardGeneric("hyperedges"))
setMethod("hyperedges", signature(.Object="hypergraph"),
          function(.Object) .Object@hyperedges)


if (!isGeneric("nodes"))
  setGeneric("nodes", function(.Object) standardGeneric("nodes"))
setMethod("nodes", signature(.Object="hypergraph"), function(.Object) .Object@nodes)


if (!isGeneric("numNodes"))
  setGeneric("numNodes", function(.Object) standardGeneric("numNodes"))
setMethod("numNodes", signature(.Object="hypergraph"),
          function(.Object) length(.Object@nodes))

         
createInciMat <- function(nodes, edgeList) {
    inciMat <- matrix(0, nrow=length(nodes), ncol=length(edgeList))
    for (j in 1:length(edgeList)) {
        col <- as.numeric(nodes %in% edgeList[[j]])
        inciMat[, j] <- col
    }
    rownames(inciMat) <- nodes
    inciMat
}


setMethod("initialize", "hypergraph", function(.Object, nodes, hyperedges) {
    ## Create a new hypergraph instance.
    ##
    ##      nodes: character vector of node names
    ##
    ## hyperedges: a list of character vectors describing subsets of the nodes.
    ##
    .Object@nodes = nodes
    checkValidHyperedges(hyperedges, nodes)
    .Object@hyperedges = hyperedges
    .Object
})

checkValidHyperedges <- function(hyperedges, nodes) {
    
    goodHyperedges <- lapply(hyperedges, is, "Hyperedge")
    if (!all(goodHyperedges))
      stop("hyperedge list elements must be instances of the Hyperedge class.")
    hyperedgeSet <- sapply(hyperedges, nodes)
    unknownNodes <- !(hyperedgeSet %in% nodes)
    if (any(unknownNodes)) {
        unknownNodes <- hyperedgeSet[unknownNodes]
        msg <- paste("The hyperedge list is not valid because it",
                     "specifies nodes not in the node vector:")
        msg <- paste(msg, paste(dQuote(unknownNodes), collapse=" "), sep="\n")
        stop(msg)
    }
    TRUE
}

