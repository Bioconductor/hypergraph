setClass("hypergraph", representation(nodes="character", hyperedges="list"),
         validity=function(.Object) validHypergraph(.Object))


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
    .Object@hyperedges = lapply(hyperedges, as.character)
    .Object
})

checkValidHyperedges <- function(hyperedges, nodes) {
    validHyperedge <- function(edge)
      is.vector(edge) && is.character(edge)
    
    goodHyperedges <- lapply(hyperedges, validHyperedge)
    if (!all(goodHyperedges))
      stop("hyperedge list elements must be character vectors")
    
    hyperedgeSet <- unlist(hyperedges)
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

