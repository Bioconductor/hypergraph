.onLoad <- function(lib, pkg) require("methods", quietly=TRUE)

setClass("Hyperedge", representation(head="character", label="character"))


setMethod("initialize", "Hyperedge",
          function(.Object, nodes, label="") {
              .Object@head <- as.character(nodes)
              .Object@label <- label
              .Object
          })
Hyperedge <- function(nodes, label="")
  new("Hyperedge", nodes=nodes, label=label)

l2hel <- function(e, labels) {
    numEdges <- length(e)
    if (missing(labels))
      labels <- as.character(1:numEdges)
    if (numEdges != length(labels))
      stop("e and labels must have the same length")
    hel <- vector(mode="list", length=numEdges)
    for (i in 1:numEdges)
        hel[[i]] <- Hyperedge(nodes=e[[i]], label=labels[i])
    hel
}

if (!isGeneric("nodes"))
  setGeneric("nodes", function(object) standardGeneric("nodes"))
setMethod("nodes", signature(object="Hyperedge"), function(object) object@head)


if (!isGeneric("label"))
  setGeneric("label", function(object) standardGeneric("label"))
setMethod("label", signature(object="Hyperedge"),
          function(object) object@label)


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


setMethod("nodes", signature(object="DirectedHyperedge"), function(object) {
    c(object@tail, object@head)
})


if (!isGeneric("toUndirected"))
  setGeneric("toUndirected", function(.Object) standardGeneric("toUndirected"))
setMethod("toUndirected", signature(.Object="DirectedHyperedge"),
          function(.Object) {
              new("Hyperedge", nodes=nodes(.Object), label=label(.Object))
          })
          

setGeneric("head", function(.Object) standardGeneric("head"))
setMethod("head", signature(.Object="DirectedHyperedge"),
          function(.Object) .Object@head)


setGeneric("tail", function(.Object) standardGeneric("tail"))
setMethod("tail", signature(.Object="DirectedHyperedge"),
          function(.Object) .Object@tail)


setMethod("show", "DirectedHyperedge", function(object) {
    callNextMethod()
    cat(length(tail(object)), "nodes in the tail and ")
    cat(length(head(object)), "nodes in head.\n")
})
    


setClass("Hypergraph", representation(nodes="character", hyperedges="list"))


setGeneric("hyperedges", function(.Object) standardGeneric("hyperedges"))
setMethod("hyperedges", signature(.Object="Hypergraph"),
          function(.Object) .Object@hyperedges)


## FIXME: How do I ask for the generic from the graph package.  Want to use it
## if available and otherwise define my own.  Same with numNodes
if (!isGeneric("nodes"))
  setGeneric("nodes", function(object) standardGeneric("nodes"))
setMethod("nodes", signature(object="Hypergraph"), function(object)
          object@nodes)


if (!isGeneric("numNodes"))
  setGeneric("numNodes", function(object) standardGeneric("numNodes"))
setMethod("numNodes", signature(object="Hypergraph"),
          function(object) length(object@nodes))


setGeneric("inciMat", function(.Object) standardGeneric("inciMat"))
setMethod("inciMat", signature(.Object="Hypergraph"),
          function(.Object) {
              nds <- nodes(.Object)
              hEdgeList <- hyperedges(.Object)
              createInciMat(nds, hEdgeList)
          })


createInciMat <- function(nodes, edgeList) {
    inciMat <- matrix(0, nrow=length(nodes), ncol=length(edgeList))
    for (j in 1:length(edgeList)) {
        col <- as.numeric(nodes %in% nodes(edgeList[[j]]))
        inciMat[, j] <- col
    }
    rownames(inciMat) <- nodes
    colnames(inciMat) <- sapply(edgeList, label)
    inciMat
}

checkValidHyperedges <- function(hyperedges, nodes) {
    goodHyperedges <- lapply(hyperedges, is, "Hyperedge")
    if (!all(goodHyperedges))
      stop("hyperedge list elements must be instances of the Hyperedge class.")
    hyperedgeSet <- unlist(lapply(hyperedges, nodes))
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

setMethod("initialize", "Hypergraph", function(.Object, nodes, hyperedges) {
    ## Create a new hypergraph instance.
    ##
    ##      nodes: character vector of node names
    ##
    ## hyperedges: a list of character vectors describing subsets of the nodes.
    ##
    .Object@nodes = nodes
    hypergraph:::checkValidHyperedges(hyperedges, nodes)
    .Object@hyperedges = hyperedges
    .Object
})
Hypergraph <- function(nodes, hyperedges) {
    ## Convenience function to create Hypergraph instances
    new("Hypergraph", nodes=nodes, hyperedges=hyperedges)
}


if (!isGeneric("toGraphNEL"))
  setGeneric("toGraphNEL", function(.Object) standardGeneric("toGraphNEL"))
setMethod("toGraphNEL", signature(.Object="Hypergraph"),
          function(.Object) {
              hEdges <- hyperedges(.Object)
              hEdgeNames <- names(hEdges)
              if (is.null(hEdgeNames))
                hEdgeNames <- as.character(1:length(hEdges))
              if (any(hEdgeNames %in% nodes(.Object)))
                stop("hyperedge names must be distinct from node names")
              bpgNodes <- c(nodes(.Object), hEdgeNames)
              heEdgeL <- lapply(hEdges, function(x) {
                  list(edges=match(nodes(x), bpgNodes), weights=rep(1, length(nodes(x))))})
              names(heEdgeL) <- hEdgeNames
              hnEdgeL <- vector(mode="list", length=length(nodes(.Object)))
              names(hnEdgeL) <- nodes(.Object)
              for (i in 1:length(hEdges)) {
                  he = hEdges[[i]]
                  heNode <- hEdgeNames[i]
                  heNodeIndex <- which(heNode == bpgNodes)
                  for (n in nodes(he))
                    hnEdgeL[[n]] <- append(hnEdgeL[[n]], heNodeIndex)
              }
              hnEdgeL <- lapply(hnEdgeL, function(x) {
                  list(edges=x, weights=rep(1, length(x)))})
              bpgEdgeL <- c(heEdgeL, hnEdgeL)
              new("graphNEL", bpgNodes, bpgEdgeL)
          })

          

        
        

    


    
