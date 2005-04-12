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
  setGeneric("nodes", function(object) standardGeneric("nodes"))
setMethod("nodes", signature(object="hypergraph"), function(object)
          object@nodes)



if (!isGeneric("numNodes"))
  setGeneric("numNodes", function(object) standardGeneric("numNodes"))
setMethod("numNodes", signature(object="hypergraph"),
          function(object) length(object@nodes))

if (!isGeneric("inciMat"))
  setGeneric("inciMat", function(.Object) standardGeneric("inciMat"))
setMethod("inciMat", signature(.Object="hypergraph"),
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

## Do we want toGraphNEL or as(hGraph, "graphNEL")
## The former has a more compact signature, but is perhaps less standard?
if (!isGeneric("toGraphNEL"))
  setGeneric("toGraphNEL", function(.Object) standardGeneric("toGraphNEL"))
setMethod("toGraphNEL", signature(.Object="hypergraph"),
          function(.Object) {
              hEdges <- hyperedges(.Object)
              hEdgeNames <- names(hEdges)
              if (is.null(hEdgeNames))
                hEdgeNames <- as.character(1:length(hEdges))
              if (any(hEdgeNames %in% nodes(.Object)))
                stop("hyperedge names must be distinct from node names")
              bpgNodes <- c(nodes(.Object), hEdgeNames)
              heEdgeL <- lapply(hEdges, function(x) {
                  list(edges=match(x, bpgNodes), weights=rep(1, length(x)))})
              names(heEdgeL) <- hEdgeNames
              hnEdgeL <- vector(mode="list", length=length(nodes(.Object)))
              names(hnEdgeL) <- nodes(.Object)
              for (i in 1:length(hEdges)) {
                  he = hEdges[[i]]
                  heNode <- hEdgeNames[i]
                  heNodeIndex <- which(heNode == bpgNodes)
                  for (n in he)
                    hnEdgeL[[n]] <- append(hnEdgeL[[n]], heNodeIndex)
              }
              hnEdgeL <- lapply(hnEdgeL, function(x) {
                  list(edges=x, weights=rep(1, length(x)))})
              bpgEdgeL <- c(heEdgeL, hnEdgeL)
              new("graphNEL", bpgNodes, bpgEdgeL)
          })

          

        
        

    


    
