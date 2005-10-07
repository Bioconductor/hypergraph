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



setMethod("hyperedges", signature(.Object="Hypergraph"),
          function(.Object) .Object@hyperedges)


setMethod("nodes", signature(object="Hypergraph"), function(object)
          object@nodes)


setMethod("numNodes", signature(object="Hypergraph"),
          function(object) length(object@nodes))


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