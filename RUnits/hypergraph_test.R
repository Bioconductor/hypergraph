simpleHypergraph <- function() {
    nodes <- LETTERS[1:4]
    hEdges <- list("A", LETTERS[1:2], LETTERS[3:4])
    hg <- new("hypergraph", nodes=nodes, hyperedges=hEdges)
}    


testConstruction <- function() {
    hg <- simpleHypergraph()
}


testHyperedges <- function() {
    nodes <- LETTERS[1:4]
    hEdges <- list("A", LETTERS[1:2], LETTERS[3:4])
    hg <- new("hypergraph", nodes=nodes, hyperedges=hEdges)
    checkEquals(hEdges, hyperedges(hg))
}


testNodes <- function() {
    nodes <- LETTERS[1:4]
    hEdges <- list("A", LETTERS[1:2], LETTERS[3:4])
    hg <- new("hypergraph", nodes=nodes, hyperedges=hEdges)
    checkEquals(nodes, nodes(hg))
}


testBadHyperedges <- function() {
    nodes <- LETTERS[1:4]
    hyperedges <- list(matrix(0, nrow=2, ncol=2))
    checkException(new("hypergraph", nodes=nodes, hyperedges=hyperedges))

    hyperedges <- list(1:2, 1:3)
    checkException(new("hypergraph", nodes=nodes, hyperedges=hyperedges))
    
    hyperedges <- list("A", c("A", "B"), c("C", "Z"), c("Q", "R", "S"))
    checkException(new("hypergraph", nodes=nodes, hyperedges=hyperedges))
    
}

testNumNodes <- function() {
    nodes <- letters[1:10]
    hyperedges <- list("a")
    hg <- new("hypergraph", nodes=nodes, hyperedges=hyperedges)
    checkEquals(10, numNodes(hg))
}
