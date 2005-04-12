simpleHypergraph <- function() {
    nodes <- LETTERS[1:4]
    hEdges <- lapply(c("A", LETTERS[1:2], LETTERS[3:4]), "Hyperedge")
    hg <- new("hypergraph", nodes=nodes, hyperedges=hEdges)
}    


testConstruction <- function() {
    hg <- simpleHypergraph()
}


testDirectedHypergraph <- function() {
    nodes <- letters[1:4]
    dhe1 <- DirectedHyperedge(tail=c("a", "b"), head=c("c", "d"))
    dhe2 <- DirectedHyperedge(tail=c("a"), head=c("b", "c", "d"))
    dhe3 <- DirectedHyperedge(tail=c("b", "c"), head=c("d"))
    dhe4 <- DirectedHyperedge(tail=c("a"), head=c("b"))
    hg <- new("hypergraph", nodes=nodes,
              hyperedges=list(dhe1, dhe2, dhe3, dhe4))
}
 

testHyperedges <- function() {
    nodes <- LETTERS[1:4]
    hEdges <- lapply(c("A", LETTERS[1:2], LETTERS[3:4]), "Hyperedge")
    hg <- new("hypergraph", nodes=nodes, hyperedges=hEdges)
    checkEquals(hEdges, hyperedges(hg))
}


testNodes <- function() {
    nodes <- LETTERS[1:4]
    hEdges <- lapply(c("A", LETTERS[1:2], LETTERS[3:4]), "Hyperedge")
    hg <- new("hypergraph", nodes=nodes, hyperedges=hEdges)
    checkEquals(nodes, nodes(hg))
}


testBadHyperedges <- function() {
    nodes <- LETTERS[1:4]
    hyperedges <- list(matrix(0, nrow=2, ncol=2))
    checkException(new("hypergraph", nodes=nodes, hyperedges=hyperedges))

    hyperedges <- lapply(c(1:2, 1:3), "Hyperedge")
    checkException(new("hypergraph", nodes=nodes, hyperedges=hyperedges))
    
    hyperedges <- lapply(c("A", c("A", "B"), c("C", "Z"), c("Q", "R", "S")), "Hyperedge")
    checkException(new("hypergraph", nodes=nodes, hyperedges=hyperedges))
    
}


testNumNodes <- function() {
    nodes <- letters[1:10]
    hyperedges <- list(Hyperedge("a"))
    hg <- new("hypergraph", nodes=nodes, hyperedges=hyperedges)
    checkEquals(10, numNodes(hg))
}

testInciMat <- function() {
    nodes <- letters[1:4]
    hEdges <- list(c("a", "b"),
                   c("b", "c"),
                   c("c", "d", "a"))
    hg <- new("hypergraph", nodes=nodes, hyperedges=hEdges)
    mat <- inciMat(hg)
    expected <- cbind(c(1, 1, 0, 0),
                      c(0, 1, 1, 0),
                      c(1, 0, 1, 1))
    checkEquals(expected, mat)
}

testToGraphNEL <- function() {
    TRUE
}
