.onLoad <- function(lib, pkg) require("methods", quietly=TRUE)


setClass("Hyperedge", representation(head="character", label="character"))


setClass("DirectedHyperedge", representation(tail="character"),
         contains="Hyperedge")


setClass("Hypergraph", representation(nodes="character", hyperedges="list"))

