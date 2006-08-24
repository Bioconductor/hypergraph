\name{vCoverHypergraph}
\alias{vCoverHypergraph}
\title{Approximate minimum weight vertex cover in a hypergraph}
\description{Approximate minimum weight vertex cover in a hypergraph }
}
\usage{
vCoverHypergraph(g)
}
\arguments{
  \item{g}{an instance of the \code{Hypergraph} class }
}
\details{
TO BE IMPLEMENTED

The implementation is based on the algorithm by E. Ramadan, A. Tarafdar, A. Pothen, 2004.

}
\value{
TO BE IMPLEMENTED
}

\references{
A hypergraph model for the yeast protein complex network,  Ramadan, E.   Tarafdar, A.   Pothen, A., Parallel and Distributed Processing Symposium, 2004. Proceedings. 18th International.
}

\author{Li Long <li.long@isb-sib.ch>}

\seealso{}

\examples{
kcoex <- fromGXL(file(system.file("XML/snacoreex.gxl",package="RBGL")))
vCover(kcoex)

# to turn the above graph to a hypergraph
# this is a rough example 
kc_hg_n <- nodes(kcoex)
kc_hg_e <- list(c("A", "C"), c("B", "C"), c("C", "E"), c("C", "F"), c("E", "D"), c("E", "F"), c("D", "G"), c("D", "H"), c("D", "J"), c("H", "G"), c("H", "J"), c("G", "J"), c("J", "M"), c("J", "K"), c("M", "K"), c("M", "O"), c("M", "N"), c("K", "N"), c("K", "F"), c("K", "I"), c("K", "L"), c("F", "I"), c("I", "L"), c("F", "L"), c("P", "Q"), c("Q", "R"), c("Q", "S"), c("R", "T"), c("S", "T"))
kc_hg_he <- lapply(kc_hg_e, "Hyperedge")
kc_hg <- new("Hypergraph", nodes=kc_hg_n, hyperedges=kc_hg_he)

vCoverHypergraph(kc_hg)

}

\keyword{ models }
