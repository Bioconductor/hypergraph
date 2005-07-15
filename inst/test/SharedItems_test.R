getP2CMat <- function() {
    ## Create a fictional protein <--> complex matrix by hand
    c1 <- c(1, 1, 0, 0, 0, 1, 1)
    c2 <- c(0, 1, 0, 0, 0, 1, 1)
    c3 <- c(1, 1, 0, 1, 1, 0, 0)
    c4 <- c(0, 1, 0, 1, 0, 1, 0)
    c5 <- c(0, 0, 0, 0, 0, 0, 1)
    p2c <- cbind(c1, c2, c3, c4)
    rownames(p2c) <- paste("p", 1:nrow(p2c), sep="")
    colnames(p2c) <- paste("C", 1:ncol(p2c), sep="")
    p2c
}

testFindSharedProteinStructuresEmpty <- function() {
    p2c <- getP2CMat()
    shared <- findSharedItemSets(p2c, 5)
    checkEquals(character(0), ls(shared))
}


testFindSharedProteinStructures <- function() {
    p2c <- getP2CMat()
    shared <- findSharedItemSets(p2c, 3)
    checkEquals(1, length(ls(shared)))

    shared <- findSharedItemSets(p2c, 2)                
    checkEquals(c(1, 2), categories(shared[["2-6-7"]]))
    checkEquals(c(2, 6, 7), items(shared[["2-6-7"]]))

    structures <- c("1-2", "2-4", "2-6", "2-6-7")
    checkEquals(structures, ls(shared))

}

    
