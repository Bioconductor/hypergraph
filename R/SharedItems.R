## TODO:
## - documentation
## - a proper item set hash?
## - vectorizing intersection


setClass("SharedItemSet", representation(items="numeric", categories="numeric"))
SharedItemSet <- function(items, categories) {
    new("SharedItemSet", items=items, categories=categories)
}


## items accessors
setGeneric("items", function(object) standardGeneric("items"))
setGeneric("items<-", function(object, value) standardGeneric("items<-"))
setMethod("items", "SharedItemSet", function(object) object@items)
setReplaceMethod("items", "SharedItemSet", function(object, value) {
    object@items <- value
    object
})


## categories accessors
setGeneric("categories", function(object) standardGeneric("categories"))
setGeneric("categories<-", function(object, value) standardGeneric("categories<-"))
setMethod("categories", "SharedItemSet", function(object) object@categories)
setReplaceMethod("categories",
                 signature(object="SharedItemSet", value="list"),
                 function(object, value) {
                     object@categories <- value
                     object
                 })


setClass("CategoryData", representation(data="list"))


setMethod("[", signature(x="CategoryData", i="numeric", j="missing", drop="missing"),
          function(x, i, j, drop) {
              if (length(x) == 1)
                return(x@data[[i]])
              subList <- x@data[i]
              names(subList) <- NULL
              cats <- do.call("c", subList)
              catNames <- unique(names(cats))
              cats <- unique(cats)
              names(cats) <- catNames
              cats
          })
              
              

setMethod("length", signature(x="CategoryData"), function(x) length(x@data))


setClass("CategoryMatrix", representation(incidenceMatrix="matrix",
                                          categoryData="CategoryData"))

setMethod("categories", signature(object="CategoryMatrix"),
          function(object) object@categoryData)

setMethod("dim", signature(x="CategoryMatrix"),
          function(x) dim(x@incidenceMatrix))

setMethod("dimnames", signature(x="CategoryMatrix"),
          function(x) dimnames(x@incidenceMatrix))


setGeneric("membershipMat", function(object) standardGeneric("membershipMat"))

setMethod("membershipMat", signature(object="CategoryMatrix"),
          function(object) object@incidenceMatrix)

setReplaceMethod("categories",
                 signature(object="CategoryMatrix", value="CategoryData"),
                 function(object, value) object@categoryData <- value)

                 
setMethod("[", signature(x="CategoryMatrix", i="numeric", j="numeric"),
          function(x, i, j, drop) x@incidenceMatrix[i, j])
setMethod("[", signature(x="CategoryMatrix", i="missing", j="numeric"),
          function(x, i, j, drop) x@incidenceMatrix[, j])
setMethod("[", signature(x="CategoryMatrix", i="numeric", j="missing"),
          function(x, i, j, drop) x@incidenceMatrix[i,])

setMethod("initialize",
          signature(.Object="CategoryMatrix"),
          function(.Object, data, categories) {
              if (missing(categories)) {
                  categories <- list()
                  for (i in 1:ncol(data)) {
                      v <- i
                      names(v) <- colnames(data)[i]
                      categories[[i]] <- v
                  }
              }
              catData <- new("CategoryData", data=categories)
              .Object@incidenceMatrix <- data
              .Object@categoryData <- catData
              .Object
          })
CategoryMatrix <- function(data, ...) {
  new("CategoryMatrix", data=data, ...)
}


setGeneric("findSharedItemSets", function(data, minSize, ...)
           standardGeneric("findSharedItemSets"))

setGeneric("findSharedItemSet", function(data, items, candidates)
           standardGeneric("findSharedItemSet"))


setMethod("findSharedItemSets",
          signature(data="CategoryMatrix", minSize="numeric"),
          function(data, minSize, itemSetCache) {
              i2cMat <- membershipMat(data)
              sharedCounts <- t(i2cMat) %*% i2cMat
              numComplexes <- ncol(i2cMat)
              if (missing(itemSetCache))
                globalItemSetCache <- new.env(hash=TRUE, parent=NULL)
              else
                globalItemSetCache <- itemSetCache
              localItemSetCache <- new.env(hash=TRUE, parent=NULL)
              alreadyFound <- function(key) {
                  exists(key, localItemSetCache) ||
                  exists(key, globalItemSetCache)
              }
              for (i in 1:numComplexes) {
                  for (j in 2:numComplexes) {
                      if (i >= j) next 
                      if (sharedCounts[i, j] < minSize) next 
                      items <- categoryIntersect(data[, i], data[, j])
                      sharedKey <- stringifyIndices(items)
                      if (! alreadyFound(sharedKey)) {
                          possibleCategories <- candidateCategories(i, sharedCounts,
                                                                    length(structure))
                          shared <- findSharedItemSet(data, items, possibleCategories)
                          localItemSetCache <- storeSharedItemSet(shared, localItemSetCache)
                      } 
                  }
              }
              ## compute CategoryMatrix 
              if (length(ls(localItemSetCache)) >= 2) {
                  rowNames <- rownames(data)
                  cm <- sharedItemSetsHash2CategoryMatrix(localItemSetCache,
                                                          rowNames)
              } else {
                  ## Only one SharedItemSet found, we are done
                  cm <- NULL
              }
              ## merge localItemSetCache with globalItemSetCache
              keys <- ls(localItemSetCache)
              for (key in keys)
                globalItemSetCache[[key]] <- localItemSetCache[[key]]
              list(itemSetCache=globalItemSetCache, categoryMatrix=cm)
          })


categoryIntersect <- function(a, b) {
    ## Return indices of proteins shared by complexA and complexB.
    ## TODO: make this vectorized so you can
    ## categoryIntersect(a, b, c, d)
    which(a & b)
}


candidateCategories <- function(categoryPrototype, sharedCountMat, size) {
    ## Return indices of the complexes that share at least size
    ## proteins with the complex specified by categoryPrototype
    which(sharedCountMat[categoryPrototype, ] >= size)
}


setMethod("findSharedItemSet",
          signature(data="CategoryMatrix", items="numeric",
                    candidates="numeric"),
          function(data, items, candidates) {
              ## Return indices of categories that contain the specified items
              ## (a vector of item indices).
              ## candidates specifies column indices to restrict search
              hasItemSet <- function(x) {
                  all(x[items] == 1)
              }
              found <- which(apply(data[ , candidates], 2, hasItemSet))
              complexIds <- candidates[found]
              complexes <- categories(data)[complexIds]
              SharedItemSet(items, complexes)
          })


stringifyIndices <- function(x) {
    ## Create a string from a vector x that can be used as the key in
    ## an environment.  This is currently a **very** naive approach.
    ## A proper hash function would be more appropriate, but for now I
    ## don't want to introduce a dependency on the digest package.
    key <- paste(x, collapse="-")
    if (nchar(key) > 255)
      warning("String-ification resulted in key with ",
              nchar(key), " character, which is probably too long.")
    key
}


setGeneric("storeSharedItemSet", function(itemSet, cache)
           standardGeneric("storeSharedItemSet"))

setMethod("storeSharedItemSet", signature(itemSet="SharedItemSet",
                                          cache="environment"),
          function(itemSet, cache) {
              ## Store itemSet in the given cache.
              ##
              ## The structure is stored in a key generated by stringifying the vector of
              ## protein indices that define the structure.
              ##
              itemStr <- stringifyIndices(items(itemSet))
              cache[[itemStr]] <- itemSet
              cache
          })
           


sharedItemSetsHash2CategoryMatrix <- function(itemSetHash, rowNames) {
    colList <- eapply(itemSetHash, function(x) {
        thisCol <- numeric(length(rowNames))
        thisCol[items(x)] <- 1
        thisCol
    })
    inciMat <- do.call("cbind", colList)
    rownames(inciMat) <- rowNames

    categoryList <- eapply(itemSetHash, function(x) {
        categories(x)
    })
    CategoryMatrix(inciMat, categoryList)
}
    
