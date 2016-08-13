# Functions used to calculate the inverse of a matrix, and store the result in
# an object as a means for caching the result of a potentially expensive
# calculation.
# 
# Call makeCacheMatrix(x) to create an object that stores the matrix (x) and has
# a place for storing the inverse. Call cacheSolve(cacheMatrix) to calculate the
# inverse of the matrix and store the result. If the inverse has already been
# calculated for a matrix, the cached value is returned instead of
# being recalculated.
#
# Note: The matrix is assumed to be invertible.
#
# Example:
#
# m <- matrix(c(2, 2, 3, 2), 2, 2)
# cm <- makeCacheMatrix(m)
# ## calculate the inverse of the matrix 
# cacheSolve(cm)
# ## return the cached inverse
# cacheSolve(cm)

#' Create an object that stores a matrix and it's inverse.
#'
#' @param x the matrix to store, and for which the inverse will be solved
#'
#' @return an object with attributes:
#'   set - replace the matrix
#'   get - get the matrix
#'   getInverse - get the inverse of the matrix, or NULL if not set yet
#'   setInverse - set the inverse of the matrix
#'
#' @examples
#' m <- matrix(c(2, 2, 3, 2), 2, 2)
#' cm <- makeCacheMatrix(m)
#' cm$get()
#' cm$getInverse()
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    getInverse <- function() inv
    setInverse <- function(inverse) inv <<- inverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

#' Solve for the inverse of a matrix, potentially using a cached result.
#' 
#' Calculate the inverse of a matrix stored in an object created using
#' makeCacheMatrix. The object will be used to cache the inverse of the matrix
#' so that it is only actually calculated the first time it is called.
#'
#' @param x an object created using makeCacheMatrix
#' @param ... additional parameters to be sent to the solve function
#'
#' @return inverse of the matrix
#' 
#' @seealso \code{\link{solve}}
#'
#' @examples
#' cm <- makeCacheMatrix(m)
#' cacheSolve(cm)
cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if (!is.null(inv)) {
        message("using cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}
