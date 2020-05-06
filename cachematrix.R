## These functions cache the inverse of the matrix by creating a special matrix which 
## can cache its inverse and then compute the inverse of this special matrix.


## This function creates a special matrix that will be used to cache its inverse in the cacheSolve function later on.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
 }
  get <- function() x
  setInverse <- function(solveMatrix) i <<- solveMatrix
  getInverse <- function() i
  list(set = set, 
       get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## This function determines the inverse of the special matrix returned from makeCacheMatrix.
## If the inverse has already been calculated, then the cachesolve should retrieve the inverse from the cache without needing to compute it again.

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}
