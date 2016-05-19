## Matrix inversion is usually a costly computation 
## and are benefit to caching the inverse of a matrix 
## in stead of rather than computint it repeatedly. 
## In this assignment a short a pair of functions are written to cache 
## the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
        x <<- y
        m <<- NULL
      }
      get <- function() x
      setCachaMatrix <- function(solve) m <<- solve
      getCachaMatrix <- function() m
      list(set = set, 
           get = get,
           setCachaMatrix = setCachaMatrix,
           getCachaMatrix = getCachaMatrix)

}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  m <- x$getCachaMatrix()
  if(!is.null(m)) {
    message("getting cached inversed matrix")
    return(m)
  }
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setCachaMatrix(m)
  m
  
}
