## 'makeCacheMatrix' is first called on a square matrix to build a list of
## caching functions around it. 'cacheSolve' is then called on that list where
## inverse of the matrix will be calculated only when the matrix value changes
## setting a cached value otherwise

## This function encapsulates a square matrix to provide caching functionality

makeCacheMatrix <- function(x = matrix()) {

  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setSolve <- function(solve) s <<- solve
  getSolve <- function() s
  list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)
  
}


## This function recieves a 'makeCacheMatrix' and returns the inverse of the
## matrix; either from the cache or by actually solving for it on the first call

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  s <- x$getSolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setSolve(s)
  s
}
