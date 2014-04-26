## This finds the inverse of the matrix and caches it in order to save some performance since inverse is a heavy function.
## The first part here, has getters and setters.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(invers) inv <<- invers
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## This find the inverse of a matrix if it has not already been found in previous runs.Works only for invertible marices.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
}
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}
