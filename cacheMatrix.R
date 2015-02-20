## Together, the two functions calculate and cache the inverse of a matrix for efficient computation.
## This first function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  get <- function(){
    x
  }
  set_inverse <- function(inverse){
    inv <<- inverse
  }
  get_inverse <- function(){
    inv
  } 
  list(get=get,set_inverse=set_inverse,get_inverse=get_inverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inverse <- x$get_inverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$set_inverse(inverse)
  inverse
}