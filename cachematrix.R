## These functions cache the inverse of a matrix, to avoid calculate repeatedly
## these inversions, that can be very costly to computer

## makeCacheMatrix() create a list containing four functions that are the setters 
## of the object matrix as input, the getter of this object; and the setters and
## getters of the inverse matrix that is calculated

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve() uses one object of the type makeCacheMatrix to calculate the
## inverse matrix (if it wasn't calculated yet) or to load the cache of the
## inverse matrix, if it was already calculated, avoidind high consumption of
## the computer

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i 
}
