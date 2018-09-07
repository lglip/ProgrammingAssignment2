## The first function creates a specific object that caches
## the inverse of a matrix (and ultimately prints a list).

makeCacheMatrix <- function(x = matrix()) {
      cm <- NULL
      set <- function(y) {
            x <<- y
            cm <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) cm <<- solve
      getinverse <- function() cm
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}

## The second functions calculates the inverse of the
## resulting matrix of the makeCacheMatrix() function
## or retrieves the inverse from the cache if it has
## already been computed.

cacheSolve <- function(x, ...) {
      cm <- x$getinverse()
      if(!is.null(cm)) {
            message("getting cached data")
            return(cm)
      }
      matrx <- x$get()
      cm <- solve(matrx, ...)
      x$setinverse(cm)
      cm
}

## End of functions.