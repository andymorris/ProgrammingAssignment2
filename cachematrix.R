## These functions implement the creation of a 'special' matrix whose inverse
## is cacheable, and the ability to get the inverse of such a matrix (from
## the cache if available). This can greatly speed up code that computes the
## inverse of (unchanged) matrices often.

## Create a matrix whose inverse is cacheable.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(i) inverse <<- i
  getInverse <- function() inverse
  list(set = set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Get the inverse of a matrix created by makeCacheMatrix. If the inverse has
## not been computed, it will be computed and cached. If it has already been
## computed, the inverse will be returned from the cache.

cacheSolve <- function(x, ...) {
  cached <- x$getInverse()
  if (!is.null(cached)) {
    cached
  } else {
    inverse <- solve(x$get(), ...)
    x$setInverse(inverse)
    inverse
  }
}
