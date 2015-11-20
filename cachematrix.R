## object to contain matrix and it's 
## inverse (if computed)

makeCacheMatrix <- function(actual = matrix()) {
  inverse <- NULL
  set <- function(x) {
    actual <<- x
    inverse <<- NULL
  }
  get <- function() {
    actual
  }

  setInverse <- function(i) {
    inverse <<- i
  }
  getInverse <- function() {
    inverse
  }
  list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## method to get inverse.  If not computed,
## it will stor it in cache.  If previously
## computed, it will use cache

cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()

  if (is.null(inverse)) {
    actual <- x$get()
    if (dim(actual)[1] == dim(actual)[2]) {
      inverse <- solve(actual)
      x$setInverse(inverse)
    }
  }
  inverse 
}
