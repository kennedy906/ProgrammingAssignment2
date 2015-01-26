## This exercise demonstrates the use of lexical scoping rules to pass
##   information between different environments: in this case, storing
##   the inverse of a matrix and recalling it from the cache later

## This abstract object stores the matrix itself along with the computed
##   value of the inverted matrix (so that it can be recalled later)

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function receives the object created by the makeCacheMatrix function
##   and reports the inverse, if already computed, or computes it if not.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
