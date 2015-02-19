## The following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL     # inv will store the cached inverse matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve, computes the inverse of the matrix. If the inverse has already been calculated and the matrix remained unchanged, then cachesolve will return the cached inverse.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()    
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}


Example:
# x <- matrix(rnorm(4), nrow = 2)
# ex <- makeCacheMatrix(x)
# ex$get()         ## Produce the matrix
# cacheSolve(ex)   ## Return the inverse of the matrix
# cacheSolve(ex)   ## A second time run, will return the cache inverse