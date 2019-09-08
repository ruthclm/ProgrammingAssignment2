## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly.
## The following functions creates a special "matrix" object that can cache its inverse
## and computes the inverse of the special "matrix" returned by makeCacheMatrix above

## This makeCacheMatrix function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  
}

## This cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  m <- x$getInverse()
  if (!is.null(m)) {
    message("getting inverse matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
  
}
