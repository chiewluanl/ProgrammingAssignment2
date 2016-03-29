## Put comments here that give an overall description of what your
## functions do

## Create a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  mm <- x$getInverse()
  if(!is.null(mm)) {
    message("getting cached data")
    return(mm)
  }
  data <- x$get()
  mm <- solve(data, ...)
  x$setInverse(mm)
  mm
}
