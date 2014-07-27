## Matrix inversion is usually a costly computation and their may be some 
## benefit to chaching the inverse of a matrix rather than compute it repeatedly.

## This function creates special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x=matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<-inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}

## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated(and the matrix has not changed), 
## then the chachesolve should retrive the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if ( ! is.null(m)) {
    print("getting cached data")
    return(m)
  }
  m <- solve(x$get())
  x$setInverse(m)
  m
}


