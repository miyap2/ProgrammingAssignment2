## Matrix inversion is usually a costly computation and there may be some benefits to caching
## the inverse of a matrix rather than computing it repeatedly.
## The functions below cache the inverse of a matrix.

## Function makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
   invm <- NULL
   
   set <- function(y) {
      x <<- y
      invm <<- NULL
   }
   
   get <- function() x
   
   setInverse <- function(inverse) invm <<- inverse
   
   getInverse <- function() invm
   
   list(set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}


## Function cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then  
## cacheSolve  should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x'
   invm <- x$getInverse()
   
   if(!is.null(invm)) {
      message("Getting cached inverse matrix data.")
      return(invm)
   }
   data <- x$get()
   invm <- solve(data, ...)
   x$setInverse(invm)
   invm
}
