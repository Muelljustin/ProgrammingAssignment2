## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
      invmat <- NULL
      set <- function(y) {
          x <<- y
          inv <<- NULL
      }
      get <- function() x
      setInverse <- function(inverse) invmat <<- inverse
      getInverse <- function() invmat
      list(set = set,
           get = get,
           setInverse = setInverse,
           getInverse = getInverse)
        
}


## This Function complies the inverse of the makeCacheMatrix written above. 
##As long as the matrix has not been changed, the function will pull the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      invmat <- x$getInverse()
      if (!is.null(invmat)) {
              message("Gathering cached data")
              return(invmat)
      }
      mat <- x$get()
      invmat <- solve(mat, ...)
      x$setInverse(invmat)
      invmat
}
