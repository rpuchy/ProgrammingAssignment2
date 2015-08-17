## The functions contained in this script provide a method to 
## calculate and cache inverse matrices

## This function creates a list of functions to set - sets the raw matrix, get -  returns the raw matrix, 
## setinverse - sets the cache'd inverse matrix , getinverse - returns the cache'd inverse matrix
## This function should be called with the raw matrix

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y) {
        x <<- y
        i <<- NULL
      }
      get <- function() x
      setinverse <- function(inv) i <<- inv
      getinverse <- function() i
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## This function calculates and stores the inverse matrix using the list of functions created in makeCacheMatrix

cacheSolve <- function(x, ...) {

      i <- x$getinv()
      if(!is.null(i)) {
        message("getting cached data")
        return(i)
      }
      matrix <- x$get()
      i <- solve(matrix)
      x$setinverse(i)
      ## Return a matrix that is the inverse of 'x'
      i
}
