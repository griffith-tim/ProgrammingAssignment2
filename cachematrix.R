## This pair of functions computes the inverse of an invertable matrix
## and caches that inverse.  When the inverse function is called again, these
## functions check to determine if the inverse has already been calculated.

## This first function takes a matrix and cahces it and its inverse.
## It returns a list of functions that can get a matrix ($get), 
## get its inverse ($getinverse), and set the inverse ($setinverse). 

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y) {
            x <<- y
            i <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) i <<- inverse
      getinverse <- function() i
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## This function calculates the inverse of a matrix if it is not alreafy known.
## It takes the list from the makeCacheMatrix function and returns the inverse 
## of the matrix cached in this function.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      i <- x$getinverse()
      if(!is.null(i)) {
            message("getting cached data")
            return(i)
      }
      data <- x$get()
      i <- solve(data, ...)
      x$setinverse(i)
      i
}
