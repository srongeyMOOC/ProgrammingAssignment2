## Programming Assignment 2
## Create and use a special type of matrix that calculates and caches
## the inverse of the matrix for future use

## Function for creating the special type of matrix
makeCacheMatrix <- function(x = matrix()) {
  
    # Define sub-functions
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    
    get <- function() x
    
    setinverse <- function(inverse) m <<- inverse
    
    getinverse <- function() m
    
    #Return list of sub-functions available
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Function for calculating the inverse of the matrix
## but uses the cached result if available
cacheSolve <- function(x, ...) {

  m <- x$getinverse()
  if(!is.null(m)) {
    #Matrix seen before so can used cached result
    return(m)
  }
  
  #Calculate inverse using solve function
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m  
  
}
