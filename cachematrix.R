## JFR - 05/22/2014 
## The makeCacheMatrix and the cacheSolve functions are a pair of functions that work in tandem to help alleviate
## the computational costs of computing the inverse of a square matrix repeatedly.  The goal of these tandem 
## functions is to create a matrix cache that computes the matrix's inverse once and return the calculated 
## value for any subsequent calls that require the inverse rather than recomputing it each time.

## Usage:
## M <- matrix(c(1, 2, 3, 4), nrow=2, ncol=2)
## cacheMatrix <- makeCacheMatrix(M)
## cacheSolve(cacheMatrix)

## NOTE: These functions do not check to see if a matrix  is inversible, i.e. a square matrix with valid values.

## Write a short comment describing this function
## JFR - 05/22/2014 - makeCacheMatrix
## makeCacheMatrix - Creates a matrix object that is capable of caching its inverse
## Returns - a List of functions that operate on the defined matrix

makeCacheMatrix <- function(x = matrix()) {
    cachedInverse <- NULL
    ## Define setter and getter methods for this matrix object
    set <- function(y) {
      x <<- y
      ## Clear the cached value if it exists
      cachedInverse <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) cachedInverse <<- inverse
    getInverse <- function() cachedInverse
    ## Return a list of function objects
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Write a short comment describing this function
## JFR - 05/22/2014 - makeCacheMatrix
## cacheSolve - Returns the inverse of a makeCacheMatrix object if it has already been calculated or it 
## calculates the inverse and stores it in the makeCahceMatrix object for later use.

cacheSolve <- function(x, ...) {       
  ## Attempt to get the Matrix Inverse from the CacheMatrix object.  Calculate it if it is NULL
  inverseMatrix <- x$getInverse()
  if(!is.null(inverseMatrix)) {
    message("getting cached data")
    return(inverseMatrix)
  }
  ## Need to calculate since inverse was never calculated before
  data <- x$get()
  inverseMatrix <- solve(data, ...)
  ## Store inverse for future use
  x$setInverse(inverseMatrix)
  inverseMatrix
}
