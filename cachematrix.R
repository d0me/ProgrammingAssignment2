## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## This function creates a special "matrix" object that can cache its inverse
  
  makeCacheMatrix <- function(x = matrix()) { ## define the argument with default mode of "matrix"
    inv <- NULL                             ## put inv as NULL, holds value of inverse of the matrix  
    set <- function(y) {                    ## defines set function to assign  a new 
      x <<- y                             ## value of the matrix in the parent environment
      inv <<- NULL                        ## if there is a new matrix, reset inv to NULL
    }
    get <- function() x                     ## defines the get function - returns the value of the argument of the matrix 
    
    setinverse <- function(inverse) inv <<- inverse  ## assigns the value of inv in the parent environment
    getinverse <- function() inv                     ## gets the value of inv where called
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  ## needed so you can refer
    ## to the functions with the $ operator
  }
  
  
  ## Write a short comment describing this function
  
  cacheSolve <- function(x, ...) {
    ## Returns a matrix that is the inverse of x
    inv <- x$getinverse()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
  }
  
