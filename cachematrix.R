## Coursera Data Science: Programming in R
## Frederick Cabasa
## November 6, 2016
## Overview: These two functions create a special matrix by which we create
## the inverse matrix and its cache.  The second function checks to see if we had
## calculated the inverse previously.  If not, then we calculate the inverse
## of the passed matrix to the function.


## The first function, makeCacheMatrix creates a special "vector", 
## which is really a list containing a function to
##
##  1. set the value of the matrix
##  2. get the value of the matrix
##  3. set the value of the inverse
##  4. get the value of the inverse

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


## The following function calculates the inverse of the special "matrix" created above.
## It first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the data by using the solve function
## and sets the value of the inverse in the cache via the setinverse function.

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

