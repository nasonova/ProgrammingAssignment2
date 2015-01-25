## This is the set of function that allow you to cache the inverse matrix
## so this is calculated only once

## This functions is used to create a special object that stores an inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  ## set the inverse matrix to null
  m <- NULL
  ## set function: set the passed value to matrix and clears the inverse matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## get function: returns the valuse of matrix
  get <- function() x
  ## setinverse function: set the inverse matrix
  setinverse <- function(inv) m <<- inv
  ## getinverse function: returns the inverse matrix
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function calculete the inverse matrix for the matrix created
## with above function
cacheSolve <- function(x, ...) {
  ## check if we already calculate the inverse matrix
  inv <- x$getinverse()
  if(!is.null(inv)) {
    ## we already have the answer so retutn it
    message("getting cached data")
    return(inv)
  }
  ## we do not have the answer so calculate it
  data <- x$get()
  ## calculate inverse matrix
  inv <- solve(data)
  ## saving data to cache
  x$setinv(inv)
  ## Return a matrix that is the inverse of 'x'
  inv
}
