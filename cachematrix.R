## These functions define a matrix object in which a matrix
## and its inverse are stored within. The inverse returns 
## NULL until it is computed, and once computed recalculating
## is avoided by having results cached.

## The matrix cache object in which we store the matrix
## and its inverse, once it is calculated.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get<- function() x
  setinv <- function(invcalc) inv <<- invcalc
  getinv <- function() inv
  list(set = set, get = get, 
       setinv = setinv,
       getinv = getinv)
}


## This function solves the matrix inverse and caches
## the result to prevent the need to recalculate 
## later for any previously calculated matrix inverse.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- lapply(data, solve, ...)
  x$setinv(inv)
  
}
