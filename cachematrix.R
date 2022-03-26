## These two functions essentially create a matrix, compute the inverse of the matrix, and cache these values. 
## If the values for the matrix have previously been calculated, cacheSolve retrieves these values bypassing computation.
## I retrieved the inverse function using the matlib package.

## makeCacheMatrix does 2 things: (1) sets the value of the matrix and gets that value; (2) sets and gets the value of the matrix's inverse.


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve uses the values from makeCacheMatrix to return a matrix that is the inverse of 'x'.
## To save time, the function first verifies if the inverse matrix has previously been calculated.
## If so, it skips the computation and retrieves the cached value.
## If not, it proceeds with the computation via the getinv function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
  
}
