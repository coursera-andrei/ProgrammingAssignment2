## This source code contains a pair of functions that cache the inverse of a matrix.
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve
## should retrieve the inverse from the cache.


## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## Function is used in pair with cacheSolve function to optimize the number 
## of time-consuming computations for solve() function when the content of matrix
## is not changing.
## makeCacheMatrix takes an object of a matrix class as an input. A matrix needs to
## be a square matrix.The output is a list object or a message if the type of matrix 
## is not correct.

makeCacheMatrix <- function(x = matrix()) {
  if (nrow(x) == ncol(x)){
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
  }
    get <- function() x
    setinv <- function(inv) inv <<- inv
    getinv <- function() inv
    list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)}
  
  else {print("Matrix is not square. Please, assign a square matrix.")}
}


## cacheSolve takes an x object as an input and returns a matrix 
## that is the inverse of 'x'. cacheSolve function is used in pair with makeCacheMatrix
## function.

cacheSolve <- function(x, ...) {

  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
