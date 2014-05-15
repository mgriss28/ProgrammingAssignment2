## makeCacheMatrix creates a special "matrix" object that can cache 
## the inverse of the matrix.
## This function:
##  	- Sets the value of the Matrix
##		- Gets the value of the Matrix
## 		- Sets the value of the Inverse of the Matrix
##		- Gets the value of the Inverse of the Matrix

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(solve) m <<- solve
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}



## cacheSolve first checks to see if the inverse of the matrix 
## has been calculated.  If so, it will retrieve the inverse from
## cache, and therefore skips the calculation.  If the inverse
## has not been cached, then the function will calculate the 
## inverse of the matrix.

cacheSolve <- function(x, ...) {
  
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  m
}
