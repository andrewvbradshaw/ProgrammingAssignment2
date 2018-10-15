## Below are two functions caching the inverse of a matrix

## This funtion makes a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function( matrix ) {
    m <<- matrix
    i <<- NULL
  }
  get <- function() {
    m
  }
  setInverse <- function(inverse) {
    i <<- inverse
  }
  getInverse <- function() {
    i
  }
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## This function shows the inverse of the special matrix returned by 
## "makeCacheMatrix". If the inverse is already calculated
## and the matrix is notchanged, "cacheSolve" brings in the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if( !is.null(m) ) {
    message("retrieving cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data) %*% data
  x$setInverse(m)
  m
}
