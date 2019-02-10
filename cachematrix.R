## This creates a matrix and then uses the cached value
## to solve for the inverse of that matrix

## This function creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL ## m for memory, is set to null for later use
  set <- function(y){
    x <<- y  
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(matrix) m <<- matrix
  getmatrix <- function() m
  list(set = set, get = get, ## naming these values makes it easy to use them in the next function
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}

## This function computes the inverse of the special matrix computed above
cacheSolve <- function(x, ...) {
  m <- x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m) ## if the matrix is already cached, this will return the cached value
  }
  data <- x$get()
  ## The solve function solves the equation a %*% x = b for x, where b can be either a vector or a matrix
  m <- solve(data,...) 
  x$setmatrix(m)
  m
}
