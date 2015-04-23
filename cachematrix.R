## Author: Carlos Espino García
##
## cachematrix.R
##
## Uses two functions:  
##  - One to create a cache matrix object that caches the matrix and its inverse.
##  - The other one calculates the inverse of the cache matrix object only once.
##
## Example:
##  random_matrix <- matrix(runif(9), nrow=3)
##  cached_matrix <- makeCacheMatrix(random_matrix)
##  cacheSolve(cached_matrix)
##  cached_matrix$get() %*% cached_matrix$getInverse()  # Check if the inverse was 
##                                                      # computed correctly (the result
##                                                      # must is an identity matrix)
##  cached_matrix $set(another_matrix)                  # Set another cached matrix
##  cached_matrix $get()                                # Returns the cached matrix
##
##  cached_matrix $getInverse()                         # Returns the cached inverse 


## makeCacheMatrix creates a cache matrix object
makeCacheMatrix <- function( m = matrix() ) {
  cached_inverse <- NULL
  set <- function( matrix ) {
    m <<- matrix
    cached_inverse <<- NULL
  }
  get <- function() m
  setInverse <- function(cached_inverse) cached_inverse <<- cached_inverse
  getInverse <- function() cached_inverse
  
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

### cacheSolve returns the inverse of a cache matrix object 
cacheSolve <- function(m, ...) {
  inverse <- m$getInverse()
  if( !is.null(inverse) ) {
    message("getting cached data")
    return(inverse)
  }
  matrix <- m$get()
  inverse <- solve(matrix)
  m$setInverse(inverse)
  inverse
}