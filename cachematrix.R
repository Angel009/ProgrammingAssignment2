## Put comments here that give an overall description of what your
## functions do

##As described in the assignment, It was necessary to 
##write an R function that is able to cache potentially time-consuming computations.
##Being more specific, this code(both functions) help the user/developer to cache the Inverse of a Matrix

##The implementation is based on the example: "Caching the Mean of a Vector"


## Write a short comment describing this function

##The function 'makeCacheMatrix' creates a matrix using the 
##parameters given by the user, then
##set and get both the value of the matrix and the value of 
##the inverted matrix(as a list format).

makeCacheMatrix <- function(x = matrix()) {
  cache <- NULL
  set <- function(y) {
    x <<- y
    cache <<- NULL
  }
  
  get <- function() x
  set_matrix <- function(inverse) cache <<- inverse
  get_inverse <- function() cache
  
  list(set = set, get = get,
       set_matrix = set_matrix,
       get_inverse = get_inverse)
}


## Write a short comment describing this function

##The function 'cacheSolve' calculates the inverse of 
##the 'matrix' created with 'makeCacheMatrix' it first 
##checks to see if the mean has already been calculated. 
##If so, it gets the mean from the cache and skips the computation. 
##Otherwise, it calculates the inverse and stores it at cache

cacheSolve <- function(x, ...) {
  cache <- x$get_inverse()
  
  if(!is.null(cache)) {
    msg("CACHED DATA")
    return(cache)
  }
  
  data <- x$get()
  cache <- solve(data, ...)
  
  x$set_matrix(cache)
  cache
}
