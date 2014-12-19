## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  invert_x <- NULL
  
  set <- function(y) {
    x <<- y
    invert_x <<- NULL
  }
  
  get <- function() x
  setinverted <- function(inverted) invert_x <<- inverted
  getinverted <- function() invert_x
  
  list(set = set, get = get,setinverted = setinverted,getinverted = getinverted)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverted <- x$getinverted()
  
  if(!is.null(inverted)) {
    message("Getting Cached Data")
    return(inverted)
  }
  norm <- x$get()
  inverted <- solve(norm)
  x$setinverted(inverted)
  
  inverted
}
