## The below functions compute the inverse of a square matrix
## and then cache's its value to save time when it is used again

makeCacheMatrix <- function(x = matrix()) {
  ## This function makes a square matrix, then we call it, then we set the inverse and then we cache it.

  inv = NULL
  set = function(y) {
    ## We use <<- to assign an object from a different environment
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## cache solve calls the cache saving lots of time!

cacheSolve <- function(x, ...) {
  ## This function will return the inverse of the original matrix input to makeCacheMatrix()
  
  inv = x$getinv()
  
  # Now we check if the inverse has been calculated and decide whether or not to get it from the cache
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
  # If it hasn't been calculated we calculate the inverse 
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  # and save it in the cache
  x$setinv(inv)
  
  return(inv)
}
