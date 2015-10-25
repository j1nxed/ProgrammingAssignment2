## Function will cache the invers of a matrix object and store it,
## than checks if it already exists and takes it from cache. 

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set = function(y) {
    x <<- y          ##assigning value to an object
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

cacheSolve <- function(x, ...) {     # x will be result of makecachematrix()
  inv = x$getinv()
  if (!is.null(inv)){  ##checkes if inverse exsist
    message("getting cached data")    
    return(inv)        ## if so gets it from cache
  }
  # otherwise, calculates the inverse 
  mat.data = x$get()
  inv = solve(mat.data, ...)
  x$setinv(inv)
  return(inv)
}