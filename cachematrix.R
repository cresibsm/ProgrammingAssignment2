## Matrix inversion is usually a costly computation and their may be some benefit to caching the 
## inverse of a matrix rather than compute it repeatedly 
## The following functions cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinvers <- function(invers) i <<- invers
  getinvers <- function() i
  list(set = set, get = get,
       setinvers = setinvers,
       getinvers = getinvers)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinvers()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinvers(i)
  i
  
}




