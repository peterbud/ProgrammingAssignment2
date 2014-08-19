## Matrix inversion is usually a costly computation and their may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly

## This function creates a special "matrix" object that can cache its inverse. 
makeCacheMatrix <- function(x = matrix()) {
  
  ## Initiate variable
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  
  ## Return the list of functions to be used
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse= getinverse)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then the cachesolve should retrieve the inverse from
## the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'

  ## Try to get the cached inverse
  m <- x$getinverse()
  
  if(!is.null(m)) {
    ## If there is a cached data available, then just return it
    message("getting cached data")
    return(m)
  }
  
  ## If no cached data returned, then we need to compute
  message("computing and storing cached data")
  
  ## Retrieve original matrix to be inverted
  data <- x$get()
  
  ## Invert the matrix
  m <- solve(data)
  
  ## Store he inverted matrix in the cache
  x$setinverse(m)
  
  ## Return the inverse matrix
  m  
}
