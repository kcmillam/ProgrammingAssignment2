##  this function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {       
  m <- NULL
  set <- function(y) {                      
    x <<- y
    m <<- NULL 
  }
  get <- function() x
  setinverse <- function(solved) m <<- solved  #incorporates "solved" variable from the cachesolve function below
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  solved <- x$getinverse()
  if(!is.null(solved)) {   ## if a value is found for solved, it tells you this in a message
    message("getting cached data")
    return(solved)
  }
  data <- x$get()
  solved <- solve(data, ...)
  x$setinverse(solved)
  solved          
}