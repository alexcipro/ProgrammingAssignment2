## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv0 <- NULL
  set <- function(y) {
    x <<- y
    inv0 <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv0 <<- solve
  getinverse <- function() inv0
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv0 <- x$getinverse()
  if(!is.null(inv0)) {
    message("getting cached data")
    return(inv0)
  }
  data <- x$get()
  inv0 <- solve(data, ...)
  x$setinverse(inv0)
  inv0
}


