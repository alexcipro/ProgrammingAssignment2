## Put comments here that give an overall description of what your
## functions do

# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following functions are used to cache the inverse of a matrix.

## Write a short comment describing this function

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

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

# The following function returns the inverse of the matrix and assumes 
# that the matrix is always invertible. Firstly, it checks if the inverse 
# has already been computed. If so, skips the computation and it gets 
# the result and. Otherwise, the function computes the inverse, 
# sets the value in the cache with setinverse function.

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

## Run example
## > m0 <- matrix(c(1:2,11:12), nrow = 2, ncol = 2, byrow = TRUE)
## > m0
## [,1] [,2]
## [1,]    1    2
## [2,]   11   12
## > myMatrix <- makeCacheMatrix(m0)
## > myMatrix$get()
## [,1] [,2]
## [1,]    1    2
## [2,]   11   12
## > cacheSolve(myMatrix)
## [,1] [,2]
## [1,] -1.2  0.2
## [2,]  1.1 -0.1
## > cacheSolve(myMatrix)
## getting cached data
## [,1] [,2]
## [1,] -1.2  0.2
## [2,]  1.1 -0.1

