## Put comments here that give an overall description of what your
## functions do
#
## Write a short comment describing this function
#The function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the inverse of the matrix
# 4. get the inverse of the matrix
#
#
makeCacheMatrix <- function(m = matrix()) {
  i <- NULL
  set <- function(y) {
    m <<- y
    i <<- NULL
  }
  get <- function() m
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}
#
#
## Write a short comment describing this function
#This function computes the inverse of the special "matrix" returned by 
#makeCacheMatrix above. If the inverse has already been calculated (and the matrix
#has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  #m1 <- x$get
  m <- x$getInverse()
  if (!is.null(m)) { #& identical(x, m1) #wondering if additional condition to be added tocheck matrix did not change
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data...)
  x$setInverse(m)
  m
}
#
