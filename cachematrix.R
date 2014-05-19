## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
##
## "cacheSolve" accepts a cache matrix and attempts to locate a cached inverse matrix.
## If cache is found, it is returned directly, otherwise an inverse is calculated
## with the solve function and the result is cached in the cache matrix object and returned.
##
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    # get and return cached inverse matrix of 'x'
    message("getting cached data")
    return(m)
  }
  # get the matrix of the "cache matrix" object 'x'
  data <- x$get()
  # calculate inverse matrix of 'x'
  m <- solve(data, ...)
  # save calculated value for subsequent cache use.
  x$setinv(m)
  # return calculated inverse matrix of 'x'
  m
}



###

if (FALSE) {
## sample code
m1 <- matrix(c(1,0,5,2,1,6,3,4,0),ncol=3)
m2 <- matrix(c(2,2,3,2),ncol=2)

v1 <- makeCacheMatrix(m1)
v2 <- makeCacheMatrix(m2)

cacheSolve(v1)
cacheSolve(v1)

cacheSolve(v2)
cacheSolve(v2)
}