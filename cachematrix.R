## Put comments here that give an overall description of what your
## functions do
##
## 'cachematrix' provides a method of calculating an inverse of a matrix,
## caching results for each unique matrix, preventing repeated calculations.
## It is assumed that all matrices being cached are invertible.
## There are two function: 'makeCacheMatrix' and 'cacheSolve'.
## 'makeCacheMatrix' intitalizes a 'cache matrix' object given a matrix.
## 'cacheSolve' checks the cache for an existing inverse of a 'cache matrix',
## returns it, or calculates the inverse, caching it for future queries
## before retruning it.
##

## Write a short comment describing this function
##
## "makeCacheMatrix" takes a matrix object and initializes a 'cache matrix'
## object. A 'cache matrix' object consists of four functions: set, get,
## setinv and getinv. The set function stores the original matrix and 
## initializes the inverse as NULL. The get function returns the original
## matrix, setinv calculates and saves the invese of matrix 'x' (the cached
## result), and 'getinv' returns the inverse matrix.
##
makeCacheMatrix <- function(x = matrix()) {
  # initialize inverse matrix as NULL
  m <- NULL
  # set the original matrix value and initial inverse of the 'cache matrix'
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  # define function to return original matrix
  get <- function() x
  # define function to calculate inverse matrix
  setinv <- function(solve) m <<- solve
  # define function to return inverse matrix
  getinv <- function() m
  # return list of functions
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
##
## "cacheSolve" accepts a cache matrix and attempts to locate a cached inverse
## matrix. If cache is found, it is returned directly, otherwise an inverse is
## calculated with the solve function and the result is cached in the cache
## matrix object and returned.
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


##
## Anything within the curly brackts below will not be executed.
##
if (FALSE) {
## sample code
source("cachematrix.R")
# create two inversible matrices
m1 <- matrix(c(1,0,5,2,1,6,3,4,0),ncol=3)
m2 <- matrix(c(2,2,3,2),ncol=2)
# generate 'cache matrix' objects for each
v1 <- makeCacheMatrix(m1)
v2 <- makeCacheMatrix(m2)
# test matrix 1 inverse without and with cache
cacheSolve(v1)
cacheSolve(v1)
# test matrix 2 inverse without and with cache
cacheSolve(v2)
cacheSolve(v2)
}