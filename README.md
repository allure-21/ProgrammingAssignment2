# ProgrammingAssignment2
Programming Assignment week 2 R Programming

## The makeCacheMatrix function creates a special array that caches its inverse with cacheSolve. 
## This function calculates the inverse of the special matrix returned by makeCacheMatrix. 
## If the inverse has been calculated and the matrix still the same, the cache retrieves the inverse of the cache.
## For this assignment, the supplied matrix can always be reversed.

## The following functions are used to create a special object that stores an array and caches its inverse. 
## The first function, makeCacheMatrix, makes a special array, which is a list that contains a function for:
    ## set the value of the matrix
    ## get the value of the matrix
    ## set the value of the inverse
    ## get the value of the inverse

## The firt functions creates a special matrix.

makeCacheMatrix <- function(x = matrix()) {

  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## With this function we can calculate the inverse of a special matrix that is returned with previous makeCacheMatrix. 
##In case the inverse has been calculated and the matrix still the same, the cacheSolve function would recover to the inverse of the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
