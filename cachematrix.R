## Put comments here that give an overall description of what your
## functions do

## These functions create a matrixobject and compute the inverse. The inverse will be cached in the matrixobject, 
## so that if the function cacheSolved is repeated it will get the cached object

## Write a short comment describing this function
## setM resets Matrixobject
## getM returns x
## setSolve sets inverseMatrix 
## getSolve gets inverseMatrix

makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  setM <- function(y) {
    x <<- y
    inverseMatrix <<- NULL
  }
  getM <- function() x
  setSolve <- function(solve) inverseMatrix <<- solve
  getSolve <- function() inverseMatrix
  list(setM = setM, getM = getM,
       setSolve = setSolve,
       getSolve = getSolve)
  
}


## Write a short comment describing this function

## if an inverseMatrix is found in the cache, it will be used, else the inverse Matrix will be computed.

cacheSolve <- function(x, ...) {
  inverseMatrix <- x$getSolve()
  if(!is.null(inverseMatrix)) {
    message("getting cached data")
    return(inverseMatrix)
  }
  data <- x$getM()
  inverseMatrix <- solve(data, ...)
  x$setSolve(inverseMatrix)
  inverseMatrix
        ## Return a matrix that is the inverse of 'x'
}
