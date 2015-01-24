################################################################
## These two functions calculate the inverse of a matrix.
## The first one (makeCacheMatrix) creates the variables and functions,
## and the second one computes the inverse (the first time) and the
## following (after checking that the original matrix to get inverted is the
## same) returns the "already computed matrix".
################################################################

################################################################
## makeCacheMatrix:
## This function creates the matrix and a copy (in variable k) in the parent environment to
## compare in the future, and the functions to access the elements. It returns the functions
## and a copy of the original matrix
################################################################

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  k <<- x
  set <- function(y) {
    k <<- y
    ##        m <<- NULL
  }
  get <- function () x
  doSolve <- function (solve) m <<- solve
  getSolve <- function() m
  getMat <- function() k
  list(set = set, get = get,
       doSolve = doSolve, getSolve = getSolve,
       getMat = getMat,
       mat = x)
}


################################################################
## cacheSolve:
## Checks if there is a previos object and if the object refers to
## the same (original) matrix.
## If that is the case, returns the already cached value.
## If not, computes the inverse with "solve"
###############################################################

cacheSolve <- function(x, ...) {
  m <- x$getSolve()
  if(!is.null(m) & identical(x$mat, x$getMat())) {   ####
    message("Getting cached inverse")
    return(m)
  }
  data <- x$get()
  x$set(data)
  m <- solve(data)
  x$doSolve(m)
  m
}
