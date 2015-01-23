makeCacheMatrix <- function(x = matrix()){
    m <- NULL
    set <- function(y) {
        k <<- y
        ##    m <<- NULL
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


cacheSolve <- function (x, ...) {
    m <- x$getSolve()
    if(!is.null(m) & identical(x$mat, x$getMat())) {   ####
        message("Getting cached inverse")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$doSolve(m)
    x$set(data)
    m
}
