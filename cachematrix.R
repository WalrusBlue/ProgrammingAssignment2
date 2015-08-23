
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) inverse <<- inv
    getinverse <- function() inverse
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}



cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("Cached Inverse Returned")
        return(inverse)
    }
    m <- x$get()
    inverse <- solve(m, ...)
    x$setinverse(inverse)
    inverse
}
