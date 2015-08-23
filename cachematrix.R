##This function creates a special matrix object that caches the inverse of the matrix
##The function returns a list containing the set and get values of the matrix and the inverse set and get values of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    ##x is a square invertible matrix

    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
        ## <<- to assign outside the current enviroment
    }
    get <- function() x
    setinverse <- function(inv) inverse <<- inv
    getinverse <- function() inverse
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
    ##return the list of matrix and inverse data which is usable by the cacheSolbe function
}


##This function returns the inverse of an input matrix
##It is assumed that the input matrix is invertible.
##If the inverse of the input matrix is already stored in memory it returns the cached data.
##Otherwise, the inverse is calculated and cached.
cacheSolve <- function(x, ...) {
    ##x is ideally the output of makeCacheMatrix()
    inverse <- x$getinverse()
    ##check if inverse was previously calculated and return cached inverse if it exists
    if(!is.null(inverse)) {
        message("Cached Inverse Returned")
        return(inverse)
    }
    ##If the inverse was not previously calculated, we calculate teh inverse
    m <- x$get()
    inverse <- solve(m, ...)
    
    ##Cache inverse and return inverse
    x$setinverse(inverse)
    inverse
}
