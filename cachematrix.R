## These functions are used to save the matrix and inverse matrix and use the saved matrix.

## This function saves and returns the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL #variable to hold inverse
        set <- function(y) {
                x <<- y ## save the matrix for later use
                i <<- NULL ## set inverse matrix to null
        }
        get <- function() x ##typing x$get will print the matrix
        setinverse <- function(inv) i <<- inv ##save the inverse matrix for later use.
        getinverse <- function() i ##typing x$getinverse will print the matrix inverse
        list(set = set, get = get,
               setinverse = setinverse,
               getinverse = getinverse)
}


## This function uses the cached value of the inverse if already present or sets the inverse to cache for later use.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
