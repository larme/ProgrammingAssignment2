## A pair of functions that cache the inverse of a matrix.

## A function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    solution <- NULL
    set <- function(y) {
        x <<- y
        solution <<- NULL 
    }

    get <- function () x
    setSolution <- function(s) solution <<- s
    getSolution <- function() solution
    
    list(set=set,
         get=get,
         setSolution=setSolution,
         getSolution=getSolution)
}

## A function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then cacheSolve should retrieve
## the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    solution <- x$getSolution()
    if(!is.null(solution)) {
        message("getting cached solution")
        return(solution)
    }
    m <- x$get()
    solution <- solve(m)
    x$setSolution(solution)
    solution
}
