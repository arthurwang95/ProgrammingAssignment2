## Cache the inverse of a matrix

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    
    inver <- NULL
    set <- function(y) {
        x <<- y
        inver <<- NULL
    }
    get <- function() x
    set_inver <- function(inversion) inver <<- inversion
    get_inver <- function() inver
    list(set = set, get = get,
         set_inver = set_inver,
         get_inver = get_inver)
    
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inver_1 <- x$get_inver()
    if(!is.null(inver_1 )) {
        message("getting cached data")
        return(inver_1)
    }
    data <- x$get()
    inver_1 <- solve(data, ...)
    x$set_inver (inver_1 )
    inver_1 
}