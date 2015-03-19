## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        matinv <- NULL
        set <- function(y) {
                x <<- y
                matinv <<- NULL
        }
        get <- function() x
        setsolv <- function(mean) matinv <<- solv
        getsolv <- function() matinv
        list(set = set, get = get,
             setsolv = setsolv,
             getsolv = getsolv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        matinv <- x$getsolv()
        if(!is.null(matinv)) {
                message("getting cached data")
                return(matinv)
        }
        data <- x$get()
        matinv <- solv(data, ...)
        x$setsolv(matinv)
        matinv

}
