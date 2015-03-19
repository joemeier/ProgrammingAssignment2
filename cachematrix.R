## Caching the Inverse of a square matrix
##  with 2 funtions makeCacheMatrix + cacheSolve


## makeCacheMatrix: 
##  This function creates a special "matrix" object 
##  that can cache its inverse
##  usage:
##     m <- makeCacheMatrix( matrix(c(1,2,14,25), nrow = 2, ncol = 2) );
##  check the result: 
##     summary(m)
##  show matrix:
##     m$get()
 
makeCacheMatrix <- function(x = matrix()) {
        matinv <- NULL
        set <- function(y) {
                x <<- y
                matinv <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) matinv <<- solve
        getsolve <- function() matinv
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## cacheSolve:
##  This function computes the inverse of the special "matrix" 
##  returned by makeCacheMatrix
##  usage:
##     getSolve(m) ## 1st call: result: inverse matrix
##     getSolve(m) ## 2nd call: result: message "getting cached data" + inverse matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        matinv <- x$getsolve()
        if(!is.null(matinv)) {
                message("getting cached data")
                return(matinv)
        }
        data <- x$get()
        matinv <- solve(data, ...)
        x$setsolve(matinv)
        matinv

}
