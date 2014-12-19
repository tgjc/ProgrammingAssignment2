## The two functions below are used together to find the inverse of a matrix x. 
## If the matrix has not changed since the inverse was last calculated, it is 
## returned without the need to recalculate it, thus, saving computing time.

## makeCacheMatrix:
## This function creates a special R object that 
## 1. Initializes a variable 'inv' 
##    (which will be used to save inverse matrix latter, i.e. a cached data);
## 2. Provides function get() to obtain "raw" matrix (of which one needs to find 
##    its inverse);
## 3. Provides function setinv() to assign computed inverse matrix (of x) to m;
## 4. Provides function getinv() to obtain the cached inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(newInv) inv <<- newInv
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## cacheSolve:
## This function does the actual inverting of matrix x.  It first checks if the in-
## verse matrix has been found; if yes, returns the result and quits. If not, the 
## inverse of x is calculated, saved to cached, and returned.
## NOTE: argument x for this function must be cached, i.e. a list returned from
## calling makeCacheMatrix(x).

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}