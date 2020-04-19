## Taking the inverse of a matrix may be time consuming, especially
## if it needs to be done repeatedly, such as in a loop.
## The functions makeCacheMatrix and cacheSolve will allow you to
## cache the inverse of a matrix so that it can be looked up in the
## cache rather than recomputed.

## makeCacheMatrix creates a list containing a function to:
## 1. set the matrix
## 2. get the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(solve) i <<- solve
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve will calculate the inverse of the matrix created in makeCacheMatrix
## However, it will first check to see if the inverse has already been calculated
## and if so, it will get the inverse from the cache and skip the computation.
## If the inverse has not been calculated, it will calculate using the setinv function.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
    }
