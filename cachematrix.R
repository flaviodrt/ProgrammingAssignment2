
## This functin create a object that can inverse and cache its matrix.

makeCacheMatrix <- function(x = matrix()) {
    matrix <- NULL
    set <- function(y) {
        x <<- y
        matrix <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) matrix <<- solve
    getsolve <- function() matrix
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## This function calculates the inverse of a matrix
## The function try to get the inverse matrix in cache
## if is not stored in cache, calculate and cache tha matrix

cacheSolve <- function(x, ...) {
    cache <- x$getsolve()
    if(!is.null(cache)) {
        message("getting cached data")
        return(cache)
    }
    data <- x$get()
    cache <- solve(data, ...)
    x$setsolve(cache)
    cache
}
