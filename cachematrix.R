# caching the inverse of a matrix

# makeCacheMatrix creates a special "matrix" object containing a function to: 
#     set the value of the matrix
#     get the value of the matrix
#     set the value of the inverse matrix
#     get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    cache = NULL
    set <- function(y) {
        x <<- y
        cache <<- NULL
    }
    get <- function() x
    setinv <- function(solve) cache <<- solve
    getinv <- function() cache
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


# cacheSolve computes the inverse of the special "vector" returned by 
# makeCacheMatrix. However, it first checks to see if the ivnerse has already 
# been computed. If so, it gets the inverse from the cache and skips the 
# computation. Otherwise, it calculates the inverse of the matrix and sets 
# the value of the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
    cache <- x$getinv()
    if(!is.null(cache)) {
        message("getting cached data")
        return(cache)
    }
    data <- x$get()
    cache <- solve(data, ...)
    x$setinv(cache)
    cache
}

