#Two functions for caching the iverse of a matrix and uses
#the cached value if the inverse is already calculated.

## The function "makeCacheMatrics" 
#creates a "matrix" object that can cache its inverse using the "solve" function

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The function "cacheSolve" takes the previously created "x" matrix and checks if the inverse
#is already calculated. If so the cached inversed matrix will be used.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
