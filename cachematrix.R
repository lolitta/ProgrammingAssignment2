## Compute matrix inversion and cache it. 
## If the matrix inversion was cached, 
## return "getting cached data" message with the cached values


## makeCacheMatrix() is used to create the inverse of a matrix, it can
## 1. set the value for the matrix inversion computing
## 2. get the value of the matrix inversion compution
## 3. set the value of the inversed matrix
## 4. get the value of the inversed matrix

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) s <<- solve
    getinverse <- function() s
    list(set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve() is used to cache and return the inverse of a Matrix.
## It first checks to see if the cached has existed, 
## If so, it gets the inverse values from the cache and skips the computation.
## Otherwise, it compute the inverse of a matrix and sets the result in the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    s <- x$getinverse()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setinverse(s)
    s
}
