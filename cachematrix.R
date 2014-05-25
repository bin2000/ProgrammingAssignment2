## Provide a way to get inverse of matrix
## and cache the result

## return a vector of functions for matrix
## that provides: set, get, setinverse and getinverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
            x <<- y
            inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)    
}


## solve (get the inverse) of the matrix
## get cached result from previous call if possible
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv        
}
