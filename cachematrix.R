## makeCacheMatrix and cacheSolve together cache the inverse of a matrix

## makeCacheMatrix contructs a "matrix" object (a list) containing 
## functions to set and get the value of the matrix,
## and to set and get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, 
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve returns the matrix inverse of 'x', either by
## loading the inverse from the cache or by calculating it (and
## subsequently setting it in the cache)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
