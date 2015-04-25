## These functions allow the inverse of a matrix to be calculcated more quickly.
## The makeCacheMatrix function creates a matrix and caches its inverse.
## The cacheSolve function checks if there is an inverse matrix in the cache. If there is, it returns its value. If not, it calculates the inverse and stores it in the cache.

## The makeCacheMatrix function creates a matrix that caches its inverse. 
## It sets the value of the matrix, then gets the value. 
## Following this, the function sets the value of the inverse of the matrix, then gets the value of the inverse of the matrix.

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


## The cacheSolve function calculates the inverse of a matrix. 
## First, it checks the cache to see if the inverse has already been calculated, and if so, returns that value.
## If the inverse has not been calculated, it calculates the inverse and uses the setinverse function to set the inverse in the cache.

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
        ## Return a matrix that is the inverse of 'x'
}
