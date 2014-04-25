## Following pair of functions are used to create matrix, calculate its inverse
## and store it in cache. If necessary matrice inverse can be obtained from cahe
## instead of calculating it again.

## The first function, makeCacheMatrix creates a vector, 
## which is a list containing following functions:
## "set" - set the matrix
## "get" - get the matrix
## "setInverse" - set the the inverse of matrix
## "getInverse" - get the the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) m <<- solve
    getInverse <- function() m
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## The following function calculates the inverse of the matrix created with 
## the above function. However, it first checks to see if the inverse has 
## already been calculated. If so, it gets the inverse from the cache and 
## skips the computation. Otherwise, it calculates the inverse of the matrix 
## and sets the value of the inverse in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m 
}
