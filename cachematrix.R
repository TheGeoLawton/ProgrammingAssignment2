## Put comments here that give an overall description of what your
## functions do

## Makes a list of functions that will get and set the matrix and inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){ #Function to set x to a new value and reset inverse
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve #set inverse of matrix
        getinv <- function() inv #gets value of inverse
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Checks whether there is an inv value for the current matrix...
## If there is an valid inv value, it returns that instead of caluclating.
## If not, it calculates it and saves it in the cache

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)){
                message("getting cached inverse")
                return(m)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
