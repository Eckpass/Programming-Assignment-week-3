##This function allows you to create a matrix that can cache its inverse.
## In this function we use set and get to indicate that we want to obtain the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function()i
        list(set=set,
             get=get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This functions should help you to get the inverse of a matrix so you can calculate it faster.


cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if (!is.null(i)) {
                message("cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}



